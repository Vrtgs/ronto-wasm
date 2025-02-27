use crate::read_tape::ReadTape;
use std::fmt::{Debug, Display, Formatter};
use std::io::{Read, Result};
use std::marker::PhantomData;

fn vector_from_vec<T>(vec: Vec<T>) -> Result<WasmVec<T>> {
    WasmVec::try_from(vec.into_boxed_slice()).map_err(invalid_data)
}

macro_rules! expect {
    ($condition:expr, $($errmsg:tt)*) => {
        match $condition {
            true => Ok(()),
            false => Err(invalid_data(format!($($errmsg)*))),
        }
    }
}

pub(crate) trait Decode: Sized {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self>;
}

impl Decode for bool {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        match u8::decode(file)? {
            0 => Ok(false),
            1 => Ok(true),
            _ => Err(invalid_data("boolean should only be 0 or 1")),
        }
    }
}

macro_rules! impl_decode_for_int {
    ($($ints:ident)*) => {
        $(
            impl Decode for $ints {
                fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
                    file.read_leb128()
                }
            }
        )*
    }
}

macro_rules! impl_decode_naive {
    ($($types:ty)*) => {
        $(
        impl Decode for $types {
            fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
                file.read_chunk().map(<$types>::from_le_bytes)
            }
        }
        )*
    }
}

impl Decode for u8 {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        file.read_byte()
    }
}

impl Decode for i8 {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        u8::decode(file).map(|byte| byte as i8)
    }
}

impl_decode_for_int! {
    i16 i32 i64
    u16 u32 u64
}

impl_decode_naive! {
    f32 f64
    // u128 is encoded as just naive bytes
    i128 u128
}

impl<T: Decode> Decode for PhantomData<T> {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        let _ = T::decode(file)?;
        Ok(PhantomData)
    }
}

trait Enum: Sized {
    type Discriminant: Decode + Debug + Display + Copy + Clone + 'static;
    const VARIANTS: &'static [Self::Discriminant];

    fn discriminant(&self) -> Self::Discriminant;

    fn enum_try_decode(
        variant: Self::Discriminant,
        file: &mut ReadTape<impl Read>,
    ) -> Option<Result<Self>>;
}

impl<E: Enum> Decode for E {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        let variant = E::Discriminant::decode(file)?;
        Self::enum_try_decode(variant, file).ok_or_else(|| {
            invalid_data(format!(
                "invalid {name}: {variant}, expected one of: {expected:?}",
                name = std::any::type_name::<Self>(),
                expected = E::VARIANTS
            ))
        })?
    }
}

macro_rules! discard {
    ($type: ty) => {
        _
    };
}

macro_rules! filter_discard {
    ($([$ident: ident])*) => { Self { $($ident),* } };
    ([_] $($tt:tt)*) => { filter_discard!($($tt)*) };
    ([$ident:ident] $($tt:tt)*) => { filter_discard!($($tt)* [$ident]) };
}

macro_rules! normalize_struct {
    ($(#[$($meta:tt)*])* $outer_vis:vis struct $name:ident { $($vis:vis $field:ident : $type:ty),* }) => {
        $(#[$($meta)*])*
        $outer_vis struct $name {
            $($vis $field : $type),*
        }
    };
    ($(#[$($meta:tt)*])* $outer_vis:vis struct $name:ident { $vis:vis _ : $type:ty $(, $($next:tt)*)? }) => {
        normalize_struct! {
            $(#[$($meta)*])*
            $outer_vis struct $name {
                $($($next)*)?
            }
        }
    };

    ($(#[$($meta:tt)*])* $outer_vis:vis struct $name:ident { $vis:vis $ident:ident : $type:ty $(, $($next:tt)*)? }) => {
        normalize_struct! {
            $(#[$($meta)*])*
            $outer_vis struct $name {
                $($($next)*)?
                $vis $ident: $type
            }
        }
    };
}

macro_rules! decodable {
    {} => {};
    {$(#[$($meta:tt)*])* union $name:ident = $first_ty:ident $(| $rest:ident)*; $($next:tt)*} => {
        $(#[$($meta)*])*
        pub enum $name {
            $first_ty($first_ty)
            $(, $rest($rest))*
        }

        impl Enum for $name {
            type Discriminant = <$first_ty as Enum>::Discriminant;
            #[doc(hidden)]
            const VARIANTS: &[Self::Discriminant] = &{
                let mut union_discriminants = [0; {<$first_ty as Enum>::VARIANTS.len() $(+ <$rest as Enum>::VARIANTS.len())*}];
                let mut i = 0;

                let arrs = [<$first_ty as Enum>::VARIANTS $(, <$rest as Enum>::VARIANTS)*];

                let mut arrs_cursor: &[_] = &arrs;
                while let [next, rest @ ..] = arrs_cursor {
                    arrs_cursor = rest;

                    let mut arr_cursor = *next;
                    while let [discriminant, rest @ ..] = arr_cursor {
                        arr_cursor = rest;
                        union_discriminants[i] = *discriminant;
                        i += 1;
                    }
                }

                assert!(i == union_discriminants.len(), "couldn't fill discriminant array");

                let mut i = 0;
                while i < union_discriminants.len() {
                    let mut j = i+1;
                    while j < union_discriminants.len() {
                        if union_discriminants[i] == union_discriminants[j] {
                            panic!(concat!("enums [", stringify!($first_ty) $(, ", ", stringify!($rest))*, "] aren't disjoint"))
                        }
                        j += 1;
                    }
                    i += 1;
                }


                union_discriminants
            };

            #[inline(always)]
            fn discriminant(&self) -> Self::Discriminant {
                match self {
                    Self::$first_ty(x) => <$first_ty as Enum>::discriminant(x),
                    $(Self::$rest(x) => <$rest as Enum>::discriminant(x)),*
                }
            }

            #[doc(hidden)]
            #[inline(always)]
            fn enum_try_decode(variant: Self::Discriminant, file: &mut ReadTape<impl Read>) -> Option<Result<Self>> {
                <$first_ty as Enum>::enum_try_decode(variant, file).map(|res| res.map(Self::$first_ty))
                    $(.or_else(|| <$rest as Enum>::enum_try_decode(variant, file).map(|res| res.map(Self::$rest))))*
            }
        }

        impl From<$first_ty> for $name {
            fn from(value: $first_ty) -> Self {
                Self::$first_ty(value)
            }
        }

        $(impl From<$rest> for $name {
            fn from(value: $rest) -> Self {
                Self::$rest(value)
            }
        })*

        decodable! { $($next)* }
    };
    {$(#[$($meta:tt)*])* enum $name:ident: $type: ty { $($variant:ident $(($($inner:ty),*))? $({ $($ident:ident: $named_ty:ty),* })? = $value:expr),+ $(,)? } $($next:tt)*} => {
        $(#[$($meta)*])*
        pub enum $name {
            $($variant $(($($inner,)*))? $({ $($ident: $named_ty),* })?),*
        }

        impl Enum for $name {
            type Discriminant = $type;
            #[doc(hidden)]
            const VARIANTS: &[$type] = &[$($value),*];

            fn discriminant(&self) -> Self::Discriminant {
                match *self {
                    $(Self::$variant $(($(discard!($inner),)*))? $({ $($ident: _),* })? => $value),*
                }
            }

            #[inline(always)]
            fn enum_try_decode(variant: $type, _file: &mut ReadTape<impl Read>) -> Option<Result<Self>> {
                match variant {
                    $(
                    $value => {
                        Some(Ok($name::$variant
                            $((
                                $(
                                {
                                    match <$inner>::decode(_file) {
                                        Ok(x) => x,
                                        Err(err) => return Some(Err(err))
                                    }
                                }
                                ),*
                            ))?
                            $({
                                $(
                                $ident: {
                                    match <$named_ty>::decode(_file) {
                                        Ok(x) => x,
                                        Err(err) => return Some(Err(err))
                                    }
                                }
                                ),*
                            })?
                        ))
                    },
                    )*
                    _ => None,
                }
            }
        }

        decodable! { $($next)* }
    };
    {$(#[$($meta:tt)*])* struct $name:ident { $($field:tt : $type:ty),+ $(,)? } $($next:tt)*} => {
        normalize_struct!{
            $(#[$($meta)*])*
            pub struct $name {
                $(
                pub $field: $type,
                )*
            }
        }
        impl Decode for $name {
            fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
                $(
                let $field = <$type>::decode(file)?;
                )*

                Ok(filter_discard!($([$field])*))
            }
        }
        decodable! { $($next)* }
    };
    {$(#[$($meta:tt)*])* struct $name:ident ($($type:ty),* $(,)?); $($next:tt)*} => {
        $(#[$($meta)*])*
        pub struct $name (
            $(
            pub $type,
            )*
        );
        impl Decode for $name {
            fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
                Ok(Self(
                    $(
                    <$type>::decode(file)?,
                    )*
                ))
            }
        }
        decodable! { $($next)* }
    };
}

#[expect(dead_code)]
pub struct CustomSection(pub WasmVec<u8>);

impl Debug for CustomSection {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CustomSection").finish_non_exhaustive()
    }
}

impl Decode for CustomSection {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        let length = Index::decode(file)?;
        let bx = file.read(length.as_usize())?;
        Ok(CustomSection(WasmVec::from_trusted_box(bx)))
    }
}

decodable! {
    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    enum RefrenceType: u8 {
        FunctionRef = 0x70,
        ExternRef = 0x6F,
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    enum NumericType: u8 {
        I32  = 0x7F,
        I64  = 0x7E,
        F32  = 0x7D,
        F64  = 0x7C,
        V128 = 0x7B,
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    union ValueType = NumericType | RefrenceType;

    #[derive(Debug)]
    enum Section: u8 {
        Custom(CustomSection) = 0x00,
        Type(WithLength<TypeSection>) = 0x01,
        Import(WithLength<ImportSection>) = 0x02,
        Function(WithLength<FunctionSection>) = 0x03,
        Table(WithLength<TableSection>) = 0x04,
        Memory(WithLength<MemorySection>) = 0x05,
        Global(WithLength<GlobalSection>) = 0x06,
        Export(WithLength<ExportSection>) = 0x07,
        Start(WithLength<StartSection>) = 0x08,
        Element(WithLength<ElementSection>) = 0x09,
        Code(WithLength<CodeSection>) = 0x0a,
        Data(WithLength<DataSection>) = 0x0b,
        DataCount(WithLength<DataCountSection>) = 0x0c,
    }
}

#[derive(Debug)]
struct WithLength<T>(T);

impl<T: Decode> Decode for WithLength<T> {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        let length = Index::decode(file)?.as_usize();
        let buffer = file.read(length)?.into_vec();
        let tape = ReadTape::memory_buffer(buffer);
    }
}

type Length = PhantomData<Index>;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct TagByte<const TAG: u8>;

impl<const TAG: u8> Decode for TagByte<TAG> {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        expect!(file.read_byte()? == TAG, "expected next byte to be {}", TAG).map(|()| TagByte)
    }
}

decodable! {
    #[derive(Debug)]
    struct TypeInfo {
        _: TagByte<0x60>,
        parameters: WasmVec<ValueType>,
        result: WasmVec<ValueType>,
    }

    #[derive(Debug)]
    struct TypeSection {
        functions: WasmVec<TypeInfo>,
    }
}

impl Decode for String {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        let len = Index::decode(file)?;
        String::from_utf8(file.read(len.as_usize())?.into()).map_err(invalid_data)
    }
}

decodable! {
    #[derive(Debug)]
    struct Import {
        module: String,
        name: String,
        description: InterfaceDescription,
    }

    #[derive(Debug)]
    enum InterfaceDescription: u8 {
        Function(FunctionIndex) = 0x00,
        Table(TableIndex) = 0x01,
        Memory(MemoryIndex) = 0x02,
        Global(GlobalIndex) = 0x03,
    }

    #[derive(Debug)]
    struct ImportSection {
        imports: WasmVec<Import>,
    }

    #[derive(Debug)]
    struct Export {
        name: String,
        description: InterfaceDescription,
    }

    #[derive(Debug)]
    struct ExportSection {
        exports: WasmVec<Export>,
    }

    #[derive(Debug)]
    enum ElementKind: u8 {
        FunctionReference = 0x00,
    }

    #[derive(Debug)]
    struct ElementSection {
        elements: WasmVec<Element>,
    }

    #[derive(Debug)]
    struct CodeSection {
        definitions: WasmVec<FunctionDefinition>,
    }

    #[derive(Debug)]
    struct StartSection(FunctionIndex);
}

#[derive(Debug)]
#[expect(dead_code)]
pub struct Element {
    pub kind: ElementKind,
    pub init: WasmVec<Expression>,
    pub mode: ElementMode,
}

#[derive(Debug)]
#[expect(dead_code)]
pub enum ElementMode {
    Active { index: Index, offset: Expression },
    Passive,
    Declarative,
}

impl Decode for Element {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        let flags = file.read_byte()?;
        let mode = match flags {
            0x00 | 0x02 | 0x04 | 0x06 => {
                let table = match flags {
                    0x00 | 0x04 => TableIndex::ZERO,
                    0x02 | 0x06 => TableIndex::decode(file)?,
                    _ => unreachable!(),
                };
                let offset = Expression::decode(file)?;
                ElementMode::Active {
                    index: table.0,
                    offset,
                }
            }
            0x01 | 0x05 => ElementMode::Passive,
            0x03 | 0x07 => ElementMode::Declarative,
            _ => return Err(invalid_data("invalid element flags")),
        };
        let kind = match flags {
            0x00 | 0x04 => ElementKind::FunctionReference,
            _ => ElementKind::decode(file)?,
        };
        let init = match flags {
            0x00 | 0x01 | 0x02 | 0x03 => {
                WasmVec::<FunctionIndex>::decode(file)?.map(Expression::function_call)
            }
            _ => WasmVec::<Expression>::decode(file)?,
        };
        let result = Element { kind, init, mode };
        Ok(result)
    }
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub locals: WasmVec<ValueType>,
    pub body: Expression,
}

impl<T: Decode, U: Decode> Decode for (T, U) {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        Ok((T::decode(file)?, U::decode(file)?))
    }
}

impl Decode for FunctionDefinition {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        let _ = u32::decode(file)?;
        let count = Index::decode(file)?;
        let mut locals = vec![];
        for _ in 0..count.0 {
            let (run_length, value_type) = <(Index, ValueType)>::decode(file)?;
            locals.extend(std::iter::repeat_n(value_type, run_length.as_usize()));
        }
        let locals = vector_from_vec(locals)?;
        let body = Expression::decode(file)?;
        Ok(FunctionDefinition { locals, body })
    }
}

macro_rules! index_ty {
    ($($ty: ident)+) => {
        paste::paste! {
            decodable! {$(
                #[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, bytemuck::Pod, bytemuck::Zeroable)]
                #[repr(transparent)]
                struct [<$ty Index>](Index);
            )+}

            $(
                impl [<$ty Index>] {
                    pub const ZERO: Self = Self(Index::ZERO);
                    pub const MAX: Self  = Self(Index::MAX);
                }
            )+
        }
    };
}

index_ty! {
    Type
    Function
    Table
    Memory
    Global
    Element
    Data
    Local
    Label

    Extern
}

impl FunctionIndex {
    pub const NULL: Self = Self::MAX;
}

impl ExternIndex {
    pub const NULL: Self = Self::MAX;
}

type MemoryType = Limit;

decodable! {
    #[derive(Debug)]
    struct FunctionSection {
        signatures: WasmVec<TypeIndex>,
    }
}

pub struct Data {
    pub init: WasmVec<u8>,
    pub mode: ElementMode,
}

impl Decode for Data {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        let mode = match u32::decode(file)? {
            0 => ElementMode::Active {
                index: MemoryIndex::ZERO.0,
                offset: Expression::decode(file)?,
            },
            1 => ElementMode::Passive,
            2 => ElementMode::Active {
                index: MemoryIndex::decode(file)?.0,
                offset: Expression::decode(file)?,
            },
            variant => return Err(invalid_data(format!("invalid data variant: {variant}"))),
        };
        let data_len = Index::decode(file)?;
        let bytes = file
            .read(data_len.as_usize())
            .map(WasmVec::from_trusted_box)?;

        Ok(Data { init: bytes, mode })
    }
}

impl Debug for Data {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        struct BytesPreview<'a>(&'a [u8]);
        impl Debug for BytesPreview<'_> {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                let mut debug_list = f.debug_list();
                match self.0.len() {
                    len @ ..=4 => debug_list.entries(&self.0[..len]).finish(),
                    _ => debug_list.entries(&self.0[..4]).finish_non_exhaustive(),
                }
            }
        }

        f.debug_struct("Data")
            .field("init", &BytesPreview(&self.init))
            .field("mode", &self.mode)
            .finish_non_exhaustive()
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Limit {
    pub min: Index,
    pub max: Index,
}

impl Decode for Limit {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        let bounded = bool::decode(file)?;
        let min = Index::decode(file)?;
        let max = match bounded {
            true => Index::decode(file)?,
            false => Index::MAX,
        };
        if max < min {
            return Err(invalid_data("degenerate limit encountered; max < min"));
        }
        Ok(Limit { min, max })
    }
}

decodable! {
    #[derive(Debug)]
    struct TableValue {
        element_type: ValueType,
        limits: Limit,
    }

    #[derive(Debug)]
    struct TableSection {
        tables: WasmVec<TableValue>,
    }

    #[derive(Debug)]
    struct MemorySection {
        memories: WasmVec<Limit>,
    }

    #[derive(Debug)]
    struct GlobalType {
        value_type: ValueType,
        mutable: bool,
    }

    #[derive(Debug)]
    struct Global {
        r#type: GlobalType,
        expression: Expression,
    }

    #[derive(Debug)]
    struct GlobalSection {
        globals: WasmVec<Global>,
    }

    #[derive(Debug)]
    struct DataSection {
        data: WasmVec<Data>,
    }

    #[derive(Debug)]
    struct DataCountSection(u32);

    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    struct MemoryArgument {
        align: Index,
        offset: Index,
    }
}

pub use crate::instruction::Instruction;
use crate::invalid_data;
use crate::vector::{Index, WasmVec};

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum BlockType {
    Empty,
    Type(ValueType),
    TypeIndex(TypeIndex),
}

impl Decode for BlockType {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        let byte = file.peek_byte()?;
        if byte == 0x40 {
            file.read_byte()?;
            Ok(BlockType::Empty)
        } else if byte > 0x40 && byte < 0x80 {
            Ok(BlockType::Type(ValueType::decode(file)?))
        } else {
            Ok(BlockType::TypeIndex(TypeIndex::decode(file)?))
        }
    }
}

#[derive(Debug, PartialEq)]
#[expect(dead_code)]
enum IfElseBlock {
    If(Expression),
    IfElse(Expression, Expression),
}

const INSTRUCTION_ELSE: u8 = 0x05;
const INSTRUCTION_END: u8 = 0x0B;

impl Decode for IfElseBlock {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        let mut ifso = vec![];

        while !matches!(file.peek_byte()?, INSTRUCTION_ELSE | INSTRUCTION_END) {
            ifso.push(Instruction::decode(file)?);
        }
        let byte = file.read_byte()?;

        let ifso = Expression {
            instructions: vector_from_vec(ifso)?,
        };
        match byte {
            INSTRUCTION_ELSE => {
                let ifnot = Expression::decode(file)?;
                Ok(IfElseBlock::IfElse(ifso, ifnot))
            }
            INSTRUCTION_END => Ok(IfElseBlock::If(ifso)),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expression {
    pub instructions: WasmVec<Instruction>,
}

impl Expression {
    fn function_call(idx: FunctionIndex) -> Self {
        Self {
            instructions: WasmVec::from_trusted_box(Box::new([Instruction::RefFunc(idx)])),
        }
    }
}

impl Decode for Expression {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        let mut instructions = vec![];

        while file.peek_byte()? != INSTRUCTION_END {
            instructions.push(Instruction::decode(file)?);
        }
        let _ = file.read_byte();

        Ok(Expression {
            instructions: vector_from_vec(instructions)?,
        })
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub enum WasmVersion {
    Version1,
}

impl Decode for WasmVersion {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        match u32::from_le_bytes(file.read_chunk()?) {
            1 => Ok(WasmVersion::Version1),
            _ => Err(invalid_data("unsupported WASM version")),
        }
    }
}

#[derive(Debug)]
pub struct WasmSections {
    pub custom: WasmVec<CustomSection>,
    pub r#type: Option<TypeSection>,
    pub import: Option<ImportSection>,
    pub function: Option<FunctionSection>,
    pub table: Option<TableSection>,
    pub memory: Option<MemorySection>,
    pub global: Option<GlobalSection>,
    pub export: Option<ExportSection>,
    pub element: Option<ElementSection>,
    pub code: Option<CodeSection>,
    pub data: Option<DataSection>,
    pub start: Option<FunctionIndex>,
}

#[derive(Debug)]
pub struct WasmBinary {
    pub version: WasmVersion,
    pub sections: WasmSections,
}

impl Decode for WasmBinary {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        expect!(file.read_chunk()? == *b"\0asm", "invalid data")?;
        let version = WasmVersion::decode(file)?;
        let mut custom_sections = vec![];
        let mut sections = WasmSections {
            custom: WasmVec::new(),
            r#type: None,
            import: None,
            function: None,
            table: None,
            memory: None,
            global: None,
            export: None,
            element: None,
            code: None,
            data: None,
            start: None,
        };

        macro_rules! insert_section {
            ($opt: ident, $expr: expr) => {
                match sections.$opt {
                    Some(_) => {
                        return Err(invalid_data(format!(
                            "duplicate {} section",
                            stringify!($opt)
                        )))
                    }
                    None => sections.$opt = Some($expr),
                }
            };
        }

        while file.has_data()? {
            match Section::decode(file)? {
                Section::Custom(section) => custom_sections.push(section),
                Section::Type(type_shit) => insert_section!(r#type, type_shit.0),
                Section::Import(imports) => insert_section!(import, imports.0),
                Section::Function(functions) => insert_section!(function, functions.0),
                Section::Table(tables) => insert_section!(table, tables.0),
                Section::Memory(memories) => insert_section!(memory, memories.0),
                Section::Global(globals) => insert_section!(global, globals.0),
                Section::Export(exports) => insert_section!(export, exports.0),
                Section::Start(start) => insert_section!(start, start.0.0),
                Section::Element(elements) => insert_section!(element, elements.0),
                Section::Code(code) => insert_section!(code, code.0),
                Section::Data(data) => insert_section!(data, data.0),
                Section::DataCount(..) => unreachable!(),
            }
        }

        sections.custom = vector_from_vec(custom_sections)?;

        Ok(WasmBinary { version, sections })
    }
}

pub fn parse_file(file: impl Read) -> Result<WasmBinary> {
    let mut file = ReadTape::new(file);
    WasmBinary::decode(&mut file)
}
