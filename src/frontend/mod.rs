use crate::frontend::vector::{Index, WasmVec};
use crate::read_tape::ReadTape;
use std::error::Error;
use std::fmt::Formatter;
use std::io::{self, Read, Result};
use std::marker::PhantomData;

mod vector;

fn invalid_data(err: impl Into<Box<dyn Error + Send + Sync>>) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, err)
}

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

trait Decode: Sized {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self>;
}

#[derive(Debug)]
struct TodoDecode<const N: usize>;
impl<const N: usize> Decode for TodoDecode<N> {
    fn decode(_: &mut ReadTape<impl Read>) -> Result<Self> {
        todo!("todo decode#{}", N)
    }
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

impl_decode_for_int! {
    i8 i16 i32 i64 i128
    u8 u16 u32 u64 u128
}
impl_decode_naive!(f32 f64);

impl<T: Decode> Decode for PhantomData<T> {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        let _ = T::decode(file)?;
        Ok(PhantomData)
    }
}

macro_rules! decodable {
    {} => {};
    {$(#[$($meta:tt)*])* enum $name:ident: $type: ty { $($variant:ident $(($($inner:ty),*))? = $value:expr),* $(,)? } $($next:tt)*} => {
        #[repr($type)]
        $(#[$($meta)*])*
        pub enum $name {
            $($variant $(($($inner,)*))?= $value),*
        }
        impl Decode for $name {
            fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
                let variant = u8::decode(file)?;
                match variant {
                    $(
                    $value => {
                        Ok($name::$variant$((
                            $(
                            <$inner>::decode(file)?,
                            )*
                        ))?)
                    },
                    )*
                    _ => Err(invalid_data(format!("invalid {}: {}", stringify!($name), variant))),
                }
            }
        }
        decodable! { $($next)* }
    };
    {$(#[$($meta:tt)*])* struct $name:ident { $($field:ident : $type:ty),* $(,)? } $($next:tt)*} => {
        $(#[$($meta)*])*
        pub struct $name {
            $(
            pub $field: $type,
            )*
        }
        impl Decode for $name {
            fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
                $(
                let $field = <$type>::decode(file)?;
                )*
                Ok(Self {
                    $(
                    $field,
                    )*
                })
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

pub struct CustomSection(WasmVec<u8>);

impl std::fmt::Debug for CustomSection {
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
    #[derive(Debug, Clone)]
    enum ValueType: u8 {
        I32 = 0x7F,
        I64 = 0x7E,
        F32 = 0x7D,
        F64 = 0x7C,
        V128 = 0x7B,
        FuncRef = 0x70,
        ExternRef = 0x6F,
    }

    #[derive(Debug)]
    enum Section: u8 {
        Custom(CustomSection) = 0x00,
        Type(Length, TypeSection) = 0x01,
        Import(Length, ImportSection) = 0x02,
        Function(Length, FunctionSection) = 0x03,
        Table(Length, TableSection) = 0x04,
        Memory(Length, MemorySection) = 0x05,
        Global(Length, GlobalSection) = 0x06,
        Export(Length, ExportSection) = 0x07,
        Start(Length, TodoDecode<6>) = 0x08,
        Element(Length, ElementSection) = 0x09,
        Code(Length, CodeSection) = 0x0a,
        Data(Length, TodoDecode<9>) = 0x0b,
        DataCount(Length, TodoDecode<10>) = 0x0c,
    }
}

type Length = PhantomData<Index>;

#[derive(Debug)]
struct TagByte<const TAG: u8>;
impl<const TAG: u8> Decode for TagByte<TAG> {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        expect!(file.read_byte()? == TAG, "expected next byte to be {}", TAG).map(|()| TagByte)
    }
}

decodable! {
    #[derive(Debug)]
    struct FuncType {
        _signature: TagByte<0x60>,
        parameters: WasmVec<ValueType>,
        result: WasmVec<ValueType>,
    }

    #[derive(Debug)]
    struct TypeSection {
        functions: WasmVec<FuncType>,
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
        definitions: WasmVec<Definition>,
    }
}

#[derive(Debug)]
struct Element {
    kind: ElementKind,
    init: WasmVec<Expression>,
    mode: ElementMode,
}

#[derive(Debug)]
enum ElementMode {
    Active {
        table: TableIndex,
        offset: Expression,
    },
    Passive,
    Declarative,
}

impl Decode for Element {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        let flags = file.read_byte()?;
        let mode = match flags {
            0x00 | 0x02 | 0x04 | 0x06 => {
                let table = match flags {
                    0x00 | 0x04 => TableIndex(Index::ZERO),
                    0x02 | 0x06 => TableIndex::decode(file)?,
                    _ => unreachable!(),
                };
                let offset = Expression::decode(file)?;
                ElementMode::Active { table, offset }
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
struct Definition {
    locals: WasmVec<ValueType>,
    body: Expression,
}

impl<T: Decode, U: Decode> Decode for (T, U) {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        Ok((T::decode(file)?, U::decode(file)?))
    }
}

impl Decode for Definition {
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
        Ok(Definition { locals, body })
    }
}

macro_rules! index_ty {
    ($($ty: ident)+) => {
        paste::paste! {
            decodable! {$(
                #[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
                struct [<$ty Index>](Index);
            )+}
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
    Local
    Label
}

type MemoryType = Limit;
type GlobalType = TodoDecode<103>;

decodable! {
    #[derive(Debug)]
    struct FunctionSection {
        signatures: WasmVec<TypeIndex>,
    }
}

decodable! {
    #[derive(Debug)]
    enum Limit: u8 {
        HalfBounded(Index) = 0x00,
        Bounded(Index, Index) = 0x01,
    }

    #[derive(Debug)]
    struct TableType {
        element_type: ValueType,
        limits: Limit,
    }

    #[derive(Debug)]
    struct TableSection {
        tables: WasmVec<TableType>,
    }

    #[derive(Debug)]
    struct MemorySection {
        memories: WasmVec<Limit>,
    }

    #[derive(Debug)]
    struct Global {
        value_type: ValueType,
        is_mutable: bool,
        expression: Expression,
    }

    #[derive(Debug)]
    struct GlobalSection {
        globals: WasmVec<Global>,
    }

    #[derive(Debug)]
    struct MemoryArgument {
        align: u32,
        offset: u32,
    }
}

mod instruction;

use instruction::Instruction;

#[derive(Debug)]
enum BlockType {
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

#[derive(Debug)]
enum IfElseBlock {
    If(Expression),
    IfElse(Expression, Expression),
}

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

const INSTRUCTION_ELSE: u8 = 0x05;
const INSTRUCTION_END: u8 = 0x0B;

#[derive(Debug)]
struct Expression {
    instructions: WasmVec<Instruction>,
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
pub struct Binary(WasmVec<Section>);

impl Decode for Binary {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        expect!(file.read_chunk()? == *b"\0asm", "invalid data")?;
        expect!(
            u32::from_le_bytes(file.read_chunk()?) == 1,
            "unsupported WASM version"
        )?;
        let mut sections = vec![];
        while file.has_data()? {
            let section = Section::decode(file)?;
            sections.push(section);
        }

        let sections = vector_from_vec(sections)?;
        Ok(Binary(sections))
    }
}

pub fn parse_file(file: impl Read) -> Result<Binary> {
    let mut file = ReadTape::new(file);
    Binary::decode(&mut file)
}
