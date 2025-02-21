use crate::read_tape::ReadTape;
use nom::Parser;
use std::error::Error;
use std::io::{self, Read, Result};
use std::marker::PhantomData;

fn invalid_data(err: impl Into<Box<dyn Error + Send + Sync>>) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, err)
}

macro_rules! expect {
    ($condition:expr, $($errmsg:tt)*) => {
        match $condition {
            true => Ok(()),
            false => Err(invalid_data(format!($($errmsg)*))),
        }
    }
}
trait Decode
where
    Self: Sized,
{
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self>;
}

#[derive(Debug)]
struct TodoDecode<const N: usize>;
impl<const N: usize> Decode for TodoDecode<N> {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
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

impl_decode_for_int!(u8 i8 u16 i16 u32 i32 u64 i64);
impl_decode_naive!(f32 f64);

impl<T: Decode> Decode for PhantomData<T> {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        let _ = T::decode(file)?;
        Ok(PhantomData)
    }
}

impl Decode for usize {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        Ok(u32::decode(file)?.try_into().expect("usize too small"))
    }
}

macro_rules! decodable {
    {} => {};
    {$(#[$($meta:tt)*])* enum $name:ident: $type: ty { $($variant:ident $(($($inner:ty),*))? = $value:expr),* $(,)? } $($next:tt)*} => {
        #[repr($type)]
        $(#[$($meta)*])*
        enum $name {
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
        struct $name {
            $(
            $field: $type,
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
}

#[derive(Debug)]
struct CustomSection;

impl Decode for CustomSection {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        let length = usize::decode(file)?;
        let _ = file.read(length)?;
        Ok(CustomSection)
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

type Length = PhantomData<usize>;

impl<T: Decode> Decode for Box<[T]> {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        let len = u32::decode(file)?;
        (0..len).map(|_| T::decode(file)).collect()
    }
}

#[derive(Debug)]
struct TagByte<const TAG: u8>;
impl<const TAG: u8> Decode for TagByte<TAG> {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        expect!(file.read_byte()? == TAG, "expected next byte to be {}", TAG)?;
        Ok(Self)
    }
}

decodable! {
    #[derive(Debug)]
    struct FuncType {
        _signature: TagByte<0x60>,
        parameters: Box<[ValueType]>,
        result: Box<[ValueType]>,
    }

    #[derive(Debug)]
    struct TypeSection {
        functions: Box<[FuncType]>,
    }
}

impl Decode for String {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        let len = usize::decode(file)?;
        Ok(dbg!(
            String::from_utf8_lossy(&file.read(dbg!(len))?).into_owned()
        ))
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
        Function(FunctionIdx) = 0x00,
        Table(TableIdx) = 0x01,
        Memory(MemoryIdx) = 0x02,
        Global(GlobalIdx) = 0x03,
    }

    #[derive(Debug)]
    struct ImportSection {
        imports: Box<[Import]>,
    }

    #[derive(Debug)]
    struct Export {
        name: String,
        description: InterfaceDescription,
    }

    #[derive(Debug)]
    struct ExportSection {
        exports: Box<[Export]>,
    }

    #[derive(Debug)]
    enum ElementKind: u8 {
        FunctionReference = 0x00,
    }

    #[derive(Debug)]
    enum Element: u32 {
        Case00(Expression, Box<[FunctionIdx]>) = 0x00,
        Case01(ElementKind, Box<[FunctionIdx]>) = 0x01,
        Case02(TableIdx, Expression, ElementKind, Box<[FunctionIdx]>) = 0x02,
        Case03(ElementKind, Box<[FunctionIdx]>) = 0x03,
        Case04(Expression, Box<[Expression]>) = 0x04,
        Case05(ValueType, Box<[Expression]>) = 0x05,
        Case06(TableIdx, Expression, ValueType, Box<[Expression]>) = 0x06,
        Case07(ValueType, Box<[Expression]>) = 0x07,
    }

    #[derive(Debug)]
    struct ElementSection {
        elements: Box<[Element]>,
    }

    #[derive(Debug)]
    struct CodeSection {
        definitions: Box<[Definition]>,
    }
}

#[derive(Debug)]
struct Definition {
    locals: Box<[ValueType]>,
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
        let count = usize::decode(file)?;
        let mut locals = vec![];
        for _ in 0..count {
            let (run_length, value_type) = <(usize, ValueType)>::decode(file)?;
            locals.extend_from_slice(
                &std::iter::repeat_n(value_type, run_length).collect::<Box<_>>(),
            );
        }
        let locals = locals.into_boxed_slice();
        let body = Expression::decode(file)?;
        Ok(Definition { locals, body })
    }
}

type TypeIdx = u32;
type FunctionIdx = u32;
type TableIdx = u32;
type MemoryIdx = u32;
type GlobalIdx = u32;
type ElementIdx = u32;
type DataIdx = u32;
type LocalIdx = u32;
type LabelIdx = u32;

type MemoryType = Limit;
type GlobalType = TodoDecode<103>;

decodable! {
    #[derive(Debug)]
    struct FunctionSection {
        signatures: Box<[TypeIdx]>,
    }
}

decodable! {
    #[derive(Debug)]
    enum Limit: u8 {
        HalfBounded(usize) = 0x00,
        Bounded(usize, usize) = 0x01,
    }

    #[derive(Debug)]
    struct TableType {
        element_type: ValueType,
        limits: Limit,
    }

    #[derive(Debug)]
    struct TableSection {
        tables: Box<[TableType]>,
    }

    #[derive(Debug)]
    struct MemorySection {
        memories: Box<[Limit]>,
    }

    #[derive(Debug)]
    struct Global {
        value_type: ValueType,
        is_mutable: bool,
        expression: Expression,
    }

    #[derive(Debug)]
    struct GlobalSection {
        globals: Box<[Global]>,
    }

    #[derive(Debug)]
    struct MemoryArgument {
        align: u32,
        offset: u32,
    }

    #[derive(Debug)]
    enum Instruction: u8 {
        Unreachable = 0x00,
        Nop = 0x01,
        Block(Expression) = 0x02,
        Loop(Expression) = 0x03,
        IfElse(IfElseBlock) = 0x04,
        Return = 0x0f,
        Call(FunctionIdx) = 0x10,
        CallIndirect(TypeIdx, TableIdx) = 0x11,
        Drop = 0x1a,
        LocalGet(LocalIdx) = 0x20,
        LocalSet(LocalIdx) = 0x21,
        LocalTee(LocalIdx) = 0x22,
        GlobalGet(GlobalIdx) = 0x23,
        GlobalSet(GlobalIdx) = 0x24,
        LoadI32(MemoryArgument) = 0x28,
        LoadI64(MemoryArgument) = 0x29,
        LoadF32(MemoryArgument) = 0x2a,
        LoadF64(MemoryArgument) = 0x2b,
        StoreI32(MemoryArgument) = 0x36,
        StoreI64(MemoryArgument) = 0x37,
        StoreF32(MemoryArgument) = 0x38,
        StoreF64(MemoryArgument) = 0x39,
        Store8I32(MemoryArgument) = 0x3a,
        Store16I32(MemoryArgument) = 0x3b,
        Store8I64(MemoryArgument) = 0x3c,
        Store16I64(MemoryArgument) = 0x3d,
        Store32I64(MemoryArgument) = 0x3e,
        ConstI32(i32) = 0x41,
        ConstI64(i64) = 0x42,
        ConstF32(f32) = 0x43,
        ConstF64(f64) = 0x44,
        ClzI32 = 0x67,
        CtzI32 = 0x68,
        PopcntI32 = 0x69,
        AddI32 = 0x6a,
        SubI32 = 0x6b,
        MulI32 = 0x6c,
        DivsI32 = 0x6d,
        DivuI32 = 0x6e,
        RemsI32 = 0x6f,
        RemuI32 = 0x70,
        AndI32 = 0x71,
        OrI32 = 0x72,
        XorI32 = 0x73,
        ShlI32 = 0x74,
        ShrsI32 = 0x75,
        ShruI32 = 0x76,
        RotlI32 = 0x77,
        RotrI32 = 0x78,
        MemorySize(TagByte<0x00>) = 0x3f,
        MemoryGrow(TagByte<0x00>) = 0x40,
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

        let ifso = Expression {
            instructions: ifso.into_boxed_slice(),
        };
        if file.read_byte()? == INSTRUCTION_ELSE {
            let ifnot = Expression::decode(file)?;
            Ok(IfElseBlock::IfElse(ifso, ifnot))
        } else {
            Ok(IfElseBlock::If(ifso))
        }
    }
}

const INSTRUCTION_ELSE: u8 = 0x05;
const INSTRUCTION_END: u8 = 0x0B;

#[derive(Debug)]
struct Expression {
    instructions: Box<[Instruction]>,
}

impl Decode for Expression {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        let mut instructions = vec![];

        while file.peek_byte()? != INSTRUCTION_END {
            instructions.push(Instruction::decode(file)?);
        }
        let _ = file.read_byte();

        Ok(Expression {
            instructions: instructions.into_boxed_slice(),
        })
    }
}

pub struct Binary;

impl Decode for Binary {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        expect!(file.read_chunk()? == *b"\0asm", "invalid data")?;
        expect!(
            u32::from_le_bytes(file.read_chunk()?) == 1,
            "unsupported WASM version"
        )?;
        let mut sections = vec![];
        while dbg!(file.has_data()?) {
            let section = Section::decode(file)?;
            println!("{:?}", section);
            sections.push(section);
        }

        Ok(Binary)
    }
}

pub fn parse_file(file: impl Read) -> Result<Binary> {
    let mut file = ReadTape::new(file);
    Binary::decode(&mut file)
}
