use crate::read_tape::ReadTape;
use std::error::Error;
use std::io::{self, Read, Result};
use std::marker::PhantomData;
use crate::frontend::vector::{Index, WasmVec};

mod vector;

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
        let length = Index::decode(file)?;
        let _ = file.read(length.as_usize())?;
        Ok(CustomSection)
    }
}

decodable! {
    #[derive(Debug)]
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
        Element(Length, TodoDecode<7>) = 0x09,
        Code(Length, TodoDecode<8>) = 0x0a,
        Data(Length, TodoDecode<9>) = 0x0b,
        DataCount(Length, TodoDecode<10>) = 0x0c,
    }
}

type Length = PhantomData<Index>;


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
        Ok(dbg!(
            String::from_utf8_lossy(&file.read(len.as_usize())?).into_owned()
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
        Func(u32) = 0x00,
        Table(u32) = 0x01,
        Memory(u32) = 0x02,
        Global(u32) = 0x03,
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
}

type TypeIdx = u32;
type MemoryType = Limit;
type GlobalType = TodoDecode<103>;

decodable! {
    #[derive(Debug)]
    struct FunctionSection {
        signatures: WasmVec<TypeIdx>,
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
    enum Instruction: u8 {
        Unreachable = 0x00,
        Nop = 0x01,
        Block(Expression) = 0x02,
        Loop(Expression) = 0x03,
        IfElse(IfElseBlock) = 0x04,
        ConstI32(i32) = 0x41,
        ConstI64(i64) = 0x42,
        ConstF32(f32) = 0x43,
        ConstF64(f64) = 0x44,
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
            instructions: ifso.into_boxed_slice().into(),
        };
        match file.read_byte()? {
            INSTRUCTION_ELSE => {
                let ifnot = Expression::decode(file)?;
                Ok(IfElseBlock::IfElse(ifso, ifnot))
            }
            INSTRUCTION_END => Ok(IfElseBlock::If(ifso)),
            _ => unreachable!()
        }
    }
}

const INSTRUCTION_ELSE: u8 = 0x05;
const INSTRUCTION_END: u8 = 0x0B;

#[derive(Debug)]
struct Expression {
    instructions: WasmVec<Instruction>,
}

impl Decode for Expression {
    fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
        let mut instructions = vec![];

        while file.peek_byte()? != INSTRUCTION_END {
            instructions.push(Instruction::decode(file)?);
        }
        let _ = file.read_byte();

        Ok(Expression {
            instructions: instructions.into_boxed_slice().into(),
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
