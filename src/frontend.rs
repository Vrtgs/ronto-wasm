use crate::bufferedread::BufferedRead;
use std::error::Error;
use std::io::{self, Read, Result};

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
trait Deserialize
where
    Self: Sized,
{
    fn deserialize(file: &mut BufferedRead<impl Read>) -> Result<Self>;
}

macro_rules! impl_deserialize_for_ints {
    ($($ints:ident)*) => {
        $(
            impl Deserialize for $ints {
                fn deserialize(file: &mut BufferedRead<impl Read>) -> Result<Self> {
                    file.read_varint()
                }
            }
        )*
    }
}

impl_deserialize_for_ints!(u8 i8 u16 i16 u32 i32 u64 i64);

macro_rules! deserializable_enum {
    ($(enum $name:ident: $type: ty { $($variant:ident = $value:expr),* $(,)? })*) => {
        $(
        #[repr($type)]
        #[derive(Debug)]
        enum $name {
            $($variant = $value),*
        }
        impl Deserialize for $name {
            fn deserialize(file: &mut BufferedRead<impl Read>) -> Result<Self> {
                let variant = file.read_varint::<u8>()?;
                match variant {
                    $(
                    $value => Ok($name::$variant),
                    )*
                    _ => Err(invalid_data(format!("invalid {}: {}", stringify!($name), variant))),
                }
            }
        }
        )*
    }
}

deserializable_enum! {
    enum ValueType: u8 {
        I32 = 0x7F,
        I64 = 0x7E,
        F32 = 0x7D,
        F64 = 0x7C,
        V128 = 0x7B,
        FuncRef = 0x70,
        ExternRef = 0x6F,
    }

    enum SectionType: u8 {
        Custom = 0,
        Type = 1,
        Import = 2,
        Function = 3,
        Table = 4,
        Memory = 5,
        Global = 6,
        Export = 7,
        Start = 8,
        Element = 9,
        Code = 10,
        Data = 11,
        DataCount = 12,
    }
}

impl<T: Deserialize> Deserialize for Box<[T]> {
    fn deserialize(file: &mut BufferedRead<impl Read>) -> Result<Self> {
        let len = u32::deserialize(file)?;
        (0..len).map(|_| T::deserialize(file)).collect()
    }
}

#[derive(Debug)]
struct FuncType {
    parameters: Box<[ValueType]>,
    result: Box<[ValueType]>,
}

impl Deserialize for FuncType {
    fn deserialize(file: &mut BufferedRead<impl Read>) -> Result<Self> {
        expect!(file.next_byte()? == 0x60, "invalid functype")?;
        let parameters = Box::<[ValueType]>::deserialize(file)?;
        let result = Box::<[ValueType]>::deserialize(file)?;
        Ok(Self { parameters, result })
    }
}

type TypeSection = Box<[FuncType]>;

impl Deserialize for &str {
    fn deserialize(file: &mut BufferedRead<impl Read>) -> Result<Self> {
       let len = u32::deserialize(file);
    }
}

pub struct Binary;

impl Deserialize for Binary {
    fn deserialize(file: &mut BufferedRead<impl Read>) -> Result<Self> {
        expect!(file.next_chunk()? == *b"\0asm", "invalid data")?;
        expect!(
            u32::from_le_bytes(file.next_chunk()?) == 1,
            "unsupported WASM version"
        )?;
        let type_section = None;
        let import_section = None;
        loop {
            let section_type = SectionType::deserialize(file)?;
            let length = u32::deserialize(file)?
                .try_into()
                .expect("u32 should be larger than a usize");
            match section_type {
                SectionType::Custom => {
                    let _ = file.read(length);
                }
                SectionType::Type => {
                    type_section = Some(TypeSection::deserialize(file)?);
                }
                SectionType::Import => {
                    import_section = Some(ImportSection::deserialize(file)?);
                }
                _ => todo!("implement {:?} sections", section_type),
            }
        }

        Ok(Binary)
    }
}

pub fn parse_file(file: impl Read) -> Result<Binary> {
    let mut file = BufferedRead::new(file);
    Binary::deserialize(&mut file)
}
