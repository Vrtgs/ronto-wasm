use std::io::{Read, Result};
use crate::frontend::{Decode, invalid_data, BlockType, Expression, FunctionIndex, GlobalIndex, IfElseBlock, LocalIndex, MemoryArgument, TableIndex, TagByte, TypeIndex};
use crate::read_tape::ReadTape;

macro_rules! absent {
    ($_:tt) => { false };
    (     ) => { true  };
}

macro_rules! instruction {
    ($(
        ($name: literal, $ident: ident) => $opcode: literal $(-> $u32_code: literal)? $(($($data: ty),*))?
    ),+ $(,)?) => {
        #[derive(Debug, Eq, PartialEq)]
        #[non_exhaustive]
        pub enum Instruction {
            $($ident $(($($data),*))? ),+
        }
        
        impl Decode for Instruction {
            fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
                let first_byte = file.read_byte()?;
                Ok(match first_byte {
                    $(
                        $opcode if !absent!($($u32_code)?) => Self::$ident $(($(<$data>::decode(file)?),*))?,
                    )+
                    _ => {
                        let second_opcode = file.read_leb128::<u32>()?;
                        let instruction = (first_byte, second_opcode);
                        match instruction {
                            $(
                                $(($opcode, $u32_code) => {
                                     Self::$ident $(($(<$data>::decode(file)?),*))?
                                },)?
                            )+
                            _ => return Err(invalid_data(format!("invalid instruction {instruction:?}")))
                        }
                    }
                })
            }
        }
        
        impl Instruction {
            fn name(&self) -> &'static str {
                match self {
                    $(Self::$ident$(($($data: ty),*))? => $name,)+
                }
            }
        }
    };
}

instruction! {
    ("unreachable", Unreachable) => 0x00,
    ("nop", Nop) => 0x01,
}

// decodable! {
//     #[derive(Debug)]
//     enum Instruction: u8 {
//         Unreachable = 0x00,
//         Nop = 0x01,
//         Block(BlockType, Expression) = 0x02,
//         Loop(BlockType, Expression) = 0x03,
//         IfElse(BlockType, IfElseBlock) = 0x04,
//         Return = 0x0f,
//         Call(FunctionIndex) = 0x10,
//         CallIndirect(TypeIndex, TableIndex) = 0x11,
//         Drop = 0x1a,
//         LocalGet(LocalIndex) = 0x20,
//         LocalSet(LocalIndex) = 0x21,
//         LocalTee(LocalIndex) = 0x22,
//         GlobalGet(GlobalIndex) = 0x23,
//         GlobalSet(GlobalIndex) = 0x24,
//         LoadI32(MemoryArgument) = 0x28,
//         LoadI64(MemoryArgument) = 0x29,
//         LoadF32(MemoryArgument) = 0x2a,
//         LoadF64(MemoryArgument) = 0x2b,
//         StoreI32(MemoryArgument) = 0x36,
//         StoreI64(MemoryArgument) = 0x37,
//         StoreF32(MemoryArgument) = 0x38,
//         StoreF64(MemoryArgument) = 0x39,
//         Store8I32(MemoryArgument) = 0x3a,
//         Store16I32(MemoryArgument) = 0x3b,
//         Store8I64(MemoryArgument) = 0x3c,
//         Store16I64(MemoryArgument) = 0x3d,
//         Store32I64(MemoryArgument) = 0x3e,
//         MemorySize(TagByte<0x00>) = 0x3f,
//         MemoryGrow(TagByte<0x00>) = 0x40,
//         ConstI32(i32) = 0x41,
//         ConstI64(i64) = 0x42,
//         ConstF32(f32) = 0x43,
//         ConstF64(f64) = 0x44,
//         ClzI32 = 0x67,
//         CtzI32 = 0x68,
//         PopcntI32 = 0x69,
//         AddI32 = 0x6a,
//         SubI32 = 0x6b,
//         MulI32 = 0x6c,
//         DivsI32 = 0x6d,
//         DivuI32 = 0x6e,
//         RemsI32 = 0x6f,
//         RemuI32 = 0x70,
//         AndI32 = 0x71,
//         OrI32 = 0x72,
//         XorI32 = 0x73,
//         ShlI32 = 0x74,
//         ShrsI32 = 0x75,
//         ShruI32 = 0x76,
//         RotlI32 = 0x77,
//         RotrI32 = 0x78,
//         RefFunc(FunctionIndex) = 0xd2,
//     }
// }