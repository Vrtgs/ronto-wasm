use std::io::{Read, Result};
use crate::frontend::{Decode, invalid_data, BlockType, Expression, FunctionIndex, GlobalIndex, IfElseBlock, LocalIndex, MemoryArgument, TableIndex, TagByte, TypeIndex};
use crate::read_tape::ReadTape;

macro_rules! absent {
    ($_:tt) => { false };
    (     ) => { true  };
}

macro_rules! normalize_match {
    (
        match ($expr:expr) {
            {$(($one:literal, $two:literal) => $res_op: expr,)*}
            {_ => $fallback: expr}
        }
    ) => {
        match ($expr) {
            $(($one, $two) => $res_op,)*
            _ => $fallback
        }
    };
    
    (
        match ($expr:expr) {
            {($one:literal, ) => $_: expr,
            $($rest:tt)*}
            { _ => $fallback: expr }
        }
    ) => {
        normalize_match! {
            match ($expr) {
                {$($rest)*}
                {_ => $fallback}
            }
        }
    };
    
    (
        match ($expr:expr) {
            {($one:literal, $two:literal) => $res_op: expr,
            $($rest:tt)*}
            {_ => $fallback: expr}
        }
    ) => {
        normalize_match! {
            match ($expr) {
                {$($rest)*
                ($one, $two) => $res_op,}
                {_ => $fallback}
            }
        }
    };
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
                        normalize_match!(match (instruction) {
                            {$(
                                ($opcode, $($u32_code)?) => {
                                     Self::$ident $(($(<$data>::decode(file)?),*))?
                                },
                            )+}
                            {_ => return Err(invalid_data(format!("invalid instruction {instruction:?}")))}
                        })
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