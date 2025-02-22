use crate::frontend::{
    BlockType, Decode, Expression, FunctionIndex, GlobalIndex, IfElseBlock, LocalIndex,
    MemoryArgument, TableIndex, TagByte, TypeIndex, invalid_data,
};
use crate::read_tape::ReadTape;
use std::io::{Read, Result};

macro_rules! discard {
    ($_:tt) => {
        _
    };
}

macro_rules! absent {
    ($_:tt) => {
        false
    };
    (     ) => {
        true
    };
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
        #[derive(Debug, PartialEq)]
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
                    $(Self::$ident$(($(discard!($data)),*))? => $name,)+
                }
            }
        }
    };
}

instruction! {
        ("unreachable ", Unreachable) => 0x00,
        ("nop", Nop) => 0x01,
        ("block", Block) => 0x02 (BlockType, Expression),
        ("loop", Loop) => 0x03 (BlockType, Expression),
        ("ifelse", IfElse) => 0x04 (BlockType, IfElseBlock),
        ("return ", Return) => 0x0f,
        ("call", Call) => 0x10 (FunctionIndex),
        ("callindirect", CallIndirect) => 0x11 (TypeIndex, TableIndex),
        ("drop ", Drop) => 0x1a,
        ("localget", LocalGet) => 0x20 (LocalIndex),
        ("localset", LocalSet) => 0x21 (LocalIndex),
        ("localtee", LocalTee) => 0x22 (LocalIndex),
        ("globalget", GlobalGet) => 0x23 (GlobalIndex),
        ("globalset", GlobalSet) => 0x24 (GlobalIndex),
        ("loadi32", LoadI32) => 0x28 (MemoryArgument),
        ("loadi64", LoadI64) => 0x29 (MemoryArgument),
        ("loadf32", LoadF32) => 0x2a (MemoryArgument),
        ("loadf64", LoadF64) => 0x2b (MemoryArgument),
        ("storei32", StoreI32) => 0x36 (MemoryArgument),
        ("storei64", StoreI64) => 0x37 (MemoryArgument),
        ("storef32", StoreF32) => 0x38 (MemoryArgument),
        ("storef64", StoreF64) => 0x39 (MemoryArgument),
        ("store8i32", Store8I32) => 0x3a (MemoryArgument),
        ("store16i32", Store16I32) => 0x3b (MemoryArgument),
        ("store8i64", Store8I64) => 0x3c (MemoryArgument),
        ("store16i64", Store16I64) => 0x3d (MemoryArgument),
        ("store32i64", Store32I64) => 0x3e (MemoryArgument),
        ("memorysize", MemorySize) => 0x3f (TagByte<0x00>),
        ("memorygrow", MemoryGrow) => 0x40 (TagByte<0x00>),
        ("consti32", ConstI32) => 0x41 (i32),
        ("consti64", ConstI64) => 0x42 (i64),
        ("constf32", ConstF32) => 0x43 (f32),
        ("constf64", ConstF64) => 0x44 (f64),
        ("clzi32 ", ClzI32) => 0x67,
        ("ctzi32 ", CtzI32) => 0x68,
        ("popcnti32 ", PopcntI32) => 0x69,
        ("addi32 ", AddI32) => 0x6a,
        ("subi32 ", SubI32) => 0x6b,
        ("muli32 ", MulI32) => 0x6c,
        ("divsi32 ", DivsI32) => 0x6d,
        ("divui32 ", DivuI32) => 0x6e,
        ("remsi32 ", RemsI32) => 0x6f,
        ("remui32 ", RemuI32) => 0x70,
        ("andi32 ", AndI32) => 0x71,
        ("ori32 ", OrI32) => 0x72,
        ("xori32 ", XorI32) => 0x73,
        ("shli32 ", ShlI32) => 0x74,
        ("shrsi32 ", ShrsI32) => 0x75,
        ("shrui32 ", ShruI32) => 0x76,
        ("rotli32 ", RotlI32) => 0x77,
        ("rotri32 ", RotrI32) => 0x78,
        ("reffunc", RefFunc) => 0xd2 (FunctionIndex),
        ("f32x4.sub", SubX4F32) => 0xfd -> 229,
}
