use crate::frontend::{BlockType, Decode, Expression, FunctionIndex, GlobalIndex, LocalIndex, MemoryArgument, TableIndex, TagByte, TypeIndex, invalid_data, LabelIndex, RefrenceType, ValueType, DataIndex};
use crate::read_tape::ReadTape;
use std::io::{Read, Result};
use crate::frontend::vector::{WasmVec, Index};

#[rustfmt::skip]
macro_rules! ty_param_discard {
    ($_:ty) => { _ };
}

macro_rules! normalize_match_double {
    (
        match ($expr:expr) {
            {$(($one:literal, $two:literal) => $res_op: expr,)*}
            {_ => $fallback: expr}
        }
    ) => {
        #[forbid(unreachable_patterns)]
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
        normalize_match_double! {
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
        normalize_match_double! {
            match ($expr) {
                {$($rest)*
                ($one, $two) => $res_op,}
                {_ => $fallback}
            }
        }
    };
}

macro_rules! normalize_match_single {
    (
        match ($expr:expr) {
            {$(($one:literal, ) => $res_op: expr,)*}
            {_ => $fallback: expr}
        }
    ) => {
        #[forbid(unreachable_patterns)]
        match ($expr) {
            $(($one, ) => $res_op,)*
            _ => $fallback
        }
    };

    (
        match ($expr:expr) {
            {($one:literal, ) => $res_op: expr,
            $($rest:tt)*}
            { _ => $fallback: expr }
        }
    ) => {
        normalize_match_single! {
            match ($expr) {
                {$($rest)*
                ($one, ) => $res_op,}
                {_ => $fallback}
            }
        }
    };

    (
        match ($expr:expr) {
            {($one:literal, $two:literal) => $_: expr,
            $($rest:tt)*}
            {_ => $fallback: expr}
        }
    ) => {
        normalize_match_single! {
            match ($expr) {
                {$($rest)*}
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

                // Warn on duplicate opcode start

                normalize_match_single!(match ((first_byte, )) {
                    {$(
                        ($opcode, $($u32_code)?) => {
                             return Ok(Self::$ident $(($(<$data>::decode(file)?),*))?)
                        },
                    )+}
                    {_ => ()}
                });

                let second_opcode = file.read_leb128::<u32>()?;

                normalize_match_double!(match ((first_byte, second_opcode)) {
                    {$(
                        ($opcode, $($u32_code)?) => {
                             Ok(Self::$ident $(($(<$data>::decode(file)?),*))?)
                        },
                    )+}
                    {_ => Err(invalid_data(format!("invalid instruction (0x{first_byte:02x}, 0x{second_opcode:02x})")))}
                })
            }
        }

        impl Instruction {
            fn name(&self) -> &'static str {
                // Warn on duplicate name
                if false {
                    #[forbid(unreachable_patterns)]
                    match "" {
                        $($name => (),)+
                        _ => ()
                    }
                }

                match self {
                    $(Self::$ident$(($(ty_param_discard!($data)),*))? => $name,)+
                }
            }
        }
    };
}

instruction! {
    // Control Instructions
    ("unreachable", Unreachable) => 0x00,
    ("nop", Nop) => 0x01,
    ("block", Block) => 0x02 (BlockType, Expression),
    ("loop", Loop) => 0x03 (BlockType, Expression),
    ("br", Branch) => 0x0c (LabelIndex),
    ("br_if", BranchIf) => 0x0d (LabelIndex),
    ("br_table", BranchTable) => 0x0e (WasmVec<LabelIndex>, Index),
    ("return ", Return) => 0x0f,
    ("call", Call) => 0x10 (FunctionIndex),
    ("callindirect", CallIndirect) => 0x11 (TypeIndex, TableIndex),

    // Reference Instructions
    ("ref.null", RefNull) => 0xd0 (RefrenceType),
    ("ref.is_null", RefIsNull) => 0xd1,
    ("ref.func", RefFunc) => 0xd2 (FunctionIndex),


    // Parametric Instructions
    ("drop", Drop) => 0x1a,
    ("select", Select) => 0x1b,
    ("select_t*", SelectT) => 0x1c (WasmVec<ValueType>),

    // Variable Instructions
    ("local.get",   LocalGet) => 0x20 ( LocalIndex),
    ("local.set",   LocalSet) => 0x21 ( LocalIndex),
    ("local.tee",   LocalTee) => 0x22 ( LocalIndex),
    ("global.get", GlobalGet) => 0x23 (GlobalIndex),
    ("global.set", GlobalSet) => 0x24 (GlobalIndex),

    // Memory Instructions

    // Primitive load
    ("i32.load",        I32Load) => 0x28 (MemoryArgument),
    ("i64.load",        I64Load) => 0x29 (MemoryArgument),
    ("f32.load",        F32Load) => 0x2a (MemoryArgument),
    ("f64.load",        F64Load) => 0x2b (MemoryArgument),

    // Casting load
    ("i32.load8_s",   I32LoadI8) => 0x2c (MemoryArgument),
    ("i32.load8_u",   I32LoadU8) => 0x2d (MemoryArgument),
    ("i32.load16_s", I32LoadI16) => 0x2e (MemoryArgument),
    ("i32.load16_u", I32LoadU16) => 0x2f (MemoryArgument),
    ("i64.load8_s",   I64LoadI8) => 0x30 (MemoryArgument),
    ("i64.load8_u",   I64LoadU8) => 0x31 (MemoryArgument),
    ("i64.load16_s", I64LoadI16) => 0x32 (MemoryArgument),
    ("i64.load16_u", I64LoadU16) => 0x33 (MemoryArgument),
    ("i64.load32_s", I64LoadI32) => 0x34 (MemoryArgument),
    ("i64.load32_u", I64LoadU32) => 0x35 (MemoryArgument),

    // Primitive store
    ("i32.store",      I32Store) => 0x36 (MemoryArgument),
    ("i64.store",      I64Store) => 0x37 (MemoryArgument),
    ("f32.store",      F32Store) => 0x38 (MemoryArgument),
    ("f64.store",      F64Store) => 0x39 (MemoryArgument),

    // Casting store
    ("i32.store8",   I32StoreI8) => 0x3a (MemoryArgument),
    ("i32.store16", I32StoreI16) => 0x3b (MemoryArgument),
    ("i64.store8",   I64StoreI8) => 0x3c (MemoryArgument),
    ("i64.store16", I64StoreI16) => 0x3d (MemoryArgument),
    ("i64.store32", I64StoreI32) => 0x3e (MemoryArgument),

    ("memory.size",  MemorySize) => 0x3f (TagByte<0x00>),
    ("memory.grow",  MemoryGrow) => 0x40 (TagByte<0x00>),

    ("memory.init", MemoryInit) => 0xFC -> 8 (DataIndex, TagByte<0x00>),
    ("data.drop", DataDrop) => 0xFC -> 9 (DataIndex),
    ("memory.copy", MemoryCopy) => 0xFC -> 10 (TagByte<0x00>, TagByte<0x00>),
    ("memory.fill", MemoryFill) => 0xFC -> 11 (TagByte<0x00>),

    // # Numeric Instructions

    // ## Constants

    ("i32.const", ConstI32) => 0x41 (i32),
    ("i64.const", ConstI64) => 0x42 (i64),
    ("f32.const", ConstF32) => 0x43 (f32),
    ("f64.const", ConstF64) => 0x44 (f64),

    // ## Equality

    // ### i32
    ("i32.eqz", EqZI32) => 0x45,
    ("i32.eq", EqI32) => 0x46,
    ("i32.ne", NeI32) => 0x47,
    ("i32.lt_s", LtI32) => 0x48,
    ("i32.lt_u", LtU32) => 0x49,
    ("i32.gt_s", GtI32) => 0x4a,
    ("i32.gt_u", GtU32) => 0x4b,
    ("i32.le_s", LeI32) => 0x4c,
    ("i32.le_u", LeU32) => 0x4d,
    ("i32.ge_s", GeI32) => 0x4e,
    ("i32.ge_u", GeU32) => 0x4f,

    // ### i64
    ("i64.eqz", EqZI64) => 0x50,
    ("i64.eq", EqI64) => 0x51,
    ("i64.ne", NeI64) => 0x52,
    ("i64.lt_s", LtI64) => 0x53,
    ("i64.lt_u", LtU64) => 0x54,
    ("i64.gt_s", GtI64) => 0x55,
    ("i64.gt_u", GtU64) => 0x56,
    ("i64.le_s", LeI64) => 0x57,
    ("i64.le_u", LeU64) => 0x58,
    ("i64.ge_s", GeI64) => 0x59,
    ("i64.ge_u", GeU64) => 0x5a,

    // ### f32
    ("f32.eq", EqF32) => 0x5b,
    ("f32.ne", NeF32) => 0x5c,
    ("f32.lt", LtF32) => 0x5d,
    ("f32.gt", GtF32) => 0x5e,
    ("f32.le", LeF32) => 0x5f,
    ("f32.ge", GeF32) => 0x60,

    // ### f64
    ("f64.eq", EqF64) => 0x61,
    ("f64.ne", NeF64) => 0x62,
    ("f64.lt", LtF64) => 0x63,
    ("f64.gt", GtF64) => 0x64,
    ("f64.le", LeF64) => 0x65,
    ("f64.ge", GeF64) => 0x66,


    // ## Arithmetic

    // ### i32
    ("i32.clz", ClzI32) => 0x67,
    ("i32.ctz", CtzI32) => 0x68,
    ("i32.popcnt", PopcntI32) => 0x69,
    ("i32.add", AddI32) => 0x6a,
    ("i32.sub", SubI32) => 0x6b,
    ("i32.mul", MulI32) => 0x6c,
    ("i32.div_s", DivI32) => 0x6d,
    ("i32.div_u", DivU32) => 0x6e,
    ("i32.rem_s", RemI32) => 0x6f,
    ("i32.rem_u", RemU32) => 0x70,
    ("i32.and", AndI32) => 0x71,
    ("i32.or", OrI32) => 0x72,
    ("i32.xor", XorI32) => 0x73,
    ("i32.shl", ShlI32) => 0x74,
    ("i32.shr_s", ShrI32) => 0x75,
    ("i32.shr_u", ShruI32) => 0x76,
    ("i32.rotl", RotlI32) => 0x77,
    ("i32.rotr", RotrI32) => 0x78,

    // ### i64
    ("i64.clz", ClzI64) => 0x79,
    ("i64.ctz", CtzI64) => 0x7a,
    ("i64.popcnt", PopcntI64) => 0x7b,
    ("i64.add", AddI64) => 0x7c,
    ("i64.sub", SubI64) => 0x7d,
    ("i64.mul", MulI64) => 0x7e,
    ("i64.div_s", DivI64) => 0x7f,
    ("i64.div_u", DivU64) => 0x80,
    ("i64.rem_s", RemI64) => 0x81,
    ("i64.rem_u", RemU64) => 0x82,
    ("i64.and", AndI64) => 0x83,
    ("i64.or", OrI64) => 0x84,
    ("i64.xor", XorI64) => 0x85,
    ("i64.shl", ShlI64) => 0x86,
    ("i64.shr_s", ShrI64) => 0x87,
    ("i64.shr_u", ShruI64) => 0x88,
    ("i64.rotl", RotlI64) => 0x89,
    ("i64.rotr", RotrI64) => 0x8a,

    // ### f32
    ("f32.abs", AbsF32) => 0x8b,
    ("f32.neg", NegF32) => 0x8c,
    ("f32.ceil", CeilF32) => 0x8d,
    ("f32.floor", FloorF32) => 0x8e,
    ("f32.trunc", TruncF32) => 0x8f,
    ("f32.nearest", NearestF32) => 0x90,
    ("f32.sqrt", SqrtF32) => 0x91,
    ("f32.add", AddF32) => 0x92,
    ("f32.sub", SubF32) => 0x93,
    ("f32.mul", MulF32) => 0x94,
    ("f32.div", DivF32) => 0x95,
    ("f32.min", MinF32) => 0x96,
    ("f32.max", MaxF32) => 0x97,
    ("f32.copysign", CopySignF32) => 0x98,

    // ### f64
    ("f64.abs", AbsF64) => 0x99,
    ("f64.neg", NegF64) => 0x9a,
    ("f64.ceil", CeilF64) => 0x9b,
    ("f64.floor", FloorF64) => 0x9c,
    ("f64.trunc", TruncF64) => 0x9d,
    ("f64.nearest", NearestF64) => 0x9e,
    ("f64.sqrt", SqrtF64) => 0x9f,
    ("f64.add", AddF64) => 0xa0,
    ("f64.sub", SubF64) => 0xa1,
    ("f64.mul", MulF64) => 0xa2,
    ("f64.div", DivF64) => 0xa3,
    ("f64.min", MinF64) => 0xa4,
    ("f64.max", MaxF64) => 0xa5,
    ("f64.copysign", CopySignF64) => 0xa6,


    // Conversion
    ("i32.wrap_i64", I32WrapI64) => 0xa7,
    ("i32.trunc_f32_s", I32TruncF32) => 0xa8,
    ("i32.trunc_f32_u", U32TruncF32) => 0xa9,
    ("i32.trunc_f64_s", I32TruncF64) => 0xaa,
    ("i32.trunc_f64_u", U32TruncF64) => 0xab,
    ("i64.extend_i32_s", I64PromoteI32) => 0xac,
    ("i64.extend_i32_u", I64PromoteU32) => 0xad,
    ("i64.trunc_f32_s", I64TruncF32) => 0xae,
    ("i64.trunc_f32_u", U64TruncF32) => 0xaf,
    ("i64.trunc_f64_s", I64TruncF64) => 0xb0,
    ("i64.trunc_f64_u", U64TruncF64) => 0xb1,
    ("f32.convert_i32_s", F32ConvertI32) => 0xb2,
    ("f32.convert_i32_u", F32ConvertU32) => 0xb3,
    ("f32.convert_i64_s", F32ConvertI64) => 0xb4,
    ("f32.convert_i64_u", F32ConvertU64) => 0xb5,
    ("f32.demote_f64", F32DemoteF64) => 0xb6,
    ("f64.convert_i32_s", F64ConvertI32) => 0xb7,
    ("f64.convert_i32_u", F64ConvertU32) => 0xb8,
    ("f64.convert_i64_s", F64ConvertI64) => 0xb9,
    ("f64.convert_i64_u", F64ConvertU64) => 0xba,
    ("f64.demote_f64", F64PromoteF32) => 0xbb,
    ("i32.reinterpret_f32", I32ReinterpretF32) => 0xbc,
    ("i64.reinterpret_f64", I64ReinterpretF64) => 0xbd,
    ("f32.reinterpret_i32", F32ReinterpretI32) => 0xbe,
    ("f64.reinterpret_i64", F64ReinterpretI64) => 0xbf,


    // ## Sign extension

    ("i32.extend8_s",  I32ExtendI8) => 0xC0,
    ("i32.extend16_s", I32ExtendI16) => 0xC1,
    ("i64.extend8_s",   I64ExtendI8) => 0xC2,
    ("i64.extend16_s", I64ExtendI16) => 0xC3,
    ("i64.extend32_s", I64ExtendI32) => 0xC4,

    // ## Saturating Conversion
    ("i32.trunc_sat_f32_s", I32TruncSatF32) => 0xfc -> 0,
    ("i32.trunc_sat_f32_u", U32TruncSatF32) => 0xfc -> 1,
    ("i32.trunc_sat_f64_s", I32TruncSatF64) => 0xfc -> 2,
    ("i32.trunc_sat_f64_u", U32TruncSatF64) => 0xfc -> 3,
    ("i64.trunc_sat_f32_s", I64TruncSatF32) => 0xfc -> 4,
    ("i64.trunc_sat_f32_u", U64TruncSatF32) => 0xfc -> 5,
    ("i64.trunc_sat_f64_s", I64TruncSatF64) => 0xfc -> 6,
    ("i64.trunc_sat_f64_u", U64TruncSatF64) => 0xfc -> 7,

    // TODO: Vector Instructions
}
