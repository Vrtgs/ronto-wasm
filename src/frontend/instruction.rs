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
            #[expect(dead_code)]
            pub fn name(&self) -> &'static str {
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

// Same safety as `std::mem::transmute`
// but it can transmute between generically sized arrays
// to [T; N] -> [U; N] assuming T is a valid layout of U
unsafe fn transmute_any<Src: Copy, Dst: Copy>(src: Src) -> Dst {
    const { assert!(size_of::<Src>() == size_of::<Dst>()) }
    union Transmute<Src: Copy, Dst: Copy> {
        from: Src,
        to: Dst
    }

    unsafe { Transmute { from: src }.to }
}


macro_rules! vector_lane {
    (
        $bits: literal x $count: literal {
            $first: ident $( $rest:ident)*
        }
    ) => {
        const _: () = assert!(128/$bits == $count);
        paste::paste!{
            #[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
            #[repr(u8)]
            pub enum [<Vector $bits x $count Lane>] {
                $first =  0 $(, $rest)*
            }

            impl [<Vector $bits x $count Lane>] {
                #[inline(always)]
                pub fn try_from_byte(byte: u8) -> Result<Self> {
                    match byte {
                        // Safety:
                        // u8 in the range of 0..16 is guaranteed to be a valid layout of VectorLane
                        0..$count => Ok(unsafe { std::mem::transmute::<u8, Self>(byte) }),
                        _ => Err(invalid_data("vector lane must be between 0 and 16"))
                    }
                }
            }

            impl Decode for [<Vector $bits x $count Lane>] {
                #[inline(always)]
                fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
                    file.read_byte().and_then([<Vector $bits x $count Lane>]::try_from_byte)
                }
            }

            impl<const N: usize> Decode for [ [<Vector $bits x $count Lane>] ; N] {
                fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
                    let chunk = file.read_chunk::<N>()?;
                    for byte in chunk {
                        // this generates much less llvm IR
                        #[allow(clippy::question_mark)]
                        if let Err(err) = [<Vector $bits x $count Lane>]::try_from_byte(byte) {
                            return Err(err)
                        }
                    }
                    // Safety: all bytes are valid vector lanes
                    Ok(unsafe { transmute_any(chunk) })
                }
            }
        }
    };
}

vector_lane! {
    8 x 16 {
        _00 _01 _02 _03
        _04 _05 _06 _07
        _08 _09 _10 _11
        _12 _13 _14 _15
    }
}

vector_lane! {
    16 x 8 {
        _0 _1 _2 _3
        _4 _5 _6 _7
    }
}

vector_lane! {
    32 x 4 {
        _0 _1
        _2 _3
    }
}

vector_lane! {
    64 x 2 { _0 _1 }
}

#[rustfmt::skip]
instruction! {
    // Control Instructions
    ("unreachable", Unreachable) => 0x00,
    ("nop", Nop) => 0x01,
    ("block", Block) => 0x02 (BlockType, Expression),
    ("loop", Loop) => 0x03 (BlockType, Expression),
    ("br", Branch) => 0x0c (LabelIndex),
    ("br_if", BranchIf) => 0x0d (LabelIndex),
    ("br_table", BranchTable) => 0x0e (WasmVec<LabelIndex>, Index),
    ("return", Return) => 0x0f,
    ("call", Call) => 0x10 (FunctionIndex),
    ("call_indirect", CallIndirect) => 0x11 (TypeIndex, TableIndex),

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
    ("i32.eq",   EqI32) => 0x46,
    ("i32.ne",   NeI32) => 0x47,
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
    ("i64.eq",   EqI64) => 0x51,
    ("i64.ne",   NeI64) => 0x52,
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
    ("i32.clz",       ClzI32) => 0x67,
    ("i32.ctz",       CtzI32) => 0x68,
    ("i32.popcnt", PopcntI32) => 0x69,
    ("i32.add",       AddI32) => 0x6a,
    ("i32.sub",       SubI32) => 0x6b,
    ("i32.mul",       MulI32) => 0x6c,
    ("i32.div_s",     DivI32) => 0x6d,
    ("i32.div_u",     DivU32) => 0x6e,
    ("i32.rem_s",     RemI32) => 0x6f,
    ("i32.rem_u",     RemU32) => 0x70,
    ("i32.and",       AndI32) => 0x71,
    ("i32.or",         OrI32) => 0x72,
    ("i32.xor",       XorI32) => 0x73,
    ("i32.shl",       ShlI32) => 0x74,
    ("i32.shr_s",     ShrI32) => 0x75,
    ("i32.shr_u",    ShruI32) => 0x76,
    ("i32.rotl",     RotlI32) => 0x77,
    ("i32.rotr",     RotrI32) => 0x78,

    // ### i64
    ("i64.clz",       ClzI64) => 0x79,
    ("i64.ctz",       CtzI64) => 0x7a,
    ("i64.popcnt", PopcntI64) => 0x7b,
    ("i64.add",       AddI64) => 0x7c,
    ("i64.sub",       SubI64) => 0x7d,
    ("i64.mul",       MulI64) => 0x7e,
    ("i64.div_s",     DivI64) => 0x7f,
    ("i64.div_u",     DivU64) => 0x80,
    ("i64.rem_s",     RemI64) => 0x81,
    ("i64.rem_u",     RemU64) => 0x82,
    ("i64.and",       AndI64) => 0x83,
    ("i64.or",         OrI64) => 0x84,
    ("i64.xor",       XorI64) => 0x85,
    ("i64.shl",       ShlI64) => 0x86,
    ("i64.shr_s",     ShrI64) => 0x87,
    ("i64.shr_u",    ShruI64) => 0x88,
    ("i64.rotl",     RotlI64) => 0x89,
    ("i64.rotr",     RotrI64) => 0x8a,

    // ### f32
    ("f32.abs",           AbsF32) => 0x8b,
    ("f32.neg",           NegF32) => 0x8c,
    ("f32.ceil",         CeilF32) => 0x8d,
    ("f32.floor",       FloorF32) => 0x8e,
    ("f32.trunc",       TruncF32) => 0x8f,
    ("f32.nearest",   NearestF32) => 0x90,
    ("f32.sqrt",         SqrtF32) => 0x91,
    ("f32.add",           AddF32) => 0x92,
    ("f32.sub",           SubF32) => 0x93,
    ("f32.mul",           MulF32) => 0x94,
    ("f32.div",           DivF32) => 0x95,
    ("f32.min",           MinF32) => 0x96,
    ("f32.max",           MaxF32) => 0x97,
    ("f32.copysign", CopySignF32) => 0x98,

    // ### f64
    ("f64.abs",           AbsF64) => 0x99,
    ("f64.neg",           NegF64) => 0x9a,
    ("f64.ceil",         CeilF64) => 0x9b,
    ("f64.floor",       FloorF64) => 0x9c,
    ("f64.trunc",       TruncF64) => 0x9d,
    ("f64.nearest",   NearestF64) => 0x9e,
    ("f64.sqrt",         SqrtF64) => 0x9f,
    ("f64.add",           AddF64) => 0xa0,
    ("f64.sub",           SubF64) => 0xa1,
    ("f64.mul",           MulF64) => 0xa2,
    ("f64.div",           DivF64) => 0xa3,
    ("f64.min",           MinF64) => 0xa4,
    ("f64.max",           MaxF64) => 0xa5,
    ("f64.copysign", CopySignF64) => 0xa6,


    // Conversion
    ("i32.wrap_i64",               I32WrapI64) => 0xa7,
    ("i32.trunc_f32_s",           I32TruncF32) => 0xa8,
    ("i32.trunc_f32_u",           U32TruncF32) => 0xa9,
    ("i32.trunc_f64_s",           I32TruncF64) => 0xaa,
    ("i32.trunc_f64_u",           U32TruncF64) => 0xab,
    ("i64.extend_i32_s",        I64PromoteI32) => 0xac,
    ("i64.extend_i32_u",        I64PromoteU32) => 0xad,
    ("i64.trunc_f32_s",           I64TruncF32) => 0xae,
    ("i64.trunc_f32_u",           U64TruncF32) => 0xaf,
    ("i64.trunc_f64_s",           I64TruncF64) => 0xb0,
    ("i64.trunc_f64_u",           U64TruncF64) => 0xb1,
    ("f32.convert_i32_s",       F32ConvertI32) => 0xb2,
    ("f32.convert_i32_u",       F32ConvertU32) => 0xb3,
    ("f32.convert_i64_s",       F32ConvertI64) => 0xb4,
    ("f32.convert_i64_u",       F32ConvertU64) => 0xb5,
    ("f32.demote_f64",           F32DemoteF64) => 0xb6,
    ("f64.convert_i32_s",       F64ConvertI32) => 0xb7,
    ("f64.convert_i32_u",       F64ConvertU32) => 0xb8,
    ("f64.convert_i64_s",       F64ConvertI64) => 0xb9,
    ("f64.convert_i64_u",       F64ConvertU64) => 0xba,
    ("f64.demote_f64",          F64PromoteF32) => 0xbb,
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

    // Vector Instructions

    // ## Vector Memory Instructions
    ("v128.load",                 V128Load) => 0xfd ->  0 (MemoryArgument),
    ("v128.load8x8_s",        V128LoadI8x8) => 0xfd ->  1 (MemoryArgument),
    ("v128.load8x8_u",        V128LoadU8x8) => 0xfd ->  2 (MemoryArgument),
    ("v128.load16x4_s",      V128LoadI16x4) => 0xfd ->  3 (MemoryArgument),
    ("v128.load16x4_u",      V128LoadU16x4) => 0xfd ->  4 (MemoryArgument),
    ("v128.load32x2_s",      V128LoadI32x4) => 0xfd ->  5 (MemoryArgument),
    ("v128.load32x2_u",      V128LoadU32x4) => 0xfd ->  6 (MemoryArgument),
    ("v128.load8_splat",   V128LoadI8Splat) => 0xfd ->  7 (MemoryArgument),
    ("v128.load16_splat", V128LoadI16Splat) => 0xfd ->  8 (MemoryArgument),
    ("v128.load32_splat", V128LoadI32Splat) => 0xfd ->  9 (MemoryArgument),
    ("v128.load64_splat", V128LoadI64Splat) => 0xfd -> 10 (MemoryArgument),
    ("v128.load32_zero",   V128LoadI32Zero) => 0xfd -> 92 (MemoryArgument),
    ("v128.load64_zero",   V128LoadI64Zero) => 0xfd -> 93 (MemoryArgument),
    ("v128.store",               V128Store) => 0xfd -> 11 (MemoryArgument),
    ("v128.load8_lane",      V128Load8Lane) => 0xfd -> 84 (MemoryArgument, Vector8x16Lane),
    ("v128.load16_lane",    V128Load16Lane) => 0xfd -> 85 (MemoryArgument, Vector16x8Lane),
    ("v128.load32_lane",    V128Load32Lane) => 0xfd -> 86 (MemoryArgument, Vector32x4Lane),
    ("v128.load64_lane",    V128Load64Lane) => 0xfd -> 87 (MemoryArgument, Vector64x2Lane),
    ("v128.store8_lane",    V128Store8Lane) => 0xfd -> 88 (MemoryArgument, Vector8x16Lane),
    ("v128.store16_lane",  V128Store16Lane) => 0xfd -> 89 (MemoryArgument, Vector16x8Lane),
    ("v128.store32_lane",  V128Store32Lane) => 0xfd -> 90 (MemoryArgument, Vector32x4Lane),
    ("v128.store64_lane",  V128Store64Lane) => 0xfd -> 91 (MemoryArgument, Vector64x2Lane),

    // ## Vector Constant
    ("v128.const", V128Const) => 0xfd -> 12 (u128),

    // ## Vector Shuffle
    ("v128.shuffle", V128Shuffle) => 0xfd -> 13 ([Vector8x16Lane; 16]),

    // ## Vector Lane Manipulation
    ("i8x16.extract_lane_s", I8x16ExtractLane) => 0xfd -> 21 ([Vector8x16Lane; 16]),
    ("i8x16.extract_lane_u", U8x16ExtractLane) => 0xfd -> 22 ([Vector8x16Lane; 16]),
    ("i8x16.replace_lane",   I8x16ReplaceLane) => 0xfd -> 23 ([Vector8x16Lane; 16]),
    ("i16x8.extract_lane_s", I16x8ExtractLane) => 0xfd -> 24 ([Vector16x8Lane; 16]),
    ("i16x8.extract_lane_u", U16x8ExtractLane) => 0xfd -> 25 ([Vector16x8Lane; 16]),
    ("i16x8.replace_lane",   I16x8ReplaceLane) => 0xfd -> 26 ([Vector16x8Lane; 16]),
    ("i32x4.extract_lane",   I32x4ExtractLane) => 0xfd -> 27 ([Vector32x4Lane; 16]),
    ("i32x4.replace_lane",   I32x4ReplaceLane) => 0xfd -> 28 ([Vector32x4Lane; 16]),
    ("i64x2.extract_lane",   I64x2ExtractLane) => 0xfd -> 29 ([Vector64x2Lane; 16]),
    ("i64x2.replace_lane",   I64x2ReplaceLane) => 0xfd -> 30 ([Vector64x2Lane; 16]),
    ("f32x4.extract_lane",   F32x4ExtractLane) => 0xfd -> 31 ([Vector32x4Lane; 16]),
    ("f32x4.replace_lane",   F32x4ReplaceLane) => 0xfd -> 32 ([Vector32x4Lane; 16]),
    ("f64x2.extract_lane",   F64x2ExtractLane) => 0xfd -> 33 ([Vector64x2Lane; 16]),
    ("f64x2.replace_lane",   F64x2ReplaceLane) => 0xfd -> 34 ([Vector64x2Lane; 16]),

    ("i8x16.swizzle",   I8x16Swizzle) => 0xfd -> 14,
    ("i8x16.splat",       I8x16Splat) => 0xfd -> 15,
    ("i16x8.splat",       I16x8Splat) => 0xfd -> 16,
    ("i32x4.splat",       I32x4Splat) => 0xfd -> 17,
    ("i64x2.splat",       I64x2Splat) => 0xfd -> 18,
    ("f32x4.splat",       F32x4Splat) => 0xfd -> 19,
    ("f64x2.splat",       F64x2Splat) => 0xfd -> 20,


    // ## Equality checks

    // ### i8
    ("i8x16.eq",   I8x16Eq) => 0xfd -> 35,
    ("i8x16.ne",   I8x16Ne) => 0xfd -> 36,
    ("i8x16.lt_s", I8x16Lt) => 0xfd -> 37,
    ("i8x16.lt_u", U8x16Lt) => 0xfd -> 38,
    ("i8x16.gt_s", I8x16Gt) => 0xfd -> 39,
    ("i8x16.gt_u", U8x16Gt) => 0xfd -> 40,
    ("i8x16.le_s", I8x16Le) => 0xfd -> 41,
    ("i8x16.le_u", U8x16Le) => 0xfd -> 42,
    ("i8x16.ge_s", I8x16Ge) => 0xfd -> 43,
    ("i8x16.ge_u", U8x16Ge) => 0xfd -> 44,

    // ### i16
    ("i16x8.eq",   I16x8Eq) => 0xfd -> 45,
    ("i16x8.ne",   I16x8Ne) => 0xfd -> 46,
    ("i16x8.lt_s", I16x8Lt) => 0xfd -> 47,
    ("i16x8.lt_u", U16x8Lt) => 0xfd -> 48,
    ("i16x8.gt_s", I16x8Gt) => 0xfd -> 49,
    ("i16x8.gt_u", U16x8Gt) => 0xfd -> 50,
    ("i16x8.le_s", I16x8Le) => 0xfd -> 51,
    ("i16x8.le_u", U16x8Le) => 0xfd -> 52,
    ("i16x8.ge_s", I16x8Ge) => 0xfd -> 53,
    ("i16x8.ge_u", U16x8Ge) => 0xfd -> 54,

    // ### i32
    ("i32x4.eq",   I32x4Eq) => 0xfd -> 55,
    ("i32x4.ne",   I32x4Ne) => 0xfd -> 56,
    ("i32x4.lt_s", I32x4Lt) => 0xfd -> 57,
    ("i32x4.lt_u", U32x4Lt) => 0xfd -> 58,
    ("i32x4.gt_s", I32x4Gt) => 0xfd -> 59,
    ("i32x4.gt_u", U32x4Gt) => 0xfd -> 60,
    ("i32x4.le_s", I32x4Le) => 0xfd -> 61,
    ("i32x4.le_u", U32x4Le) => 0xfd -> 62,
    ("i32x4.ge_s", I32x4Ge) => 0xfd -> 63,
    ("i32x4.ge_u", U32x4Ge) => 0xfd -> 64,

    // ### i64
    ("i64x2.eq",   I64x2Eq) => 0xfd -> 214,
    ("i64x2.ne",   I64x2Ne) => 0xfd -> 215,
    ("i64x2.lt_s", I64x2Lt) => 0xfd -> 216,
    ("i64x2.lt_u", U64x2Lt) => 0xfd -> 217,
    ("i64x2.gt_s", I64x2Gt) => 0xfd -> 218,
    ("i64x2.gt_u", U64x2Gt) => 0xfd -> 219,
    ("i64x2.le_s", I64x2Le) => 0xfd -> 220,
    ("i64x2.le_u", U64x2Le) => 0xfd -> 221,
    ("i64x2.ge_s", I64x2Ge) => 0xfd -> 222,
    ("i64x2.ge_u", U64x2Ge) => 0xfd -> 223,

    // ### f32
    ("f32x4.eq", F32x4Eq) => 0xfd -> 65,
    ("f32x4.ne", F32x4Ne) => 0xfd -> 66,
    ("f32x4.lt", F32x4Lt) => 0xfd -> 67,
    ("f32x4.gt", F32x4Gt) => 0xfd -> 68,
    ("f32x4.le", F32x4Le) => 0xfd -> 69,
    ("f32x4.ge", F32x4Ge) => 0xfd -> 70,
    
    // ### f64
    ("f64x2.eq", F64x2Eq) => 0xfd -> 71,
    ("f64x2.ne", F64x2Ne) => 0xfd -> 72,
    ("f64x2.lt", F64x2Lt) => 0xfd -> 73,
    ("f64x2.gt", F64x2Gt) => 0xfd -> 74,
    ("f64x2.le", F64x2Le) => 0xfd -> 75,
    ("f64x2.ge", F64x2Ge) => 0xfd -> 76,

    // ## Bitwise operations

    // ### v128
    ("v128.not",             V128Not) => 0xfd -> 77,
    ("v128.and",             V128And) => 0xfd -> 78,
    ("v128.andnot",       V128AndNot) => 0xfd -> 79,
    ("v128.or",               V128Or) => 0xfd -> 80,
    ("v128.xor",             V128Xor) => 0xfd -> 81,
    ("v128.bitselect", V128BitSelect) => 0xfd -> 82,
    ("v128.any_true",    V128AnyTrue) => 0xfd -> 83,


    // ## Numeric operations

    // ### i8
    ("i8x16.abs",                    I8x16Abs) => 0xfd ->  96,
    ("i8x16.neg",                    I8x16Neg) => 0xfd ->  97,
    ("i8x16.popcnt",            I8x16PopCount) => 0xfd ->  98,
    ("i8x16.all_true",           I8x16AllTrue) => 0xfd ->  99,
    ("i8x16.bitmask",            I8x16Bitmask) => 0xfd -> 100,
    ("i8x16.narrow_i16x8_s", I8x16NarrowI16x8) => 0xfd -> 101,
    ("i8x16.narrow_i16x8_u", U8x16NarrowU16x8) => 0xfd -> 102,
    ("i8x16.shl",                    I8x16Shl) => 0xfd -> 107,
    ("i8x16.shr_s",                  I8x16Shr) => 0xfd -> 108,
    ("i8x16.shr_u",                  U8x16Shr) => 0xfd -> 109,
    ("i8x16.add",                    I8x16Add) => 0xfd -> 110,
    ("i8x16.add_sat_s",           I8x16AddSat) => 0xfd -> 111,
    ("i8x16.add_sat_u",           U8x16AddSat) => 0xfd -> 112,
    ("i8x16.sub",                    I8x16Sub) => 0xfd -> 113,
    ("i8x16.sub_sat_s",           I8x16SubSat) => 0xfd -> 114,
    ("i8x16.sub_sat_u",           U8x16SubSat) => 0xfd -> 115,
    ("i8x16.min_s",                  I8x16Min) => 0xfd -> 118,
    ("i8x16.min_u",                  U8x16Min) => 0xfd -> 119,
    ("i8x16.max_s",                  I8x16Max) => 0xfd -> 120,
    ("i8x16.max_u",                  U8x16Max) => 0xfd -> 121,
    ("i8x16.avgr_u",                U8x16Avgr) => 0xfd -> 123,
}
