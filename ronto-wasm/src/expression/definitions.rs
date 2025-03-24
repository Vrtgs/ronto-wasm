use super::typed_instruction_code::{
    AccessType, Call, CastingMemoryAccess, MemoryAccess, MemoryInit, Primitive, RefNull,
    TypedInstructionCode, access_type,
};
use crate::expression::{ActiveCompilation, ExecutionResult};
use crate::parser::{
    DataIndex, Decode, FunctionIndex, GlobalIndex, LabelIndex, LocalIndex, MemoryIndex,
    ReferenceType, TableIndex, TagByte, TypeIndex, TypeInfo, ValueType,
};
use crate::read_tape::ReadTape;
use crate::runtime::memory_buffer::MemoryError;
use crate::runtime::{Trap, WasmContext, memory_buffer};
use crate::vector::{Index, WasmVec};
use anyhow::{Context, bail, ensure};
use std::convert::Infallible;
use std::io::Read;
use std::marker::PhantomData;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Not, Sub};

type Function = FunctionIndex;
type Label = LabelIndex;
type Type = TypeIndex;
type Table = TableIndex;
type Data = DataIndex;
type Labels = WasmVec<Label>;
type Local = LocalIndex;

type Global = GlobalIndex;
type NullByte = TagByte<0x00>;
type NullByte0 = NullByte;

type ShuffleArg = [Vector8x32Lane; 16];

macro_rules! ty_param_discard {
    ($_:ty) => {
        _
    };
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
    (
        ControlFlow {
            $(
                ($cf_name: literal, $cf_ident: ident) => $cf_opcode: literal $(($($cf_data: ident),*))?
            ),+ $(,)?
        }

        Untyped {
            $(
                ($parametric_name: literal, $parametric_ident: ident) => $parametric_opcode: literal $(($($parametric_data: ident),*))?
            ),+ $(,)?
        }

        Simple {
            $(
                $(const $(@$const:tt)?)? ($simple_name: literal, $simple_ident: ident) => $simple_opcode: literal $(-> $instr_variant: literal)? $(($($simple_data: ident),*))? code: $code:expr
            ),+ $(,)?
        }
    ) => {
        #[derive(Debug, PartialEq, Clone)]
        pub(super) enum Instruction {
            ControlFlow(ControlFlowInstruction),
            Simple(SimpleInstruction),
        }

        #[derive(Debug, PartialEq, Clone)]
        pub enum SimpleInstruction {
            Untyped(UntypedInstruction),
            Typed(TypedInstruction),
        }

        #[derive(Debug, PartialEq, Clone)]
        pub(super) enum UntypedInstruction {
            $($parametric_ident $(($($parametric_data),*))? ),+
        }

        #[derive(Debug, PartialEq, Clone)]
        pub(super) enum ControlFlowInstruction {
            $($cf_ident $(($($cf_data),*))? ),+
        }

        #[derive(Debug, PartialEq, Clone)]
        #[non_exhaustive]
        pub(super) enum TypedInstruction {
            $($simple_ident $(($($simple_data),*))? ),+
        }

        impl Decode for Instruction {
            #[inline(always)]
            fn decode(file: &mut ReadTape<impl Read>) -> anyhow::Result<Self> {
                let first_byte = file.read_byte()?;

                normalize_match_single!(match ((first_byte, )) {
                    {
                        $(
                            ($cf_opcode,) => {
                                 return Ok(Instruction::ControlFlow(
                                     ControlFlowInstruction::$cf_ident $(($(<$cf_data>::decode(file)?),*))?
                                 ))
                            },
                        )+
                        $(
                            ($parametric_opcode,) => {
                                 return Ok(Instruction::Simple(
                                     SimpleInstruction::Untyped(
                                         UntypedInstruction::$parametric_ident $(($(<$parametric_data>::decode(file)?),*))?
                                     )
                                 ))
                            },
                        )+
                        $(
                            ($simple_opcode, $($instr_variant)?) => {
                                 return Ok(Instruction::Simple(
                                     SimpleInstruction::Typed(
                                         TypedInstruction::$simple_ident $(($(<$simple_data>::decode(file)?),*))?
                                     )
                                 ))
                            },
                        )+

                    }
                    {_ => ()}
                });

                let second_opcode = file.read_leb128::<u32>()?;

                Ok(normalize_match_double!(match ((first_byte, second_opcode)) {
                    {$(
                        ($simple_opcode, $($instr_variant)?) => {
                             Instruction::Simple(SimpleInstruction::Typed(TypedInstruction::$simple_ident $(($(<$simple_data>::decode(file)?),*))?))
                        },
                    )+}
                    {_ => bail!("invalid instruction (0x{first_byte:02x}, {second_opcode})")}
                }))
            }
        }

        impl TypedInstruction {
            pub(super) fn const_available(&self) -> bool {
                macro_rules! is_const {
                    (const) => {true};
                    (     ) => {false};
                }

                match self {
                    $(Self::$simple_ident$(($(ty_param_discard!($simple_data)),*))? => is_const!($(const $($const)?)?)),+
                }
            }

            pub(super) fn simulate(&self, compiler: &mut ActiveCompilation) -> bool {
                #[allow(non_snake_case)]
                let ret = match self {
                    $(Self::$simple_ident$(($($simple_data),*))? => {
                        let primitive = const { $code };
                        #[allow(unused_parens)]
                        TypedInstructionCode::<($(($(&$simple_data),*))?)>::validate(primitive, ($(($($simple_data),*))?), compiler)
                    },)+
                };
                ret
            }

            pub(super) fn execute(&self, context: &mut WasmContext) -> ExecutionResult {
                #[allow(non_snake_case)]
                let ret = match self {
                    $(Self::$simple_ident$(($($simple_data),*))? => {
                        #[allow(unused_parens)]
                        TypedInstructionCode::<($(($(&$simple_data),*))?)>::call(const { $code }, ($(($($simple_data),*))?), context)
                    },)+
                };

                ret
            }

            pub(super) fn name(&self) -> &'static str {
                // Warn on duplicate name
                if false {
                    #[forbid(unreachable_patterns)]
                    match "" {
                        "" => (),
                        $($simple_name => (),)+
                        _ => ()
                    }
                }

                match self {
                    $(Self::$simple_ident$(($(ty_param_discard!($simple_data)),*))? => $simple_name,)+
                }
            }
        }
    };
}

macro_rules! vector_lane {
    (
        $bits: literal x $count: literal {
            $first: ident $( $rest:ident)*
        }
    ) => {
        paste::paste!{
            #[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
            #[repr(u8)]
            pub enum [<Vector $bits x $count Lane>] {
                $first =  0 $(, $rest)*
            }


            impl [<Vector $bits x $count Lane>] {
                #[inline(always)]
                pub fn try_from_byte(byte: u8) -> anyhow::Result<Self> {
                    const { assert!(<[Self]>::len(&[Self::$first $(, Self::$rest)*]) == $count) };

                    match byte {
                        // Safety:
                        // u8 in the range of 0..$count is guaranteed to be a valid layout of VectorLane
                        0..$count => Ok(unsafe { std::mem::transmute::<u8, Self>(byte) }),
                        _ => anyhow::bail!(concat!("vector lane must be between 0 and", stringify!($count)))
                    }
                }
            }

            impl Decode for [<Vector $bits x $count Lane>] {
                #[inline(always)]
                fn decode(file: &mut ReadTape<impl Read>) -> anyhow::Result<Self> {
                    file.read_byte().map_err(anyhow::Error::from).and_then([<Vector $bits x $count Lane>]::try_from_byte)
                }
            }

            impl<const N: usize> Decode for [ [<Vector $bits x $count Lane>] ; N] {
                fn decode(file: &mut ReadTape<impl Read>) -> anyhow::Result<Self> {
                    let chunk = file.read_chunk::<N>()?;
                    for byte in chunk {
                        // this generates much less bloated llvm IR
                        #[allow(clippy::question_mark)]
                        if let Err(err) = [<Vector $bits x $count Lane>]::try_from_byte(byte) {
                            return Err(err)
                        }
                    }

                    unsafe fn transmute_any<Src: Copy, Dst: Copy>(src: Src) -> Dst {
                        const { assert!(size_of::<Src>() == size_of::<Dst>()) }

                        union Transmute<Src: Copy, Dst: Copy> {
                            from: Src,
                            to: Dst
                        }

                        unsafe { Transmute { from: src }.to }
                    }

                    // Safety: all bytes are valid vector lanes
                    Ok(unsafe { transmute_any(chunk) })
                }
            }
        }
    };
}

vector_lane! {
    8 x 32 {
        _00 _01 _02 _03
        _04 _05 _06 _07
        _08 _09 _10 _11
        _12 _13 _14 _15
        _16 _17 _18 _19
        _20 _21 _22 _23
        _24 _25 _26 _27
        _28 _29 _30 _31
    }
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

macro_rules! load {
    ($ty: ident) => {
        MemoryAccess::<$ty, { access_type!(AccessType::Get) }>(PhantomData)
    };
    ($ty: ident <== $small:ident) => {
        CastingMemoryAccess::<$ty, $small, { access_type!(AccessType::Get) }>(PhantomData)
    };
}

macro_rules! store {
    ($ty: ident) => {
        MemoryAccess::<$ty, { access_type!(AccessType::Set) }>(PhantomData)
    };
    ($ty: ident ==> $small:ident) => {
        CastingMemoryAccess::<$ty, $small, { access_type!(AccessType::Set) }>(PhantomData)
    };
}

macro_rules! immediate {
    ($ty:ty) => {
        Primitive::ok(|x: &$ty, ()| *x)
    };
}

macro_rules! in_out {
    ($($in: ident: $ty: ty),*; $expr: expr) => {
        #[allow(unused_parens)]
        { Primitive::new(|(), ($($in),*): ($($ty),*)| { Ok($expr) }) }
    };
}

macro_rules! compare {
    ($($in: ident: $ty: ty),+; $expr: expr) => {
        in_out!($($in: $ty),+; {
            let res: bool = $expr;
            res as i32
        })
    };
}

macro_rules! eqz {
    ($ty:ty) => {
        compare!(a: $ty; a == 0)
    };
}

macro_rules! cmp {
    ($ty:ty; $cmp: tt) => {
        compare!(a: $ty, b: $ty; a $cmp b)
    };
}

macro_rules! bin_op {
    (wrapping (a: $ty1:ty, b: $ty2:ty); $name:ident) => { in_out!(a: $ty1, b: $ty2; paste::paste!{ <$ty1>::[<wrapping _ $name>](a, b) }) };
    ((a: $ty1:ty, b: $ty2:ty); $name:ident) => { in_out!(a: $ty1, b: $ty2; <$ty1>::$name(a, b)) };
    (wrapping <$ty:ty>::$name:ident) => { bin_op!(wrapping (a: $ty, b: $ty); $name) };
    (<$ty:ty>::$name:ident) => { bin_op!((a: $ty, b: $ty); $name) };
}

macro_rules! unop {
    (<$ty:ty>::$name:ident) => { in_out!(a: $ty; <$ty>::from(<$ty>::$name(a))) };
}

macro_rules! trap_if_zero {
    (<$ty: ty>::$op:ident) => {{
        in_out!(a: $ty, b: $ty;  {
            if b == 0 {
                return Err(Trap::divide_by_zero());
            }
            <$ty>::$op(a, b)
        })
    }};
}

macro_rules! modulo_n {
    (<$ty:ty>::$op: ident) => {{
        in_out!(a: $ty, b: $ty; {
            let b = (b & const { (<$ty>::BITS-1) as $ty }) as u32;
            <$ty>::$op(a, b)
        })
    }};
}

macro_rules! arithmetic {
    ($ty:ty; clz) => {
        unop!(<$ty>::leading_zeros)
    };
    ($ty:ty; ctz) => {
        unop!(<$ty>::trailing_zeros)
    };
    ($ty:ty; popcount) => {
        unop!(<$ty>::count_ones)
    };
    ($ty:ty; add) => {
        bin_op!(wrapping<$ty>::add)
    };
    ($ty:ty; sub) => {
        bin_op!(wrapping<$ty>::sub)
    };
    ($ty:ty; mul) => {
        bin_op!(wrapping<$ty>::mul)
    };
    ($ty:ty; div) => {
        trap_if_zero!(<$ty>::wrapping_div)
    };
    ($ty:ty; rem) => {
        trap_if_zero!(<$ty>::wrapping_rem)
    };
    ($ty:ty; neg) => {
        unop!(<$ty>::wrapping_neg)
    };
    ($ty:ty; not) => {
        unop!(<$ty>::not)
    };
    ($ty:ty; and) => {
        bin_op!(<$ty>::bitand)
    };
    ($ty:ty;  or) => {
        bin_op!(<$ty>::bitor)
    };
    ($ty:ty; xor) => {
        bin_op!(<$ty>::bitxor)
    };
    ($ty:ty; shr) => {
        modulo_n!(<$ty>::wrapping_shr)
    };
    ($ty:ty; shl) => {
        modulo_n!(<$ty>::wrapping_shl)
    };
    ($ty:ty; rotr) => {
        modulo_n!(<$ty>::rotate_right)
    };
    ($ty:ty; rotl) => {
        modulo_n!(<$ty>::rotate_left)
    };
    (float $ty:ty; abs) => {
        unop!(<$ty>::abs)
    };
    (float $ty:ty; neg) => {
        unop!(<$ty>::neg)
    };
    (float $ty:ty; ceil) => {
        unop!(<$ty>::ceil)
    };
    (float $ty:ty; floor) => {
        unop!(<$ty>::floor)
    };
    (float $ty:ty; trunc) => {
        unop!(<$ty>::trunc)
    };
    (float $ty:ty; nearest) => {
        unop!(<$ty>::round_ties_even)
    };
    (float $ty:ty; sqrt) => {
        unop!(<$ty>::sqrt)
    };
    (float $ty:ty; add) => {
        bin_op!(<$ty>::add)
    };
    (float $ty:ty; sub) => {
        bin_op!(<$ty>::sub)
    };
    (float $ty:ty; mul) => {
        bin_op!(<$ty>::mul)
    };
    (float $ty:ty; div) => {
        bin_op!(<$ty>::div)
    };
    (float $ty:ty; min) => {
        bin_op!(<$ty>::min)
    };
    (float $ty:ty; max) => {
        bin_op!(<$ty>::max)
    };
    (float $ty:ty; copysign) => {
        bin_op!(<$ty>::copysign)
    };
}

macro_rules! round_trip_cast {
    ($ty: ident <==> $small:ident) => {{
        const { assert!($small::BITS < $ty::BITS, "casting to bigger type and back does nothing") };
        in_out!(a: $ty; a as $small as $ty)
    }};
}

macro_rules! cast {
    ($ty1: ident ==> $ty2:ident) => {{
        in_out!(a: $ty1; a as $ty2)
    }};
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Optional<T>(pub Option<T>);

impl<T: Decode> Decode for Optional<T> {
    fn decode(file: &mut ReadTape<impl Read>) -> anyhow::Result<Self> {
        Ok(match u32::decode(file)? {
            0 => Self(None),
            1 => Self(Some(T::decode(file)?)),
            _ => bail!("too many items in optional segment"),
        })
    }
}

type OptionalValueType = Optional<ValueType>;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub(super) enum BlockType {
    Empty,
    Type(ValueType),
    // Explicitly stated as signed
    // https://webassembly.github.io/spec/core/binary/instructions.html#control-instructions
    TypeIndex(i32),
}

impl BlockType {
    pub(crate) fn resolve<'a>(
        &self,
        types: &'a WasmVec<TypeInfo>,
    ) -> anyhow::Result<(&'a [ValueType], &'a [ValueType])> {
        macro_rules! const_ref {
            ($ty:expr$(, ($($pat:tt)*))+ $(,)?) => {
                match $ty {
                    $($($pat)* => const { &[$($pat)*] })+
                }
            };
        }

        match *self {
            BlockType::Empty => Ok((&[], &[])),
            BlockType::Type(ty) => Ok((&[], {
                const_ref!(
                    ty,
                    (ValueType::I32),
                    (ValueType::I64),
                    (ValueType::F32),
                    (ValueType::F64),
                    (ValueType::V128),
                    (ValueType::Function),
                    (ValueType::Extern),
                )
            })),
            BlockType::TypeIndex(ty) => {
                let idx = Index(ty as u32);
                types
                    .get(idx)
                    .map(|info| (&*info.parameters, &*info.result))
                    .with_context(|| format!("unresolved type {}", idx.0))
            }
        }
    }
}

impl Decode for BlockType {
    fn decode(file: &mut ReadTape<impl Read>) -> anyhow::Result<Self> {
        let byte = file.peek_byte()?;
        if byte == 0x40 {
            file.read_byte()?;
            Ok(BlockType::Empty)
        } else if byte > 0x40 && byte < 0x80 {
            Ok(BlockType::Type(ValueType::decode(file)?))
        } else {
            Ok(BlockType::TypeIndex(i32::decode(file)?))
        }
    }
}

#[inline(always)]
fn splat32(v: u32) -> u128 {
    splat64(((v as u64) << 32) | v as u64)
}

#[inline(always)]
fn splat64(v: u64) -> u128 {
    ((v as u128) << 64) | v as u128
}

trait PrimitiveInt: Copy {
    const BITS: Index;
}

macro_rules! impl_bits {
    ($($ty: literal)*) => {
        paste::paste!{$(
            impl PrimitiveInt for [<u $ty>] {
                const BITS: Index = Index($ty);
            }

            type [<MemoryArgument $ty>] = MemoryArgument<[<u $ty>]>;
        )*}
    };
}

#[derive(Debug, PartialEq, Eq)]
pub struct MemoryArgument<T> {
    offset: Index,
    align: PhantomData<T>,
}

impl<T> Clone for MemoryArgument<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for MemoryArgument<T> {}

impl_bits! {
    8
    16
    32
    64
    128
}

impl<T: PrimitiveInt> Decode for MemoryArgument<T> {
    fn decode(file: &mut ReadTape<impl Read>) -> anyhow::Result<Self> {
        let align = Index::decode(file)?;
        let offset = Index::decode(file)?;
        ensure!(align < T::BITS, "invalid memory argument");
        Ok(MemoryArgument {
            offset,
            align: PhantomData,
        })
    }
}

impl<B> memory_buffer::MemoryArgument for MemoryArgument<B> {
    fn offset(self) -> Index {
        self.offset
    }

    fn align(self) -> usize {
        1
    }
}

instruction! {
    ControlFlow {
        ("block",          Block) => 0x02 (BlockType),
        ("loop",            Loop) => 0x03 (BlockType),
        ("if",                If) => 0x04 (BlockType),
        ("else",            Else) => 0x05,
        ("end",              End) => 0x0b,

        ("br",            Branch) => 0x0c (Label),
        ("br_if",       BranchIf) => 0x0d (Label),
        ("br_table", BranchTable) => 0x0e (Labels, Label),
        ("return",        Return) => 0x0f
    }

    Untyped {
        // Parametric
        ("drop",         Drop) => 0x1a,
        ("select",     Select) => 0x1b,
        ("select_t*", SelectT) => 0x1c (OptionalValueType),

        // Variable Instructions
        ("local.get",   LocalGet) => 0x20 (Local),
        ("local.set",   LocalSet) => 0x21 (Local),
        ("local.tee",   LocalTee) => 0x22 (Local),

        // Global Instructions
        ("global.get", GlobalGet) => 0x23 (Global),
        ("global.set", GlobalSet) => 0x24 (Global),
    }

    Simple {
        ("unreachable", Unreachable) => 0x00 code: Primitive::new(|(), ()| Err::<Infallible, _>(Trap::new())),
        ("nop",                 Nop) => 0x01 code: Primitive::ok(|(), ()| ()),


        ("call",                  Call) => 0x10 (Function) code: Call,
        ("call_indirect", CallIndirect) => 0x11 (Type, Table) code: Call,

        // Reference Instructions
        const ("ref.null",      RefNull) => 0xd0 (ReferenceType) code: RefNull, // FIXME heaptype
        const ("ref.is_null", RefIsNull) => 0xd1 code: compare!(func: Function; func == Function::NULL),
        const ("ref.func",      RefFunc) => 0xd2 (Function) code: immediate!(Function),

        // Memory Instructions

        // Primitive load
        ("i32.load",        I32Load) => 0x28 (MemoryArgument32) code: load!(u32),
        ("i64.load",        I64Load) => 0x29 (MemoryArgument64) code: load!(u64),
        ("f32.load",        F32Load) => 0x2a (MemoryArgument32) code: load!(f32),
        ("f64.load",        F64Load) => 0x2b (MemoryArgument64) code: load!(f64),

        // # Casting load

        // ## i32
        ("i32.load8_s",   I32LoadI8) => 0x2c (MemoryArgument8 ) code: load!(i32 <==  i8),
        ("i32.load8_u",   I32LoadU8) => 0x2d (MemoryArgument8 ) code: load!(i32 <==  u8),
        ("i32.load16_s", I32LoadI16) => 0x2e (MemoryArgument16) code: load!(i32 <== i16),
        ("i32.load16_u", I32LoadU16) => 0x2f (MemoryArgument16) code: load!(i32 <== u16),

        // ## i64
        ("i64.load8_s",   I64LoadI8) => 0x30 (MemoryArgument8 ) code: load!(i64 <==  i8),
        ("i64.load8_u",   I64LoadU8) => 0x31 (MemoryArgument8 ) code: load!(i64 <==  u8),
        ("i64.load16_s", I64LoadI16) => 0x32 (MemoryArgument16) code: load!(i64 <== i16),
        ("i64.load16_u", I64LoadU16) => 0x33 (MemoryArgument16) code: load!(i64 <== u16),
        ("i64.load32_s", I64LoadI32) => 0x34 (MemoryArgument32) code: load!(i64 <== i32),
        ("i64.load32_u", I64LoadU32) => 0x35 (MemoryArgument32) code: load!(i64 <== u32),

        // Primitive store
        ("i32.store",      I32Store) => 0x36 (MemoryArgument32) code: store!(i32),
        ("i64.store",      I64Store) => 0x37 (MemoryArgument64) code: store!(i64),
        ("f32.store",      F32Store) => 0x38 (MemoryArgument32) code: store!(f32),
        ("f64.store",      F64Store) => 0x39 (MemoryArgument64) code: store!(f64),

        // Casting store

        // ## i32
        ("i32.store8",   I32StoreI8) => 0x3a (MemoryArgument8 ) code: store!(i32 ==>  u8),
        ("i32.store16", I32StoreI16) => 0x3b (MemoryArgument16) code: store!(i32 ==> u16),


        // ## i64
        ("i64.store8",   I64StoreI8) => 0x3c (MemoryArgument8 ) code: store!(i64 ==>  u8),
        ("i64.store16", I64StoreI16) => 0x3d (MemoryArgument16) code: store!(i64 ==> u16),
        ("i64.store32", I64StoreI32) => 0x3e (MemoryArgument32) code: store!(i64 ==> u32),

        ("memory.size",  MemorySize) => 0x3f (NullByte) code: Primitive::full(|_, (), ctx| {
            Ok(ctx.mem_size(MemoryIndex::ZERO)?.0)
        }),
        ("memory.grow",  MemoryGrow) => 0x40 (NullByte) code: Primitive::full(|_, grow_by: u32, ctx| {
            match ctx.mem_grow(MemoryIndex::ZERO, Index(grow_by)) {
                Ok(sz) => Ok(sz.0),
                Err(MemoryError::MemoryFault(_)) => Err(Trap::new()),
                Err(MemoryError::OutOfMemory(_)) => Ok(u32::MAX),
            }
        }),

        ("memory.init", MemoryInit) => 0xFC -> 8 (Data, NullByte) code: MemoryInit,
        ("data.drop",     DataDrop) => 0xFC -> 9 (Data) code: Primitive::ok(|_, ()| ()), // TODO: reclaim data
        ("memory.copy", MemoryCopy) => 0xFC -> 10 (NullByte, NullByte0) code: {
            Primitive::full(|_, (dest, src, n): (Index, Index, Index), ctx| {
                ctx.mem_copy(MemoryIndex::ZERO, dest, src, n).map_err(Into::into)
            })
        },
        ("memory.fill", MemoryFill) => 0xFC -> 11 (NullByte)  code: {
            Primitive::full(|_, (mem_offset, val, n): (Index, u32, Index), ctx| {
                ctx.mem_fill(MemoryIndex::ZERO, mem_offset, val as u8, n).map_err(Into::into)
            })
        },


        // # Numeric Instructions

        // ## Constants

        /* instructions encode the constants as signed with zigzag encoding */
        const ("i32.const", I32Const) => 0x41 (i32) code: immediate!(i32),
        const ("i64.const", I64Const) => 0x42 (i64) code: immediate!(i64),
        const ("f32.const", F32Const) => 0x43 (f32) code: immediate!(f32),
        const ("f64.const", F64Const) => 0x44 (f64) code: immediate!(f64),

        // ## Equality

        // ### i32
        ("i32.eqz", I32EqZ) => 0x45 code: eqz!(u32),
        ("i32.eq",   I32Eq) => 0x46 code: cmp!(u32; ==),
        ("i32.ne",   I32Ne) => 0x47 code: cmp!(u32; !=),
        ("i32.lt_s", I32Lt) => 0x48 code: cmp!(i32; <),
        ("i32.lt_u", U32Lt) => 0x49 code: cmp!(u32; <),
        ("i32.gt_s", I32Gt) => 0x4a code: cmp!(i32; >),
        ("i32.gt_u", U32Gt) => 0x4b code: cmp!(u32; >),
        ("i32.le_s", I32Le) => 0x4c code: cmp!(i32; <=),
        ("i32.le_u", U32Le) => 0x4d code: cmp!(u32; <=),
        ("i32.ge_s", I32Ge) => 0x4e code: cmp!(i32; >=),
        ("i32.ge_u", U32Ge) => 0x4f code: cmp!(u32; >=),

        // ### i64
        ("i64.eqz", I64EqZ) => 0x50 code: eqz!(u64),
        ("i64.eq",   I64Eq) => 0x51 code: cmp!(u64; ==),
        ("i64.ne",   I64Ne) => 0x52 code: cmp!(u64; !=),
        ("i64.lt_s", I64Lt) => 0x53 code: cmp!(i64;  <),
        ("i64.lt_u", U64Lt) => 0x54 code: cmp!(u64;  <),
        ("i64.gt_s", I64Gt) => 0x55 code: cmp!(i64;  >),
        ("i64.gt_u", U64Gt) => 0x56 code: cmp!(u64;  >),
        ("i64.le_s", I64Le) => 0x57 code: cmp!(i64; <=),
        ("i64.le_u", U64Le) => 0x58 code: cmp!(u64; <=),
        ("i64.ge_s", I64Ge) => 0x59 code: cmp!(i64; >=),
        ("i64.ge_u", U64Ge) => 0x5a code: cmp!(u64; >=),

        // ### f32
        ("f32.eq", F32Eq) => 0x5b code: cmp!(f32; ==),
        ("f32.ne", F32Ne) => 0x5c code: cmp!(f32; !=),
        ("f32.lt", F32Lt) => 0x5d code: cmp!(f32;  <),
        ("f32.gt", F32Gt) => 0x5e code: cmp!(f32;  >),
        ("f32.le", F32Le) => 0x5f code: cmp!(f32; <=),
        ("f32.ge", F32Ge) => 0x60 code: cmp!(f32; >=),

        // ### f64
        ("f64.eq", F64Eq) => 0x61 code: cmp!(f64; ==),
        ("f64.ne", F64Ne) => 0x62 code: cmp!(f64; !=),
        ("f64.lt", F64Lt) => 0x63 code: cmp!(f64; <),
        ("f64.gt", F64Gt) => 0x64 code: cmp!(f64; >),
        ("f64.le", F64Le) => 0x65 code: cmp!(f64; <=),
        ("f64.ge", F64Ge) => 0x66 code: cmp!(f64; >=),


        // ## Arithmetic

        // ### i32

        ("i32.clz",       I32Clz) => 0x67 code: arithmetic!(u32; clz),
        ("i32.ctz",       I32Ctz) => 0x68 code: arithmetic!(u32; ctz),
        ("i32.popcnt", I32Popcnt) => 0x69 code: arithmetic!(u32; popcount),
        ("i32.add",       I32Add) => 0x6a code: arithmetic!(u32;  add),
        ("i32.sub",       I32Sub) => 0x6b code: arithmetic!(u32;  sub),
        ("i32.mul",       I32Mul) => 0x6c code: arithmetic!(u32;  mul),
        ("i32.div_s",     I32Div) => 0x6d code: arithmetic!(i32;  div),
        ("i32.div_u",     U32Div) => 0x6e code: arithmetic!(u32;  div),
        ("i32.rem_s",     I32Rem) => 0x6f code: arithmetic!(i32;  rem),
        ("i32.rem_u",     U32Rem) => 0x70 code: arithmetic!(u32;  rem),
        ("i32.and",       I32And) => 0x71 code: arithmetic!(u32;  and),
        ("i32.or",         I32Or) => 0x72 code: arithmetic!(u32;  or),
        ("i32.xor",       I32Xor) => 0x73 code: arithmetic!(u32;  xor),
        ("i32.shl",       I32Shl) => 0x74 code: arithmetic!(u32;  shl),
        ("i32.shr_s",     I32Shr) => 0x75 code: arithmetic!(i32;  shr),
        ("i32.shr_u",    I32Shru) => 0x76 code: arithmetic!(u32;  shr),
        ("i32.rotl",     I32Rotl) => 0x77 code: arithmetic!(u32; rotl),
        ("i32.rotr",     I32Rotr) => 0x78 code: arithmetic!(u32; rotr),

        // ### i64
        ("i64.clz",       I64Clz) => 0x79 code: arithmetic!(u64; clz),
        ("i64.ctz",       I64Ctz) => 0x7a code: arithmetic!(u64; ctz),
        ("i64.popcnt", I64Popcnt) => 0x7b code: arithmetic!(u64; popcount),
        ("i64.add",       I64Add) => 0x7c code: arithmetic!(u64;  add),
        ("i64.sub",       I64Sub) => 0x7d code: arithmetic!(u64;  sub),
        ("i64.mul",       I64Mul) => 0x7e code: arithmetic!(u64;  mul),
        ("i64.div_s",     I64Div) => 0x7f code: arithmetic!(i64;  div),
        ("i64.div_u",     U64Div) => 0x80 code: arithmetic!(u64;  div),
        ("i64.rem_s",     I64Rem) => 0x81 code: arithmetic!(i64;  rem),
        ("i64.rem_u",     U64Rem) => 0x82 code: arithmetic!(u64;  rem),
        ("i64.and",       I64And) => 0x83 code: arithmetic!(u64;  and),
        ("i64.or",         I64Or) => 0x84 code: arithmetic!(u64;  or),
        ("i64.xor",       I64Xor) => 0x85 code: arithmetic!(u64;  xor),
        ("i64.shl",       I64Shl) => 0x86 code: arithmetic!(u64;  shl),
        ("i64.shr_s",     I64Shr) => 0x87 code: arithmetic!(i64;  shr),
        ("i64.shr_u",    I64Shru) => 0x88 code: arithmetic!(u64;  shr),
        ("i64.rotl",     I64Rotl) => 0x89 code: arithmetic!(u64; rotl),
        ("i64.rotr",     I64Rotr) => 0x8a code: arithmetic!(u64; rotr),

        // ### f32
        ("f32.abs",           F32Abs) => 0x8b code: arithmetic!(float f32; abs),
        ("f32.neg",           F32Neg) => 0x8c code: arithmetic!(float f32; neg),
        ("f32.ceil",         F32Ceil) => 0x8d code: arithmetic!(float f32; ceil),
        ("f32.floor",       F32Floor) => 0x8e code: arithmetic!(float f32; floor),
        ("f32.trunc",       F32Trunc) => 0x8f code: arithmetic!(float f32; trunc),
        ("f32.nearest",   F32Nearest) => 0x90 code: arithmetic!(float f32; nearest),
        ("f32.sqrt",         F32Sqrt) => 0x91 code: arithmetic!(float f32; sqrt),
        ("f32.add",           F32Add) => 0x92 code: arithmetic!(float f32; add),
        ("f32.sub",           F32Sub) => 0x93 code: arithmetic!(float f32; sub),
        ("f32.mul",           F32Mul) => 0x94 code: arithmetic!(float f32; mul),
        ("f32.div",           F32Div) => 0x95 code: arithmetic!(float f32; div),
        ("f32.min",           F32Min) => 0x96 code: arithmetic!(float f32; min),
        ("f32.max",           F32Max) => 0x97 code: arithmetic!(float f32; max),
        ("f32.copysign", F32CopySign) => 0x98 code: arithmetic!(float f32; copysign),

        // ### f64
        ("f64.abs",           F64Abs) => 0x99 code: arithmetic!(float f64; abs),
        ("f64.neg",           F64Neg) => 0x9a code: arithmetic!(float f64; neg),
        ("f64.ceil",         F64Ceil) => 0x9b code: arithmetic!(float f64; ceil),
        ("f64.floor",       F64Floor) => 0x9c code: arithmetic!(float f64; floor),
        ("f64.trunc",       F64Trunc) => 0x9d code: arithmetic!(float f64; trunc),
        ("f64.nearest",   F64Nearest) => 0x9e code: arithmetic!(float f64; nearest),
        ("f64.sqrt",         F64Sqrt) => 0x9f code: arithmetic!(float f64; sqrt),
        ("f64.add",           F64Add) => 0xa0 code: arithmetic!(float f64; add),
        ("f64.sub",           F64Sub) => 0xa1 code: arithmetic!(float f64; sub),
        ("f64.mul",           F64Mul) => 0xa2 code: arithmetic!(float f64; mul),
        ("f64.div",           F64Div) => 0xa3 code: arithmetic!(float f64; div),
        ("f64.min",           F64Min) => 0xa4 code: arithmetic!(float f64; min),
        ("f64.max",           F64Max) => 0xa5 code: arithmetic!(float f64; max),
        ("f64.copysign", F64CopySign) => 0xa6 code: arithmetic!(float f64; copysign),


        // Conversion
        ("i32.wrap_i64",               I32WrapI64) => 0xa7 code: cast!(u64 ==> u32),


        ("i32.trunc_f32_s",           I32TruncF32) => 0xa8 code: cast!(f32 ==> i32),
        ("i32.trunc_f32_u",           U32TruncF32) => 0xa9 code: cast!(f32 ==> u32),
        ("i32.trunc_f64_s",           I32TruncF64) => 0xaa code: cast!(f64 ==> i32),
        ("i32.trunc_f64_u",           U32TruncF64) => 0xab code: cast!(f64 ==> u32),

        ("i64.extend_i32_s",        I64PromoteI32) => 0xac code: cast!(i32 ==> i64),
        ("i64.extend_i32_u",        I64PromoteU32) => 0xad code: cast!(u32 ==> i64),

        ("i64.trunc_f32_s",           I64TruncF32) => 0xae code: cast!(f32 ==> i64),
        ("i64.trunc_f32_u",           U64TruncF32) => 0xaf code: cast!(f32 ==> u64),
        ("i64.trunc_f64_s",           I64TruncF64) => 0xb0 code: cast!(f64 ==> i64),
        ("i64.trunc_f64_u",           U64TruncF64) => 0xb1 code: cast!(f64 ==> u64),

        ("f32.convert_i32_s",       F32ConvertI32) => 0xb2 code: cast!(i32 ==> f32),
        ("f32.convert_i32_u",       F32ConvertU32) => 0xb3 code: cast!(u32 ==> f32),
        ("f32.convert_i64_s",       F32ConvertI64) => 0xb4 code: cast!(i64 ==> f32),
        ("f32.convert_i64_u",       F32ConvertU64) => 0xb5 code: cast!(u64 ==> f32),

        ("f32.demote_f64",           F32DemoteF64) => 0xb6 code: cast!(f64 ==> f32),

        ("f64.convert_i32_s",       F64ConvertI32) => 0xb7 code: cast!(i32 ==> f64),
        ("f64.convert_i32_u",       F64ConvertU32) => 0xb8 code: cast!(u32 ==> f64),
        ("f64.convert_i64_s",       F64ConvertI64) => 0xb9 code: cast!(i64 ==> f64),
        ("f64.convert_i64_u",       F64ConvertU64) => 0xba code: cast!(u64 ==> f64),

        ("f64.promote_f32",         F64PromoteF32) => 0xbb code: cast!(f32 ==> f64),

        ("i32.reinterpret_f32", I32ReinterpretF32) => 0xbc code: in_out!(a: f32; f32::to_bits(a)),
        ("i64.reinterpret_f64", I64ReinterpretF64) => 0xbd code: in_out!(a: f64; f64::to_bits(a)),
        ("f32.reinterpret_i32", F32ReinterpretI32) => 0xbe code: in_out!(a: u32; f32::from_bits(a)),
        ("f64.reinterpret_i64", F64ReinterpretI64) => 0xbf code: in_out!(a: u64; f64::from_bits(a)),


        // ## Sign extension

        ("i32.extend8_s",   I32ExtendI8) => 0xC0 code: round_trip_cast!(i32 <==>  i8),
        ("i32.extend16_s", I32ExtendI16) => 0xC1 code: round_trip_cast!(i32 <==> i16),
        ("i64.extend8_s",   I64ExtendI8) => 0xC2 code: round_trip_cast!(i64 <==> i16),
        ("i64.extend16_s", I64ExtendI16) => 0xC3 code: round_trip_cast!(i64 <==> i16),
        ("i64.extend32_s", I64ExtendI32) => 0xC4 code: round_trip_cast!(i64 <==> i32),

        // ## Saturating Conversion

        // these casts in rust, already define the edge cases
        // https://doc.rust-lang.org/reference/expressions/operator-expr.html#type-cast-expressions
        ("i32.trunc_sat_f32_s", I32TruncSatF32) => 0xfc -> 0 code: cast!(f32 ==> i32),
        ("i32.trunc_sat_f32_u", U32TruncSatF32) => 0xfc -> 1 code: cast!(f32 ==> u32),
        ("i32.trunc_sat_f64_s", I32TruncSatF64) => 0xfc -> 2 code: cast!(f64 ==> i32),
        ("i32.trunc_sat_f64_u", U32TruncSatF64) => 0xfc -> 3 code: cast!(f64 ==> u32),

        ("i64.trunc_sat_f32_s", I64TruncSatF32) => 0xfc -> 4 code: cast!(f32 ==> i64),
        ("i64.trunc_sat_f32_u", U64TruncSatF32) => 0xfc -> 5 code: cast!(f32 ==> u64),
        ("i64.trunc_sat_f64_s", I64TruncSatF64) => 0xfc -> 6 code: cast!(f64 ==> i64),
        ("i64.trunc_sat_f64_u", U64TruncSatF64) => 0xfc -> 7 code: cast!(f64 ==> u64),

        // Vector Instructions

        // ## Vector Memory Instructions
        ("v128.load",                 V128Load) => 0xfd ->  0 (MemoryArgument128) code: load!(i128),
        // ("v128.load8x8_s",        V128LoadI8x8) => 0xfd ->  1 (MemoryArgument),
        // ("v128.load8x8_u",        V128LoadU8x8) => 0xfd ->  2 (MemoryArgument),
        // ("v128.load16x4_s",      V128LoadI16x4) => 0xfd ->  3 (MemoryArgument),
        // ("v128.load16x4_u",      V128LoadU16x4) => 0xfd ->  4 (MemoryArgument),
        // ("v128.load32x2_s",      V128LoadI32x4) => 0xfd ->  5 (MemoryArgument),
        // ("v128.load32x2_u",      V128LoadU32x4) => 0xfd ->  6 (MemoryArgument),
        // ("v128.load8_splat",   V128LoadI8Splat) => 0xfd ->  7 (MemoryArgument),
        // ("v128.load16_splat", V128LoadI16Splat) => 0xfd ->  8 (MemoryArgument),
        // ("v128.load32_splat", V128LoadI32Splat) => 0xfd ->  9 (MemoryArgument),
        // ("v128.load64_splat", V128LoadI64Splat) => 0xfd -> 10 (MemoryArgument),
        // ("v128.load32_zero",   V128LoadI32Zero) => 0xfd -> 92 (MemoryArgument),
        // ("v128.load64_zero",   V128LoadI64Zero) => 0xfd -> 93 (MemoryArgument),
        // ("v128.store",               V128Store) => 0xfd -> 11 (MemoryArgument),
        // ("v128.load8_lane",      V128Load8Lane) => 0xfd -> 84 (MemoryArgument, Vector8x16Lane),
        // ("v128.load16_lane",    V128Load16Lane) => 0xfd -> 85 (MemoryArgument, Vector16x8Lane),
        // ("v128.load32_lane",    V128Load32Lane) => 0xfd -> 86 (MemoryArgument, Vector32x4Lane),
        // ("v128.load64_lane",    V128Load64Lane) => 0xfd -> 87 (MemoryArgument, Vector64x2Lane),
        // ("v128.store8_lane",    V128Store8Lane) => 0xfd -> 88 (MemoryArgument, Vector8x16Lane),
        // ("v128.store16_lane",  V128Store16Lane) => 0xfd -> 89 (MemoryArgument, Vector16x8Lane),
        // ("v128.store32_lane",  V128Store32Lane) => 0xfd -> 90 (MemoryArgument, Vector32x4Lane),
        // ("v128.store64_lane",  V128Store64Lane) => 0xfd -> 91 (MemoryArgument, Vector64x2Lane),

        // ## Vector Constant
        const ("v128.const", V128Const) => 0xfd -> 12 (u128) code: immediate!(u128),

        // ## Vector Shuffle
        ("i8x16.shuffle", V128Shuffle) => 0xfd -> 13 (ShuffleArg) code: Primitive::ok(|shuffle: &ShuffleArg, (v1, v2): (u128, u128)| {
            let mut combined = [0u8; 32];
            combined[..16].copy_from_slice(&v1.to_le_bytes());
            combined[16..].copy_from_slice(&v2.to_le_bytes());

            let mut result = [0u8; 16];
            for (i, &lane) in shuffle.iter().enumerate() {
                result[i] = combined[lane as usize];
            }

            u128::from_le_bytes(result)
        }),

        // // ## Vector Lane Manipulation
        // ("i8x16.extract_lane_s", I8x16ExtractLane) => 0xfd -> 21 (Vector8x16Lane),
        // ("i8x16.extract_lane_u", U8x16ExtractLane) => 0xfd -> 22 (Vector8x16Lane),
        // ("i8x16.replace_lane",   I8x16ReplaceLane) => 0xfd -> 23 (Vector8x16Lane),
        // ("i16x8.extract_lane_s", I16x8ExtractLane) => 0xfd -> 24 (Vector16x8Lane),
        // ("i16x8.extract_lane_u", U16x8ExtractLane) => 0xfd -> 25 (Vector16x8Lane),
        // ("i16x8.replace_lane",   I16x8ReplaceLane) => 0xfd -> 26 (Vector16x8Lane),
        // ("i32x4.extract_lane",   I32x4ExtractLane) => 0xfd -> 27 (Vector32x4Lane),
        // ("i32x4.replace_lane",   I32x4ReplaceLane) => 0xfd -> 28 (Vector32x4Lane),
        // ("i64x2.extract_lane",   I64x2ExtractLane) => 0xfd -> 29 (Vector64x2Lane),
        // ("i64x2.replace_lane",   I64x2ReplaceLane) => 0xfd -> 30 (Vector64x2Lane),
        // ("f32x4.extract_lane",   F32x4ExtractLane) => 0xfd -> 31 (Vector32x4Lane),
        // ("f32x4.replace_lane",   F32x4ReplaceLane) => 0xfd -> 32 (Vector32x4Lane),
        // ("f64x2.extract_lane",   F64x2ExtractLane) => 0xfd -> 33 (Vector64x2Lane),
        // ("f64x2.replace_lane",   F64x2ReplaceLane) => 0xfd -> 34 (Vector64x2Lane),


        ("i8x16.swizzle", I8x16Swizzle) => 0xfd -> 14 code: Primitive::ok(|(), (v1, v2): (u128, u128)| {
            let mut result = [0u8; 16];
            let bytes = v1.to_le_bytes();
            let indices = v2.to_le_bytes();

            for i in 0..16 {
                let idx = indices[i] as usize;
                result[i] = if idx < 16 { bytes[idx] } else { 0 };
            }

            u128::from_le_bytes(result)
        }),

        ("i8x16.splat", I8x16Splat) => 0xfd -> 15 code: Primitive::ok(|(), v: u32| u128::from_le_bytes([v as u8; 16])),
        ("i16x8.splat", I16x8Splat) => 0xfd -> 16 code: Primitive::ok(|(), v: u32| {
            let v = v as u16;
            splat32(((v as u32) << 16) | v as u32)
        }),
        ("i32x4.splat", I32x4Splat) => 0xfd -> 17 code: Primitive::ok(|(), v: u32| splat32(v)),
        ("i64x2.splat", I64x2Splat) => 0xfd -> 18 code: Primitive::ok(|(), v: u64| splat64(v)),
        ("f32x4.splat", F32x4Splat) => 0xfd -> 19 code: Primitive::ok(|(), v: f32| splat32(v.to_bits())),
        ("f64x2.splat", F64x2Splat) => 0xfd -> 20 code: Primitive::ok(|(), v: f64| splat64(v.to_bits())),
        //
        //
        // // ## Equality checks
        //
        // // ### i8
        // ("i8x16.eq",   I8x16Eq) => 0xfd -> 35,
        // ("i8x16.ne",   I8x16Ne) => 0xfd -> 36,
        // ("i8x16.lt_s", I8x16Lt) => 0xfd -> 37,
        // ("i8x16.lt_u", U8x16Lt) => 0xfd -> 38,
        // ("i8x16.gt_s", I8x16Gt) => 0xfd -> 39,
        // ("i8x16.gt_u", U8x16Gt) => 0xfd -> 40,
        // ("i8x16.le_s", I8x16Le) => 0xfd -> 41,
        // ("i8x16.le_u", U8x16Le) => 0xfd -> 42,
        // ("i8x16.ge_s", I8x16Ge) => 0xfd -> 43,
        // ("i8x16.ge_u", U8x16Ge) => 0xfd -> 44,
        //
        // // ### i16
        // ("i16x8.eq",   I16x8Eq) => 0xfd -> 45,
        // ("i16x8.ne",   I16x8Ne) => 0xfd -> 46,
        // ("i16x8.lt_s", I16x8Lt) => 0xfd -> 47,
        // ("i16x8.lt_u", U16x8Lt) => 0xfd -> 48,
        // ("i16x8.gt_s", I16x8Gt) => 0xfd -> 49,
        // ("i16x8.gt_u", U16x8Gt) => 0xfd -> 50,
        // ("i16x8.le_s", I16x8Le) => 0xfd -> 51,
        // ("i16x8.le_u", U16x8Le) => 0xfd -> 52,
        // ("i16x8.ge_s", I16x8Ge) => 0xfd -> 53,
        // ("i16x8.ge_u", U16x8Ge) => 0xfd -> 54,
        //
        // // ### i32
        // ("i32x4.eq",   I32x4Eq) => 0xfd -> 55,
        // ("i32x4.ne",   I32x4Ne) => 0xfd -> 56,
        // ("i32x4.lt_s", I32x4Lt) => 0xfd -> 57,
        // ("i32x4.lt_u", U32x4Lt) => 0xfd -> 58,
        // ("i32x4.gt_s", I32x4Gt) => 0xfd -> 59,
        // ("i32x4.gt_u", U32x4Gt) => 0xfd -> 60,
        // ("i32x4.le_s", I32x4Le) => 0xfd -> 61,
        // ("i32x4.le_u", U32x4Le) => 0xfd -> 62,
        // ("i32x4.ge_s", I32x4Ge) => 0xfd -> 63,
        // ("i32x4.ge_u", U32x4Ge) => 0xfd -> 64,
        //
        // // ### i64
        // ("i64x2.eq",   I64x2Eq) => 0xfd -> 214,
        // ("i64x2.ne",   I64x2Ne) => 0xfd -> 215,
        // ("i64x2.lt_s", I64x2Lt) => 0xfd -> 216,
        // ("i64x2.lt_u", U64x2Lt) => 0xfd -> 217,
        // ("i64x2.gt_s", I64x2Gt) => 0xfd -> 218,
        // ("i64x2.gt_u", U64x2Gt) => 0xfd -> 219,
        // ("i64x2.le_s", I64x2Le) => 0xfd -> 220,
        // ("i64x2.le_u", U64x2Le) => 0xfd -> 221,
        // ("i64x2.ge_s", I64x2Ge) => 0xfd -> 222,
        // ("i64x2.ge_u", U64x2Ge) => 0xfd -> 223,
        //
        // // ### f32
        // ("f32x4.eq", F32x4Eq) => 0xfd -> 65,
        // ("f32x4.ne", F32x4Ne) => 0xfd -> 66,
        // ("f32x4.lt", F32x4Lt) => 0xfd -> 67,
        // ("f32x4.gt", F32x4Gt) => 0xfd -> 68,
        // ("f32x4.le", F32x4Le) => 0xfd -> 69,
        // ("f32x4.ge", F32x4Ge) => 0xfd -> 70,
        //
        // // ### f64
        // ("f64x2.eq", F64x2Eq) => 0xfd -> 71,
        // ("f64x2.ne", F64x2Ne) => 0xfd -> 72,
        // ("f64x2.lt", F64x2Lt) => 0xfd -> 73,
        // ("f64x2.gt", F64x2Gt) => 0xfd -> 74,
        // ("f64x2.le", F64x2Le) => 0xfd -> 75,
        // ("f64x2.ge", F64x2Ge) => 0xfd -> 76,
        //
        // ## Bitwise operations

        // ### v128
        ("v128.not",             V128Not) => 0xfd -> 77 code: arithmetic!(i128; not),
        ("v128.and",             V128And) => 0xfd -> 78 code: arithmetic!(i128; and),
        ("v128.andnot",       V128AndNot) => 0xfd -> 79 code: in_out!(a: i128, b: i128; (a & !b)),
        ("v128.or",               V128Or) => 0xfd -> 80 code: arithmetic!(i128; or),
        ("v128.xor",             V128Xor) => 0xfd -> 81 code: arithmetic!(i128; xor),
        ("v128.bitselect", V128BitSelect) => 0xfd -> 82 code: in_out!(a: i128, b: i128, c: i128; (c & b) | (!c & a)),
        ("v128.any_true",    V128AnyTrue) => 0xfd -> 83 code: compare!(a: i128; a != 0),


        // ## Numeric operations

        // // ### i8
        // ("i8x16.abs",                    I8x16Abs) => 0xfd ->  96,
        // ("i8x16.neg",                    I8x16Neg) => 0xfd ->  97,
        // ("i8x16.popcnt",            I8x16PopCount) => 0xfd ->  98,
        // ("i8x16.all_true",           I8x16AllTrue) => 0xfd ->  99,
        // ("i8x16.bitmask",            I8x16Bitmask) => 0xfd -> 100,
        // ("i8x16.narrow_i16x8_s", I8x16NarrowI16x8) => 0xfd -> 101,
        // ("i8x16.narrow_i16x8_u", U8x16NarrowU16x8) => 0xfd -> 102,
        // ("i8x16.shl",                    I8x16Shl) => 0xfd -> 107,
        // ("i8x16.shr_s",                  I8x16Shr) => 0xfd -> 108,
        // ("i8x16.shr_u",                  U8x16Shr) => 0xfd -> 109,
        // ("i8x16.add",                    I8x16Add) => 0xfd -> 110,
        // ("i8x16.add_sat_s",           I8x16AddSat) => 0xfd -> 111,
        // ("i8x16.add_sat_u",           U8x16AddSat) => 0xfd -> 112,
        // ("i8x16.sub",                    I8x16Sub) => 0xfd -> 113,
        // ("i8x16.sub_sat_s",           I8x16SubSat) => 0xfd -> 114,
        // ("i8x16.sub_sat_u",           U8x16SubSat) => 0xfd -> 115,
        // ("i8x16.min_s",                  I8x16Min) => 0xfd -> 118,
        // ("i8x16.min_u",                  U8x16Min) => 0xfd -> 119,
        // ("i8x16.max_s",                  I8x16Max) => 0xfd -> 120,
        // ("i8x16.max_u",                  U8x16Max) => 0xfd -> 121,
        // ("i8x16.avgr_u",                U8x16Avgr) => 0xfd -> 123,
    }
}
