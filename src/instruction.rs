use crate::parser::{
    BlockType, Decode, Expression, ExternIndex, FunctionIndex, GlobalIndex, IfElseBlock,
    LabelIndex, LocalIndex, MemoryArgument, MemoryIndex, ReferenceType, TableIndex, TagByte,
    TypeIndex, ValueType,
};
use crate::read_tape::ReadTape;
use crate::runtime::{
    MemoryError, MemoryFault, ReferenceValue, Validator, Value, ValueInner, WasmContext,
};
use crate::vector::{Index, WasmVec};
use crate::{Stack, invalid_data};
use bytemuck::Pod;
use std::convert::Infallible;
use std::io::{Read, Result};
use std::marker::PhantomData;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Not, Sub};

type Extern = ExternIndex;
type Function = FunctionIndex;
type Label = LabelIndex;
type Type = TypeIndex;
type Table = TableIndex;
type Labels = WasmVec<Label>;
type Local = LocalIndex;

type Global = GlobalIndex;
type NullByte = TagByte<0x00>;

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
    ($(
        ($name: literal, $ident: ident) => $opcode: literal $(-> $u32_code: literal)? $(($($data: ident),*))? code: $code:expr
    ),+ $(,)?) => {
        #[derive(Debug, PartialEq, Clone)]
        #[non_exhaustive]
        pub enum Instruction {
            $($ident $(($($data),*))? ),+
        }

        impl Decode for Instruction {
            fn decode(file: &mut ReadTape<impl Read>) -> Result<Self> {
                let first_byte = file.read_byte()?;

                normalize_match_single!(match ((first_byte, )) {
                    {$(
                        ($opcode, $($u32_code)?) => {
                             return Ok(Self::$ident $(($(<$data>::decode(file)?),*))?)
                        },
                    )+}
                    {_ => ()}
                });

                let second_opcode = u32::decode(file)?;

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
            pub fn validate(&self, validator: &mut Validator) -> bool {
                #[allow(non_snake_case)]
                match self {
                    $(Self::$ident$(($($data),*))? => {
                        let primitive = &const { $code };
                        #[allow(unused_parens)]
                        InstructionCode::<($(($(&$data),*))?)>::validate(primitive, ($(($($data),*))?), validator)
                    },)+
                }
            }

            pub fn execute(&self, context: &mut WasmContext) -> ExecutionResult {
                #[allow(non_snake_case)]
                match self {
                    $(Self::$ident$(($($data),*))? => {
                        let primitive = &const { $code };
                        #[allow(unused_parens)]
                        InstructionCode::<($(($(&$data),*))?)>::call(primitive, ($(($($data),*))?), context)
                    },)+
                }
            }

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
        to: Dst,
    }

    unsafe { Transmute { from: src }.to }
}

pub enum ExecutionError {
    Unwind(Label),
    Trap,
}

impl From<MemoryFault> for ExecutionError {
    fn from(_: MemoryFault) -> Self {
        ExecutionError::Trap
    }
}

pub type ExecutionResult<T = ()> = std::result::Result<T, ExecutionError>;

struct Primitive<Data, In, Out, F> {
    f: F,
    data: PhantomData<fn(Data, In, &mut WasmContext) -> Out>,
}

impl<Data, In, Out, F: Fn(Data, In) -> Out> Primitive<Data, In, Out, F> {
    pub const fn ok(
        f: F,
    ) -> Primitive<Data, In, Out, impl Fn(Data, In, &mut WasmContext) -> ExecutionResult<Out>> {
        Primitive::new(move |data, input| Ok(f(data, input)))
    }
}

impl<Data, In, Out, F: Fn(Data, In) -> ExecutionResult<Out>> Primitive<Data, In, Out, F> {
    pub const fn new(
        f: F,
    ) -> Primitive<Data, In, Out, impl Fn(Data, In, &mut WasmContext) -> ExecutionResult<Out>> {
        Primitive::full(move |data, input, _| f(data, input))
    }
}

impl<Data, In, Out, F: Fn(Data, In, &mut WasmContext) -> ExecutionResult<Out>>
    Primitive<Data, In, Out, F>
{
    pub const fn full(f: F) -> Self {
        Self {
            f,
            data: PhantomData,
        }
    }
}

pub trait Param: Sized + 'static {
    fn pop_validation_input(validator: &mut Validator) -> bool;
    fn push_validation_output(validator: &mut Validator);
    fn pop_checked(context: &mut Vec<Value>) -> Option<Self>;
    fn pop(context: &mut Vec<Value>) -> Self {
        Self::pop_checked(context).expect("invalid validation step")
    }
    fn push(self, context: &mut Vec<Value>);
}

macro_rules! impl_param_for_tuple {
    ($(($($T:literal),+ $(,)?))+) => {paste::paste! {
        $(
        impl<$([<T $T>]: ValueInner),*> Param for ($([<T $T>]),+,) {
            fn pop_validation_input(validator: &mut Validator) -> bool {
                validator.pop_n().is_some_and(|arr| arr == [ $([<T $T>]::r#type()),*])
            }

            fn push_validation_output(validator: &mut Validator) {
                validator.push_n([$([<T $T>]::r#type()),+]);
            }

            fn pop_checked(context: &mut Vec<Value>) -> Option<Self> {
                let [$( [<t $T>] ),+] = context.pop_n()?;
                Some(($([<T $T>]::from([<t $T>]).unwrap()),+,))
            }

            fn push(self, context: &mut Vec<Value>) {
                context.push_n([$(self.$T.into()),+]);
            }
        }
        )+
    }};
}

impl Param for () {
    fn pop_validation_input(_: &mut Validator) -> bool {
        true
    }
    fn push_validation_output(_: &mut Validator) {}
    fn pop_checked(_: &mut Vec<Value>) -> Option<Self> {
        Some(())
    }
    fn pop(_: &mut Vec<Value>) -> Self {}
    fn push(self, _: &mut Vec<Value>) {}
}

impl Param for Infallible {
    fn pop_validation_input(_: &mut Validator) -> bool {
        unreachable!()
    }

    fn push_validation_output(validator: &mut Validator) {
        validator.set_unreachable()
    }

    fn pop_checked(_: &mut Vec<Value>) -> Option<Self> {
        unreachable!()
    }
    fn pop(_: &mut Vec<Value>) -> Self {
        unreachable!()
    }

    fn push(self, _: &mut Vec<Value>) {
        match self {}
    }
}

impl<T: ValueInner> Param for T {
    fn pop_validation_input(stack: &mut Validator) -> bool {
        stack.pop().is_some_and(|t| t == T::r#type())
    }

    fn push_validation_output(stack: &mut Validator) {
        stack.push(T::r#type())
    }

    fn pop_checked(context: &mut Vec<Value>) -> Option<Self> {
        context.pop().and_then(T::from)
    }

    fn push(self, context: &mut Vec<Value>) {
        context.push(self.into())
    }
}

impl_param_for_tuple! {
    (0, 1)
    (0, 1, 2)
    (0, 1, 2, 3)
    (0, 1, 2, 3, 4)
    (0, 1, 2, 3, 4, 5)
    (0, 1, 2, 3, 4, 5, 6)
    (0, 1, 2, 3, 4, 5, 6, 7)
    (0, 1, 2, 3, 4, 5, 6, 7, 8)
}

pub trait InstructionCode<Data> {
    fn validate(&self, data: Data, validator: &mut Validator) -> bool;
    fn call(&self, data: Data, context: &mut WasmContext) -> ExecutionResult;
}

impl<Data, In: Param, Out: Param, F: Fn(Data, In, &mut WasmContext) -> ExecutionResult<Out>>
    InstructionCode<Data> for Primitive<Data, In, Out, F>
{
    fn validate(&self, _: Data, validator: &mut Validator) -> bool {
        let res = In::pop_validation_input(validator);
        if res {
            Out::push_validation_output(validator)
        }
        res
    }

    fn call(&self, data: Data, context: &mut WasmContext) -> ExecutionResult {
        let input = In::pop(context.stack);
        (self.f)(data, input, context).map(|x| Out::push(x, context.stack))
    }
}

macro_rules! flag {
    ($name: ident { $flag_name: ident }; true = $truthy: ident; false = $falsy: ident) => {
        trait $name {
            const $flag_name: bool;
        }
        struct $truthy;
        struct $falsy;
        impl $name for $truthy {
            const $flag_name: bool = true;
        }

        impl $name for $falsy {
            const $flag_name: bool = false;
        }
    };
}

flag!(BlockBranchBehavior { LOOP_BACK }; true = Loop; false = Break);

struct Block<T>(PhantomData<T>);

impl<T: BlockBranchBehavior> InstructionCode<(&BlockType, &Expression)> for Block<T> {
    fn validate(
        &self,
        (r#type, expr): (&BlockType, &Expression),
        validator: &mut Validator,
    ) -> bool {
        // TODO: validate blocks
        let (input, out) = match *r#type {
            BlockType::Empty => (&[][..], &[][..]),
            BlockType::Type(ref out) => (&[][..], std::slice::from_ref(out)),
            BlockType::TypeIndex(ty) => {
                let (Some(r#in), Some(out)) =
                    (validator.get_type_input(ty), validator.get_type_output(ty))
                else {
                    return false;
                };
                (r#in, out)
            }
        };

        let ends_with = |validator: &mut Validator, needle: &[ValueType]| {
            if needle.is_empty() {
                return true;
            }
            validator
                .stack()
                .iter()
                .rev()
                .take(needle.len())
                .copied()
                .eq(needle.iter().copied())
        };

        ends_with(validator, input)
            && expr
                .instructions
                .iter()
                .all(|inst| inst.validate(validator))
            && ends_with(validator, out)
    }

    fn call(
        &self,
        (_, expr): (&BlockType, &Expression),
        context: &mut WasmContext,
    ) -> ExecutionResult {
        'block: loop {
            for instruction in expr.instructions.iter() {
                match instruction.execute(context) {
                    Ok(()) => (),
                    Err(ExecutionError::Trap) => return Err(ExecutionError::Trap),
                    Err(ExecutionError::Unwind(LabelIndex(Index(0)))) => match T::LOOP_BACK {
                        true => continue 'block,
                        false => break 'block Ok(()),
                    },
                    Err(ExecutionError::Unwind(LabelIndex(Index(up @ 1..)))) => {
                        return Err(ExecutionError::Unwind(LabelIndex(Index(up - 1))));
                    }
                }
            }

            return Ok(());
        }
    }
}

flag!(BranchBehavior { CONDITIONAL }; true = Conditional; false = Unconditional);

struct IfBlock;

impl InstructionCode<(&BlockType, &IfElseBlock)> for IfBlock {
    fn validate(&self, _: (&BlockType, &IfElseBlock), validator: &mut Validator) -> bool {
        u32::pop_validation_input(validator)
    }

    fn call(
        &self,
        (_, expr): (&BlockType, &IfElseBlock),
        context: &mut WasmContext,
    ) -> ExecutionResult {
        let (if_so, if_not) = match expr {
            IfElseBlock::If(expr) => (&*expr.instructions, &[][..]),
            IfElseBlock::IfElse(if_so, if_not) => (&*if_so.instructions, &*if_not.instructions),
        };

        let instr = match u32::pop(context.stack) {
            0 => if_not,
            _ => if_so,
        };

        for instruction in instr {
            match instruction.execute(context) {
                Ok(()) => (),
                Err(ExecutionError::Trap) => return Err(ExecutionError::Trap),
                Err(ExecutionError::Unwind(LabelIndex(Index(0)))) => break,
                Err(ExecutionError::Unwind(LabelIndex(Index(up @ 1..)))) => {
                    return Err(ExecutionError::Unwind(LabelIndex(Index(up - 1))));
                }
            }
        }

        Ok(())
    }
}

struct Branch<T>(PhantomData<T>);

impl<T: BranchBehavior> InstructionCode<&Label> for Branch<T> {
    fn validate(&self, &label: &Label, validator: &mut Validator) -> bool {
        let has_label = validator.contains_label(label);
        let contains_condition = !T::CONDITIONAL || i32::pop_validation_input(validator);
        has_label && contains_condition
    }

    fn call(&self, &label: &Label, context: &mut WasmContext) -> ExecutionResult {
        if T::CONDITIONAL && i32::pop(context.stack) == 0 {
            return Ok(());
        }

        Err(ExecutionError::Unwind(label))
    }
}

struct BranchTable;

impl InstructionCode<(&Labels, &Label)> for BranchTable {
    fn validate(&self, (labels, fallback): (&Labels, &Label), validator: &mut Validator) -> bool {
        fallback.0 < labels.len_idx() && i32::pop_validation_input(validator)
    }

    fn call(
        &self,
        (labels, fallback): (&Labels, &Label),
        context: &mut WasmContext,
    ) -> ExecutionResult {
        let idx = Index(u32::pop(context.stack));
        Err(ExecutionError::Unwind(match labels.get(idx) {
            Some(&label) => label,
            None => *labels.get(fallback.0).unwrap(),
        }))
    }
}

struct Drop;
impl InstructionCode<()> for Drop {
    fn validate(&self, (): (), validator: &mut Validator) -> bool {
        validator.pop().is_some()
    }

    fn call(&self, (): (), context: &mut WasmContext) -> ExecutionResult {
        let _ = context.pop().unwrap();
        Ok(())
    }
}

struct Select;
impl InstructionCode<()> for Select {
    fn validate(&self, (): (), validator: &mut Validator) -> bool {
        i32::pop_validation_input(validator) && validator.pop_n().is_some_and(|[a, b]| a == b)
    }

    fn call(&self, (): (), context: &mut WasmContext) -> ExecutionResult {
        let [val2, val1] = context.pop_n().unwrap();
        let value = match i32::pop(context.stack) {
            0 => val2,
            _ => val1,
        };
        context.push(value);
        Ok(())
    }
}

struct RefNull;

impl InstructionCode<&ReferenceType> for RefNull {
    fn validate(&self, ref_ty: &ReferenceType, validator: &mut Validator) -> bool {
        match ref_ty {
            ReferenceType::Function => Function::push_validation_output(validator),
            ReferenceType::Extern => Extern::push_validation_output(validator),
        }
        true
    }

    fn call(&self, ref_ty: &ReferenceType, context: &mut WasmContext) -> ExecutionResult {
        match ref_ty {
            ReferenceType::Function => {
                context.push(Value::Ref(ReferenceValue::Function(Function::NULL)))
            }
            ReferenceType::Extern => context.push(Value::Ref(ReferenceValue::Extern(Extern::NULL))),
        }
        Ok(())
    }
}

struct Call;

impl InstructionCode<&Function> for Call {
    fn validate(&self, &func: &Function, validator: &mut Validator) -> bool {
        validator.simulate_call(func)
    }

    fn call(&self, func: &Function, context: &mut WasmContext) -> ExecutionResult {
        match context.call(*func) {
            Ok(()) => Ok(()),
            Err(()) => Err(ExecutionError::Trap),
        }
    }
}

impl InstructionCode<(&Table, &Type)> for Call {
    fn validate(&self, _: (&Table, &Type), _: &mut Validator) -> bool {
        todo!()
    }

    fn call(
        &self,
        (&table_idx, &type_idx): (&Table, &Type),
        context: &mut WasmContext,
    ) -> ExecutionResult {
        let idx = Index(u32::pop(context.stack));
        let ReferenceValue::Function(func_idx) = context.table_load(table_idx, idx).unwrap() else {
            unreachable!();
        };
        self.call(&func_idx, context)
    }
}

trait VariableIndex: Copy {
    fn exists(self, validator: &mut Validator) -> bool;
    fn mutable(self, validator: &mut Validator) -> bool;
    fn r#type(self, validator: &mut Validator) -> ValueType;
    fn load(self, context: &mut WasmContext) -> Value;
    fn store(self, value: Value, context: &mut WasmContext);
}

impl VariableIndex for Local {
    fn exists(self, validator: &mut Validator) -> bool {
        validator.get_local(self).is_some()
    }

    fn mutable(self, _: &mut Validator) -> bool {
        true
    }

    fn r#type(self, validator: &mut Validator) -> ValueType {
        // `exists` should always run before `type`
        validator.get_local(self).unwrap()
    }

    fn load(self, context: &mut WasmContext) -> Value {
        // due to validation, we exist
        *context.get_local(self).unwrap()
    }

    fn store(self, value: Value, context: &mut WasmContext) {
        // due to validation, we exist
        *context.get_local(self).unwrap() = value
    }
}

impl VariableIndex for Global {
    fn exists(self, validator: &mut Validator) -> bool {
        validator.get_global(self).is_some()
    }

    fn mutable(self, validator: &mut Validator) -> bool {
        // `exists` should always run before `mutable`
        validator.get_global(self).unwrap().mutable
    }

    fn r#type(self, validator: &mut Validator) -> ValueType {
        // `exists` should always run before `type`
        validator.get_global(self).unwrap().value_type
    }

    fn load(self, context: &mut WasmContext) -> Value {
        // due to validation, we exist
        context.load_global(self).unwrap()
    }

    fn store(self, value: Value, context: &mut WasmContext) {
        // due to validation, we exist and are mutable
        context.store_global(self, value).unwrap()
    }
}

enum AccessType {
    Get = 0,
    Set = 1,
    Tee = 2,
}

macro_rules! access_type {
    (@fetch $lit:expr) => {
        const {
            match $lit {
                0 => AccessType::Get,
                1 => AccessType::Set,
                2 => AccessType::Tee,
                _ => unreachable!(),
            }
        }
    };
    ($lit:expr) => {
        const { $lit as usize }
    };
}

struct VariableAccess<I, const A: usize>(PhantomData<[I; A]>);

impl<I: VariableIndex, const A: usize> InstructionCode<&I> for VariableAccess<I, A> {
    fn validate(&self, &index: &I, validator: &mut Validator) -> bool {
        let exists = index.exists(validator);
        let mut has_valid_signature = move || {
            let index_ty = index.r#type(validator);
            let valid_stack = match access_type!(@fetch A) {
                AccessType::Get => {
                    validator.push(index_ty);
                    return true;
                }
                AccessType::Set => validator.pop().is_some_and(|ty| ty == index_ty),
                AccessType::Tee => validator.peek().is_some_and(|&ty| ty == index_ty),
            };

            valid_stack && index.mutable(validator)
        };

        exists && has_valid_signature()
    }

    fn call(&self, &index: &I, context: &mut WasmContext) -> ExecutionResult {
        match access_type!(@fetch A) {
            AccessType::Get => {
                let value = index.load(context);
                context.push(value);
            }
            AccessType::Set => {
                let value = context.pop().unwrap();
                index.store(value, context)
            }
            AccessType::Tee => {
                let value = *context.peek().unwrap();
                index.store(value, context)
            }
        }
        Ok(())
    }
}

struct MemoryAccess<T, const A: usize>(PhantomData<[T; A]>);

impl<T: ValueInner, const A: usize> InstructionCode<&MemoryArgument> for MemoryAccess<T, A> {
    fn validate(&self, &_: &MemoryArgument, validator: &mut Validator) -> bool {
        match access_type!(@fetch A) {
            AccessType::Get => {
                let res = u32::pop_validation_input(validator);
                if res {
                    T::push_validation_output(validator)
                };
                res
            }
            AccessType::Set => {
                T::pop_validation_input(validator) && u32::pop_validation_input(validator)
            }
            AccessType::Tee => {
                let res =
                    T::pop_validation_input(validator) && u32::pop_validation_input(validator);
                if res {
                    T::push_validation_output(validator)
                }
                res
            }
        }
    }

    fn call(&self, &mem_arg: &MemoryArgument, context: &mut WasmContext) -> ExecutionResult {
        match access_type!(@fetch A) {
            AccessType::Get => {
                let index = Index(u32::pop(context.stack));
                let value = context.mem_load::<T>(MemoryIndex::ZERO, mem_arg, index)?;
                context.push(value.into());
            }
            AccessType::Set => {
                let value = context.pop().and_then(T::from).unwrap();
                let index = Index(u32::pop(context.stack));
                context.mem_store::<T>(MemoryIndex::ZERO, mem_arg, index, &value)?;
            }
            AccessType::Tee => {
                let index = Index(u32::pop(context.stack));
                let value = context.peek().and_then(T::from_ref).unwrap();
                context.mem_store::<T>(MemoryIndex::ZERO, mem_arg, index, value)?;
            }
        }
        Ok(())
    }
}

trait Extend<T: ValueInner>: Pod {
    fn extend(from: Self) -> T;
    fn narrow(from: T) -> Self;
}

struct CastingMemoryAccess<T, E, const A: usize>(PhantomData<[(T, E); A]>);

macro_rules! impl_extend {
    ($(Extend<$ty:ty> for $small:ty)*) => {$(
        // https://doc.rust-lang.org/reference/expressions/operator-expr.html#type-cast-expressions
        // this works perfectly as long as we just extend from smaller
        // because to smaller always truncates
        // and smaller to bigger depends on the smaller type
        const _: () = assert!(<$small>::BITS < <$ty>::BITS);

        impl Extend<$ty> for $small {
            fn extend(from: Self) -> $ty {
                from as $ty
            }

            fn narrow(from: $ty) -> Self {
                from as $small
            }
        }
    )*};
    ($($ty:ty)*) => {impl_extend!{
        $(Extend<i32> for $ty Extend<i64> for $ty)*
    }};
}

impl_extend!(i8 u8 i16 u16);
impl_extend!(Extend<i64> for i32 Extend<i64> for u32);

impl<T: ValueInner, E: Extend<T>, const A: usize> InstructionCode<&MemoryArgument>
    for CastingMemoryAccess<T, E, A>
{
    #[inline(always)]
    fn validate(&self, mem_arg: &MemoryArgument, validator: &mut Validator) -> bool {
        MemoryAccess::<T, A>::validate(&MemoryAccess(PhantomData), mem_arg, validator)
    }

    fn call(&self, &mem_arg: &MemoryArgument, context: &mut WasmContext) -> ExecutionResult {
        match access_type!(@fetch A) {
            AccessType::Get => {
                let index = Index(u32::pop(context.stack));
                let value = context.mem_load::<E>(MemoryIndex::ZERO, mem_arg, index)?;
                context.push(E::extend(value).into());
            }
            AccessType::Set => {
                let value = context.pop().and_then(T::from).map(E::narrow).unwrap();
                let index = Index(u32::pop(context.stack));
                context.mem_store::<E>(MemoryIndex::ZERO, mem_arg, index, &value)?;
            }
            AccessType::Tee => unreachable!(),
        }
        Ok(())
    }
}

macro_rules! vector_lane {
    (
        $bits: literal x $count: literal {
            $first: ident $( $rest:ident)*
        }
    ) => {
        const _: () = assert!(128 == ($count * $bits) && ($count as u8).is_power_of_two() && ($bits as u8).is_power_of_two());
        paste::paste!{
            #[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
            #[repr(u8)]
            pub enum [<Vector $bits x $count Lane>] {
                $first =  0 $(, $rest)*
            }


            impl [<Vector $bits x $count Lane>] {
                #[inline(always)]
                pub fn try_from_byte(byte: u8) -> Result<Self> {
                    const { assert!(<[Self]>::len(&[Self::$first $(, Self::$rest)*]) == $count) };

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
                        // this generates much less bloated llvm IR
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
    (<$ty:ty>::$name:ident) => { in_out!(a: $ty; <$ty>::$name(a)) };
}

macro_rules! trap_if_zero {
    ($a: ident, $b: ident, $op: expr) => {{
        if $b == 0 {
            return Err(ExecutionError::Trap);
        }
        $op($a, $b)
    }};
}

macro_rules! arithmetic {
    ($ty:ty; clz) => { unop!(<$ty>::leading_zeros) };
    ($ty:ty; ctz) => { unop!(<$ty>::trailing_zeros) };
    ($ty:ty; popcount) => { unop!(<$ty>::count_ones) };
    ($ty:ty; add) => { bin_op!(wrapping <$ty>::add) };
    ($ty:ty; sub) => { bin_op!(wrapping <$ty>::sub) };
    ($ty:ty; mul) => { bin_op!(wrapping <$ty>::mul) };
    ($ty:ty; div) => { in_out!(b: $ty, a: $ty; trap_if_zero!(a, b, <$ty>::wrapping_div)) };
    ($ty:ty; rem) => { in_out!(b: $ty, a: $ty; trap_if_zero!(a, b, <$ty>::wrapping_rem)) };
    ($ty:ty; neg) => { unop!(<$ty>::wrapping_neg) };
    ($ty:ty; not) => { unop!(<$ty>::not) };
    ($ty:ty; and) => { bin_op!(<$ty>::bitand) };
    ($ty:ty;  or) => { bin_op!(<$ty>::bitor ) };
    ($ty:ty; xor) => { bin_op!(<$ty>::bitxor) };
    ($ty:ty; shr) => { bin_op!(wrapping (a: $ty, b: u32); shr) };
    ($ty:ty; shl) => { bin_op!(wrapping (a: $ty, b: u32); shl) };
    ($ty:ty; rotr) => { bin_op!((a: $ty, b: u32); rotate_right) };
    ($ty:ty; rotl) => { bin_op!((a: $ty, b: u32); rotate_left) };
    (float $ty:ty; abs) => { unop!(<$ty>::abs) };
    (float $ty:ty; neg) => { unop!(<$ty>::neg) };
    (float $ty:ty; ceil) => { unop!(<$ty>::ceil) };
    (float $ty:ty; floor) => { unop!(<$ty>::floor) };
    (float $ty:ty; trunc) => { unop!(<$ty>::trunc) };
    (float $ty:ty; nearest) => { unop!(<$ty>::round_ties_even) };
    (float $ty:ty; sqrt) => { unop!(<$ty>::sqrt) };
    (float $ty:ty; add) => { bin_op!(<$ty>::add) };
    (float $ty:ty; sub) => { bin_op!(<$ty>::sub) };
    (float $ty:ty; mul) => { bin_op!(<$ty>::mul) };
    (float $ty:ty; div) => { bin_op!(<$ty>::div) };
    (float $ty:ty; min) => { bin_op!(<$ty>::min) };
    (float $ty:ty; max) => { bin_op!(<$ty>::max) };
    (float $ty:ty; copysign) => { bin_op!(<$ty>::copysign) };
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

instruction! {
    // Control Instructions
    ("unreachable",    Unreachable) => 0x00 code: Primitive::new(|(), ()| Err::<Infallible, _>(ExecutionError::Trap)),
    ("nop",                    Nop) => 0x01 code: in_out!(; ()),
    ("block",                Block) => 0x02 (BlockType, Expression) code: Block(PhantomData::<Break>),
    ("loop",                  Loop) => 0x03 (BlockType, Expression) code: Block(PhantomData::<Loop >),
    ("if",                     If)  => 0x04 (BlockType, IfElseBlock) code: IfBlock,
    ("br",                  Branch) => 0x0c (Label) code: Branch(PhantomData::<Unconditional>),
    ("br_if",             BranchIf) => 0x0d (Label) code: Branch(PhantomData::<Conditional>),
    ("br_table",       BranchTable) => 0x0e (Labels, Label) code: BranchTable,
    ("return",              Return) => 0x0f code: Primitive::new(|(), ()| Err::<Infallible, _>(ExecutionError::Unwind(Label::MAX))),
    ("call",                  Call) => 0x10 (Function) code: Call,
    ("call_indirect", CallIndirect) => 0x11 (Table, Type) code: Call,

    // Reference Instructions
    ("ref.null",      RefNull) => 0xd0 (ReferenceType) code: RefNull,
    ("ref.is_null", RefIsNull) => 0xd1 code: compare!(func: Function; func == Function::MAX),
    ("ref.func",      RefFunc) => 0xd2 (Function) code: immediate!(Function),


    // // Parametric Instructions
    ("drop",         Drop) => 0x1a code: Drop,
    ("select",     Select) => 0x1b code: Select,
    // ("select_t*", SelectT) => 0x1c (WasmVec<ValueType>),
    //
    // Variable Instructions
    ("local.get",   LocalGet) => 0x20 ( Local) code: VariableAccess::<Local,  { access_type!(AccessType::Get) }>(PhantomData),
    ("local.set",   LocalSet) => 0x21 ( Local) code: VariableAccess::<Local,  { access_type!(AccessType::Set) }>(PhantomData),
    ("local.tee",   LocalTee) => 0x22 ( Local) code: VariableAccess::<Local,  { access_type!(AccessType::Tee) }>(PhantomData),
    ("global.get", GlobalGet) => 0x23 (Global) code: VariableAccess::<Global, { access_type!(AccessType::Get) }>(PhantomData),
    ("global.set", GlobalSet) => 0x24 (Global) code: VariableAccess::<Global, { access_type!(AccessType::Set) }>(PhantomData),

    // Memory Instructions

    // Primitive load
    ("i32.load",        I32Load) => 0x28 (MemoryArgument) code: load!(i32),
    ("i64.load",        I64Load) => 0x29 (MemoryArgument) code: load!(i64),
    ("f32.load",        F32Load) => 0x2a (MemoryArgument) code: load!(f32),
    ("f64.load",        F64Load) => 0x2b (MemoryArgument) code: load!(f64),

    // Casting load
    ("i32.load8_s",   I32LoadI8) => 0x2c (MemoryArgument) code: load!(i32 <==  i8),
    ("i32.load8_u",   I32LoadU8) => 0x2d (MemoryArgument) code: load!(i32 <==  u8),
    ("i32.load16_s", I32LoadI16) => 0x2e (MemoryArgument) code: load!(i32 <== i16),
    ("i32.load16_u", I32LoadU16) => 0x2f (MemoryArgument) code: load!(i32 <== i16),
    ("i64.load8_s",   I64LoadI8) => 0x30 (MemoryArgument) code: load!(i64 <==  i8),
    ("i64.load8_u",   I64LoadU8) => 0x31 (MemoryArgument) code: load!(i64 <==  u8),
    ("i64.load16_s", I64LoadI16) => 0x32 (MemoryArgument) code: load!(i64 <== i16),
    ("i64.load16_u", I64LoadU16) => 0x33 (MemoryArgument) code: load!(i64 <== u16),
    ("i64.load32_s", I64LoadI32) => 0x34 (MemoryArgument) code: load!(i64 <== i32),
    ("i64.load32_u", I64LoadU32) => 0x35 (MemoryArgument) code: load!(i64 <== u32),

    // Primitive store
    ("i32.store",      I32Store) => 0x36 (MemoryArgument) code: store!(i32),
    ("i64.store",      I64Store) => 0x37 (MemoryArgument) code: store!(i64),
    ("f32.store",      F32Store) => 0x38 (MemoryArgument) code: store!(f32),
    ("f64.store",      F64Store) => 0x39 (MemoryArgument) code: store!(f64),

    // Casting store
    ("i32.store8",   I32StoreI8) => 0x3a (MemoryArgument) code: store!(i32 ==>  i8),
    ("i32.store16", I32StoreI16) => 0x3b (MemoryArgument) code: store!(i32 ==> i16),
    ("i64.store8",   I64StoreI8) => 0x3c (MemoryArgument) code: store!(i64 ==>  i8),
    ("i64.store16", I64StoreI16) => 0x3d (MemoryArgument) code: store!(i64 ==> i16),
    ("i64.store32", I64StoreI32) => 0x3e (MemoryArgument) code: store!(i64 ==> i32),

    ("memory.size",  MemorySize) => 0x3f (NullByte) code: Primitive::full(|_, (), ctx| {
        Ok(ctx.mem_size(MemoryIndex::ZERO)?.0)
    }),
    ("memory.grow",  MemoryGrow) => 0x40 (NullByte) code: Primitive::full(|_, grow_by: u32, ctx| {
        match ctx.mem_grow(MemoryIndex::ZERO, Index(grow_by)) {
            Ok(sz) => Ok(sz.0),
            Err(MemoryError::MemoryFault(_)) => Err(ExecutionError::Trap),
            Err(MemoryError::OutOfMemory(_)) => Ok(u32::MAX),
        }
    }),

    // ("memory.init", MemoryInit) => 0xFC -> 8 (DataIndex, NullByte),
    // ("data.drop",     DataDrop) => 0xFC -> 9 (DataIndex),
    // ("memory.copy", MemoryCopy) => 0xFC -> 10 (NullByte, NullByte),
    // ("memory.fill", MemoryFill) => 0xFC -> 11 (NullByte),


    // # Numeric Instructions

    // ## Constants
    ("i32.const", I32Const) => 0x41 (i32) code: immediate!(i32),
    ("i64.const", I64Const) => 0x42 (i64) code: immediate!(i64),
    ("f32.const", F32Const) => 0x43 (f32) code: immediate!(f32),
    ("f64.const", F64Const) => 0x44 (f64) code: immediate!(f64),

    // ## Equality

    // ### i32
    ("i32.eqz", I32EqZ) => 0x45 code: eqz!(i32),
    ("i32.eq",   I32Eq) => 0x46 code: cmp!(i32; ==),
    ("i32.ne",   I32Ne) => 0x47 code: cmp!(i32; !=),
    ("i32.lt_s", I32Lt) => 0x48 code: cmp!(i32; <),
    ("i32.lt_u", U32Lt) => 0x49 code: cmp!(u32; <),
    ("i32.gt_s", I32Gt) => 0x4a code: cmp!(i32; >),
    ("i32.gt_u", U32Gt) => 0x4b code: cmp!(u32; >),
    ("i32.le_s", I32Le) => 0x4c code: cmp!(i32; <=),
    ("i32.le_u", U32Le) => 0x4d code: cmp!(u32; <=),
    ("i32.ge_s", I32Ge) => 0x4e code: cmp!(i32; >=),
    ("i32.ge_u", U32Ge) => 0x4f code: cmp!(u32; >=),

    // ### i64
    ("i64.eqz", I64EqZ) => 0x50 code: eqz!(i64),
    ("i64.eq",   I64Eq) => 0x51 code: cmp!(i64; ==),
    ("i64.ne",   I64Ne) => 0x52 code: cmp!(i64; !=),
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
    ("f64.eq", F64Eq) => 0x61 code: cmp!(f32; ==),
    ("f64.ne", F64Ne) => 0x62 code: cmp!(f32; !=),
    ("f64.lt", F64Lt) => 0x63 code: cmp!(f32; <),
    ("f64.gt", F64Gt) => 0x64 code: cmp!(f32; >),
    ("f64.le", F64Le) => 0x65 code: cmp!(f32; <=),
    ("f64.ge", F64Ge) => 0x66 code: cmp!(f32; >=),


    // ## Arithmetic

    // ### i32

    ("i32.clz",       I32Clz) => 0x67 code: arithmetic!(i32; clz),
    ("i32.ctz",       I32Ctz) => 0x68 code: arithmetic!(i32; ctz),
    ("i32.popcnt", I32Popcnt) => 0x69 code: arithmetic!(i32; popcount),
    ("i32.add",       I32Add) => 0x6a code: arithmetic!(i32;  add),
    ("i32.sub",       I32Sub) => 0x6b code: arithmetic!(i32;  sub),
    ("i32.mul",       I32Mul) => 0x6c code: arithmetic!(i32;  mul),
    ("i32.div_s",     I32Div) => 0x6d code: arithmetic!(i32;  div),
    ("i32.div_u",     U32Div) => 0x6e code: arithmetic!(u32;  div),
    ("i32.rem_s",     I32Rem) => 0x6f code: arithmetic!(i32;  rem),
    ("i32.rem_u",     U32Rem) => 0x70 code: arithmetic!(u32;  rem),
    ("i32.and",       I32And) => 0x71 code: arithmetic!(i32;  and),
    ("i32.or",         I32Or) => 0x72 code: arithmetic!(i32;  or),
    ("i32.xor",       I32Xor) => 0x73 code: arithmetic!(i32;  xor),
    ("i32.shl",       I32Shl) => 0x74 code: arithmetic!(i32;  shl),
    ("i32.shr_s",     I32Shr) => 0x75 code: arithmetic!(i32;  shr),
    ("i32.shr_u",    I32Shru) => 0x76 code: arithmetic!(u32;  shr),
    ("i32.rotl",     I32Rotl) => 0x77 code: arithmetic!(i32; rotl),
    ("i32.rotr",     I32Rotr) => 0x78 code: arithmetic!(i32; rotr),

    // ### i64
    ("i64.clz",       I64Clz) => 0x79 code: arithmetic!(i64; clz),
    ("i64.ctz",       I64Ctz) => 0x7a code: arithmetic!(i64; ctz),
    ("i64.popcnt", I64Popcnt) => 0x7b code: arithmetic!(i64; popcount),
    ("i64.add",       I64Add) => 0x7c code: arithmetic!(i64;  add),
    ("i64.sub",       I64Sub) => 0x7d code: arithmetic!(i64;  sub),
    ("i64.mul",       I64Mul) => 0x7e code: arithmetic!(i64;  mul),
    ("i64.div_s",     I64Div) => 0x7f code: arithmetic!(i64;  div),
    ("i64.div_u",     U64Div) => 0x80 code: arithmetic!(u64;  div),
    ("i64.rem_s",     I64Rem) => 0x81 code: arithmetic!(i64;  rem),
    ("i64.rem_u",     U64Rem) => 0x82 code: arithmetic!(u64;  rem),
    ("i64.and",       I64And) => 0x83 code: arithmetic!(i64;  and),
    ("i64.or",         I64Or) => 0x84 code: arithmetic!(i64;  or),
    ("i64.xor",       I64Xor) => 0x85 code: arithmetic!(i64;  xor),
    ("i64.shl",       I64Shl) => 0x86 code: arithmetic!(i64;  shl),
    ("i64.shr_s",     I64Shr) => 0x87 code: arithmetic!(i64;  shr),
    ("i64.shr_u",    I64Shru) => 0x88 code: arithmetic!(u64;  shr),
    ("i64.rotl",     I64Rotl) => 0x89 code: arithmetic!(i64; rotl),
    ("i64.rotr",     I64Rotr) => 0x8a code: arithmetic!(i64; rotr),

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
    ("i32.wrap_i64",               I32WrapI64) => 0xa7 code: round_trip_cast!(u64 <==> u32),

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
    // ("i32.trunc_sat_f32_s", I32TruncSatF32) => 0xfc -> 0,
    // ("i32.trunc_sat_f32_u", U32TruncSatF32) => 0xfc -> 1,
    // ("i32.trunc_sat_f64_s", I32TruncSatF64) => 0xfc -> 2,
    // ("i32.trunc_sat_f64_u", U32TruncSatF64) => 0xfc -> 3,
    // ("i64.trunc_sat_f32_s", I64TruncSatF32) => 0xfc -> 4,
    // ("i64.trunc_sat_f32_u", U64TruncSatF32) => 0xfc -> 5,
    // ("i64.trunc_sat_f64_s", I64TruncSatF64) => 0xfc -> 6,
    // ("i64.trunc_sat_f64_u", U64TruncSatF64) => 0xfc -> 7,
    //
    // Vector Instructions

    // ## Vector Memory Instructions
    ("v128.load",                 V128Load) => 0xfd ->  0 (MemoryArgument) code: load!(i128),
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
    ("v128.const", V128Const) => 0xfd -> 12 (u128) code: immediate!(u128),
    //
    // // ## Vector Shuffle
    // ("v128.shuffle", V128Shuffle) => 0xfd -> 13 ([Vector8x16Lane; 16]),
    //
    // // ## Vector Lane Manipulation
    // ("i8x16.extract_lane_s", I8x16ExtractLane) => 0xfd -> 21 ([Vector8x16Lane; 16]),
    // ("i8x16.extract_lane_u", U8x16ExtractLane) => 0xfd -> 22 ([Vector8x16Lane; 16]),
    // ("i8x16.replace_lane",   I8x16ReplaceLane) => 0xfd -> 23 ([Vector8x16Lane; 16]),
    // ("i16x8.extract_lane_s", I16x8ExtractLane) => 0xfd -> 24 ([Vector16x8Lane; 16]),
    // ("i16x8.extract_lane_u", U16x8ExtractLane) => 0xfd -> 25 ([Vector16x8Lane; 16]),
    // ("i16x8.replace_lane",   I16x8ReplaceLane) => 0xfd -> 26 ([Vector16x8Lane; 16]),
    // ("i32x4.extract_lane",   I32x4ExtractLane) => 0xfd -> 27 ([Vector32x4Lane; 16]),
    // ("i32x4.replace_lane",   I32x4ReplaceLane) => 0xfd -> 28 ([Vector32x4Lane; 16]),
    // ("i64x2.extract_lane",   I64x2ExtractLane) => 0xfd -> 29 ([Vector64x2Lane; 16]),
    // ("i64x2.replace_lane",   I64x2ReplaceLane) => 0xfd -> 30 ([Vector64x2Lane; 16]),
    // ("f32x4.extract_lane",   F32x4ExtractLane) => 0xfd -> 31 ([Vector32x4Lane; 16]),
    // ("f32x4.replace_lane",   F32x4ReplaceLane) => 0xfd -> 32 ([Vector32x4Lane; 16]),
    // ("f64x2.extract_lane",   F64x2ExtractLane) => 0xfd -> 33 ([Vector64x2Lane; 16]),
    // ("f64x2.replace_lane",   F64x2ReplaceLane) => 0xfd -> 34 ([Vector64x2Lane; 16]),
    //
    // ("i8x16.swizzle",   I8x16Swizzle) => 0xfd -> 14,
    // ("i8x16.splat",       I8x16Splat) => 0xfd -> 15,
    // ("i16x8.splat",       I16x8Splat) => 0xfd -> 16,
    // ("i32x4.splat",       I32x4Splat) => 0xfd -> 17,
    // ("i64x2.splat",       I64x2Splat) => 0xfd -> 18,
    // ("f32x4.splat",       F32x4Splat) => 0xfd -> 19,
    // ("f64x2.splat",       F64x2Splat) => 0xfd -> 20,
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
    // ("v128.andnot",       V128AndNot) => 0xfd -> 79,
    ("v128.or",               V128Or) => 0xfd -> 80 code: arithmetic!(i128; or),
    ("v128.xor",             V128Xor) => 0xfd -> 81 code: arithmetic!(i128; xor),
    // ("v128.bitselect", V128BitSelect) => 0xfd -> 82,
    ("v128.any_true",    V128AnyTrue) => 0xfd -> 83 code: compare!(a: i128; a != 0),
    //
    //
    // // ## Numeric operations
    //
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
