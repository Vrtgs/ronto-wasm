use crate::instruction::{BlockType, Data, ExecutionError, ExecutionResult, Expression, Extern, Function, Global, IfElseBlock, Label, Labels, Local, NullByte, OptionalValueType, Table, Type};
use crate::parser::{LabelIndex, MemoryArgument, MemoryIndex, ReferenceType, ValueType};
use crate::runtime::parameter::sealed::{SealedInput, SealedOutput};
use crate::runtime::parameter::{FunctionInput, FunctionOutput};
use crate::runtime::{ReferenceValue, StackFrame, Validator, Value, ValueInner, WasmContext};
use crate::vector::Index;
use bytemuck::Pod;
use std::marker::PhantomData;

pub(super) trait InstructionCode<Data> {
    fn validate(&self, data: Data, validator: &mut Validator) -> bool;
    fn call(&self, data: Data, context: &mut WasmContext) -> ExecutionResult;
}

impl<
    Data,
    In: FunctionInput,
    Out: FunctionOutput,
    F: Fn(Data, In, &mut WasmContext) -> ExecutionResult<Out>,
> InstructionCode<Data> for Primitive<Data, In, Out, F>
{
    fn validate(&self, _: Data, validator: &mut Validator) -> bool {
        let res = In::validate(validator);
        if res {
            Out::update_validator(validator)
        }
        res
    }

    fn call(&self, data: Data, context: &mut WasmContext) -> ExecutionResult {
        let input = In::get(context.stack);
        (self.f)(data, input, context).map(|x| Out::push(x, context.stack))
    }
}

pub(super) struct Primitive<Data, In, Out, F> {
    f: F,
    data: PhantomData<fn(Data, In, &mut WasmContext) -> Out>,
}

impl<Data, In, Out, F: Fn(Data, In) -> Out> Primitive<Data, In, Out, F> {
    pub(super) const fn ok(
        f: F,
    ) -> Primitive<Data, In, Out, impl Fn(Data, In, &mut WasmContext) -> ExecutionResult<Out>> {
        Primitive::new(move |data, input| Ok(f(data, input)))
    }
}

impl<Data, In, Out, F: Fn(Data, In) -> ExecutionResult<Out>> Primitive<Data, In, Out, F> {
    pub(super) const fn new(
        f: F,
    ) -> Primitive<Data, In, Out, impl Fn(Data, In, &mut WasmContext) -> ExecutionResult<Out>> {
        Primitive::full(move |data, input, _| f(data, input))
    }
}

impl<Data, In, Out, F: Fn(Data, In, &mut WasmContext) -> ExecutionResult<Out>>
Primitive<Data, In, Out, F>
{
    pub(super) const fn full(f: F) -> Self {
        Self {
            f,
            data: PhantomData,
        }
    }
}

macro_rules! flag {
    ($name: ident { $flag_name: ident }; true = $truthy: ident; false = $falsy: ident) => {
        trait $name {
            const $flag_name: bool;
        }
        pub(super) struct $truthy;
        pub(super) struct $falsy;
        impl $name for $truthy {
            const $flag_name: bool = true;
        }

        impl $name for $falsy {
            const $flag_name: bool = false;
        }
    };
}

flag!(BlockBranchBehavior { LOOP_BACK }; true = Loop; false = Break);

pub(super) struct Block<T>(pub PhantomData<T>);

impl<T: BlockBranchBehavior> InstructionCode<(&BlockType, &Expression)> for Block<T> {
    fn validate(&self, _: (&BlockType, &Expression), _: &mut Validator) -> bool {
        todo!()
    }

    fn call(
        &self,
        (&r#type, expr): (&BlockType, &Expression),
        context: &mut WasmContext,
    ) -> ExecutionResult {
        let return_address = Index::from_usize(context.stack.len());
        let frame = StackFrame {
            return_amount: match r#type {
                BlockType::Empty => Index::ZERO,
                BlockType::Type(_) => Index(if T::LOOP_BACK { 0 } else { 1 }),
                BlockType::TypeIndex(idx) => {
                    let break_val = match T::LOOP_BACK {
                        false => context.get_type_output(idx),
                        true => context.get_type_input(idx),
                    };
                    Index::from_usize(break_val.unwrap().len())
                }
            },
            return_address,
        };
        context.stack_frames.push(frame);

        let ret = 'block: loop {
            for instruction in expr.instructions.iter() {
                match instruction.execute(context) {
                    Ok(()) => (),
                    Err(ExecutionError::Trap) => return Err(ExecutionError::Trap),
                    Err(ExecutionError::Unwind(LabelIndex(Index(0)))) => match T::LOOP_BACK {
                        true => continue 'block,
                        false => break 'block Ok(()),
                    },
                    Err(ExecutionError::Unwind(LabelIndex(Index(up @ 1..)))) => {
                        break 'block Err(ExecutionError::Unwind(LabelIndex(Index(up - 1))));
                    }
                }
            }

            break 'block Ok(());
        };

        let popped_frame = context.stack_frames.pop();

        debug_assert_eq!(popped_frame, Some(frame));

        ret
    }
}

flag!(BranchBehavior { CONDITIONAL }; true = Conditional; false = Unconditional);

pub(super) struct IfBlock;

impl InstructionCode<(&BlockType, &IfElseBlock)> for IfBlock {
    fn validate(&self, _: (&BlockType, &IfElseBlock), validator: &mut Validator) -> bool {
        u32::validate(validator)
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

        let instr = match i32::get(context.stack) {
            0 => if_not,
            _ => if_so,
        };

        instr.iter().try_for_each(|i| i.execute(context))
    }
}

pub(super) struct Branch<T>(pub PhantomData<T>);

impl<T: BranchBehavior> InstructionCode<&Label> for Branch<T> {
    fn validate(&self, &label: &Label, _: &mut Validator) -> bool {
        todo!()
    }

    fn call(&self, &label: &Label, context: &mut WasmContext) -> ExecutionResult {
        if T::CONDITIONAL && i32::get(context.stack) == 0 {
            return Ok(());
        }

        let label_offset = label.0.0;
        let index = Index(Index::from_usize(context.stack_frames.len()).0 - label_offset - 1);

        let return_frame = context.stack_frames[index.as_usize()];
        let stack_start = return_frame.return_address.as_usize();
        let stack_end =
            Index(Index::from_usize(context.stack.len()).0 - return_frame.return_amount.0)
                .as_usize();

        context.stack.drain(stack_start..stack_end);

        Err(ExecutionError::Unwind(label))
    }
}

pub(super) struct BranchTable;

impl InstructionCode<(&Labels, &Label)> for BranchTable {
    fn validate(&self, (labels, fallback): (&Labels, &Label), validator: &mut Validator) -> bool {
        labels
            .iter()
            .chain([fallback])
            .all(|&label| validator.contains_label(label))
            && i32::validate(validator)
    }

    fn call(
        &self,
        (labels, &fallback): (&Labels, &Label),
        context: &mut WasmContext,
    ) -> ExecutionResult {
        let idx = Index::get(context.stack);
        let label = match labels.get(idx) {
            Some(&label) => label,
            None => fallback,
        };
        InstructionCode::call(&Branch(PhantomData::<Unconditional>), &label, context)
    }
}

pub(super) struct Drop;
impl InstructionCode<()> for Drop {
    fn validate(&self, (): (), validator: &mut Validator) -> bool {
        validator.pop().is_some()
    }

    fn call(&self, (): (), context: &mut WasmContext) -> ExecutionResult {
        let _ = context.pop().unwrap();
        Ok(())
    }
}

pub(super) struct Select;
impl InstructionCode<()> for Select {
    fn validate(&self, (): (), validator: &mut Validator) -> bool {
        i32::validate(validator) && validator.pop_n()
            .is_some_and(|[a, b]| a == b && !matches!(a, ValueType::ReferenceType(_)))
    }

    fn call(&self, (): (), context: &mut WasmContext) -> ExecutionResult {
        let [val1, val2, cond] = context.pop_n().unwrap();
        let value = match cond {
            Value::I32(0) => val2,
            Value::I32(_) => val1,
            _ => unreachable!()
        };
        context.push(value);
        Ok(())
    }
}

impl InstructionCode<&OptionalValueType> for Select {
    fn validate(&self, &ty: &OptionalValueType, validator: &mut Validator) -> bool {
        let Some(ty) = ty.0 else {
            return Select::validate(self, (), validator);
        };
        i32::validate(validator) && validator.pop_n().is_some_and(|[a, b]| ty == b && a == ty)
    }

    fn call(&self, _: &OptionalValueType, context: &mut WasmContext) -> ExecutionResult {
        Select::call(self, (), context)
    }
}

pub(super) struct RefNull;

impl InstructionCode<&ReferenceType> for RefNull {
    fn validate(&self, ref_ty: &ReferenceType, validator: &mut Validator) -> bool {
        match ref_ty {
            ReferenceType::Function => Function::update_validator(validator),
            ReferenceType::Extern => Extern::update_validator(validator),
        }
        true
    }

    fn call(&self, ref_ty: &ReferenceType, context: &mut WasmContext) -> ExecutionResult {
        context.push(Value::Ref(match ref_ty {
            ReferenceType::Function => ReferenceValue::Function(Function::NULL),
            ReferenceType::Extern => ReferenceValue::Extern(Extern::NULL),
        }));
        Ok(())
    }
}

pub(super) struct Call;

impl InstructionCode<&Function> for Call {
    fn validate(&self, &func: &Function, validator: &mut Validator) -> bool {
        validator.simulate_call(func)
    }

    fn call(&self, &func: &Function, context: &mut WasmContext) -> ExecutionResult {
        let func = context.get_function(func).unwrap();
        match context.call(func) {
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
        (&table_idx, &expected_ty): (&Table, &Type),
        context: &mut WasmContext,
    ) -> ExecutionResult {
        let idx = Index::get(context.stack);
        let ReferenceValue::Function(func_idx) = context.table_load(table_idx, idx).unwrap() else {
            unreachable!();
        };

        let func = context.get_function(func_idx).ok_or(ExecutionError::Trap)?;

        if func.r#type != expected_ty {
            return Err(ExecutionError::Trap);
        }

        match context.call(func) {
            Ok(()) => Ok(()),
            Err(()) => Err(ExecutionError::Trap),
        }
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
        context.virtual_machine.load_global(self).unwrap()
    }

    fn store(self, value: Value, context: &mut WasmContext) {
        // due to validation, we exist and are mutable
        context.virtual_machine.store_global(self, value).unwrap()
    }
}

pub(super) enum AccessType {
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

pub(super) struct VariableAccess<I, const A: usize>(pub PhantomData<[I; A]>);

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

pub(super) struct MemoryAccess<T, const A: usize>(pub PhantomData<[T; A]>);

impl<T: ValueInner, const A: usize> InstructionCode<&MemoryArgument> for MemoryAccess<T, A> {
    fn validate(&self, &_: &MemoryArgument, validator: &mut Validator) -> bool {
        match access_type!(@fetch A) {
            AccessType::Get => {
                let res = u32::validate(validator);
                if res {
                    T::update_validator(validator)
                };
                res
            }
            AccessType::Set => T::validate(validator) && u32::validate(validator),
            AccessType::Tee => {
                let res = T::validate(validator) && u32::validate(validator);
                if res {
                    T::update_validator(validator)
                }
                res
            }
        }
    }

    fn call(&self, &mem_arg: &MemoryArgument, context: &mut WasmContext) -> ExecutionResult {
        match access_type!(@fetch A) {
            AccessType::Get => {
                let index = Index::get(context.stack);
                let value = context.mem_load::<T>(MemoryIndex::ZERO, mem_arg, index)?;
                context.push(value.into());
            }
            AccessType::Set => {
                let value = context.pop().and_then(T::from).unwrap();
                let index = Index::get(context.stack);
                context.mem_store::<T>(MemoryIndex::ZERO, mem_arg, index, &value)?;
            }
            AccessType::Tee => {
                let index = Index::get(context.stack);
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

pub(super) struct CastingMemoryAccess<T, E, const A: usize>(pub PhantomData<[(T, E); A]>);

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
                let index = Index::get(context.stack);
                let value = context.mem_load::<E>(MemoryIndex::ZERO, mem_arg, index)?;
                context.push(E::extend(value).into());
            }
            AccessType::Set => {
                let value = context.pop().and_then(T::from).map(E::narrow).unwrap();
                let index = Index::get(context.stack);
                context.mem_store::<E>(MemoryIndex::ZERO, mem_arg, index, &value)?;
            }
            AccessType::Tee => unreachable!(),
        }
        Ok(())
    }
}

pub(super) struct MemoryInit;

impl InstructionCode<(&Data, &NullByte)> for MemoryInit {
    fn validate(&self, (&data, _): (&Data, &NullByte), _: &mut Validator) -> bool {
        todo!()
    }

    fn call(&self, (&data, _): (&Data, &NullByte), context: &mut WasmContext) -> ExecutionResult {
        let (mem_offset, data_offset, n) = <(Index, Index, Index)>::get(context.stack);

        context.mem_init(MemoryIndex::ZERO, mem_offset, data, data_offset, n).map_err(Into::into)
    }
}


pub(super) use {access_type};