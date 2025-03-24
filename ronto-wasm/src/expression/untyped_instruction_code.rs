use crate::expression::definitions::Optional;
use crate::expression::typed_instruction_code::{AccessType, access_type};
use crate::expression::{ActiveCompilation, CompiledUntypedInstruction, ExecutionResult};
use crate::parser::{GlobalIndex, LocalIndex, ValueType};
use crate::runtime::parameter::sealed::SealedInput;
use crate::runtime::{ValueBitsType, ValueInner, WasmContext};
use anyhow::{Context, ensure};
use std::marker::PhantomData;

pub(super) trait UntypedInstructionCode<Data> {
    type CompiledData;

    fn compile(
        data: Data,
        compiler: &mut ActiveCompilation,
    ) -> anyhow::Result<CompiledUntypedInstruction>;
    fn call<T: ValueInner>(data: Self::CompiledData, context: &mut WasmContext) -> ExecutionResult;
}

pub(super) struct Drop;

impl UntypedInstructionCode<()> for Drop {
    type CompiledData = ();

    fn compile(
        (): (),
        compiler: &mut ActiveCompilation,
    ) -> anyhow::Result<CompiledUntypedInstruction> {
        let val = compiler
            .pop()
            .context("drop invoked, when nothing was on the top of the stack")?;

        Ok(match val.bits() {
            ValueBitsType::I32 => CompiledUntypedInstruction::Drop32,
            ValueBitsType::I64 => CompiledUntypedInstruction::Drop64,
            ValueBitsType::V128 => CompiledUntypedInstruction::Drop128,
        })
    }

    fn call<T: ValueInner>((): (), context: &mut WasmContext) -> ExecutionResult {
        context.pop::<T>().unwrap();
        Ok(())
    }
}

pub(super) struct Select;

fn compile_select(
    compiler: &mut ActiveCompilation,
) -> anyhow::Result<(CompiledUntypedInstruction, ValueType)> {
    let [ty1, ty2, cond] = compiler
        .pop_n()
        .context("select expects 3 values on the stack")?;
    ensure!(
        cond == ValueType::I32,
        "the top of the stack is not a condition"
    );
    ensure!(ty1 == ty2, "select type mismatch");
    let ty = ty1;
    compiler.push(ty);
    let instr = match ty.bits() {
        ValueBitsType::I32 => CompiledUntypedInstruction::Select32,
        ValueBitsType::I64 => CompiledUntypedInstruction::Select64,
        ValueBitsType::V128 => CompiledUntypedInstruction::Select128,
    };

    Ok((instr, ty))
}

impl UntypedInstructionCode<()> for Select {
    type CompiledData = ();

    fn compile(
        (): (),
        compiler: &mut ActiveCompilation,
    ) -> anyhow::Result<CompiledUntypedInstruction> {
        let (instr, val) = compile_select(compiler)?;
        ensure!(
            matches!(val, ValueType::NumericType(_)),
            "implicit select only supports numeric types"
        );
        Ok(instr)
    }

    fn call<T: ValueInner>((): (), context: &mut WasmContext) -> ExecutionResult {
        let cond = i32::get(context.stack);
        let (val1, val2) = <(T, T)>::get(context.stack);

        let val = match cond {
            0 => val2,
            _ => val1,
        };

        context.push(val);
        Ok(())
    }
}

impl UntypedInstructionCode<Optional<ValueType>> for Select {
    type CompiledData = ();

    fn compile(
        Optional(ty): Optional<ValueType>,
        compiler: &mut ActiveCompilation,
    ) -> anyhow::Result<CompiledUntypedInstruction> {
        let Some(ty) = ty else {
            return <Select as UntypedInstructionCode<()>>::compile((), compiler);
        };
        let (instr, val) = compile_select(compiler)?;
        ensure!(val == ty, "explicit select type mismatch");
        Ok(instr)
    }

    fn call<T: ValueInner>((): (), context: &mut WasmContext) -> ExecutionResult {
        <Select as UntypedInstructionCode<()>>::call::<T>((), context)
    }
}

pub(super) struct VariableAccess<I, const A: usize>(PhantomData<[I; A]>);

pub(super) type LocalGet = VariableAccess<LocalIndex, { access_type!(AccessType::Get) }>;
pub(super) type LocalSet = VariableAccess<LocalIndex, { access_type!(AccessType::Set) }>;
pub(super) type LocalTee = VariableAccess<LocalIndex, { access_type!(AccessType::Tee) }>;

pub(super) type GlobalGet = VariableAccess<GlobalIndex, { access_type!(AccessType::Get) }>;
pub(super) type GlobalSet = VariableAccess<GlobalIndex, { access_type!(AccessType::Set) }>;

trait VariableIndex: Copy {
    fn exists(self, compiler: &mut ActiveCompilation) -> Option<Self>;
    fn mutable(self, compiler: &mut ActiveCompilation) -> bool;
    fn r#type(self, compiler: &mut ActiveCompilation) -> ValueType;
    fn load<T: ValueInner>(self, context: &mut WasmContext) -> T;
    fn store<T: ValueInner>(self, value: T, context: &mut WasmContext);

    fn construct<const A: usize>(
        self,
        bits: ValueBitsType,
        compiler: &mut ActiveCompilation,
    ) -> CompiledUntypedInstruction;
}

impl VariableIndex for LocalIndex {
    fn exists(self, compiler: &mut ActiveCompilation) -> Option<LocalIndex> {
        compiler.get_local(self).map(|(_, idx)| idx)
    }

    fn mutable(self, _: &mut ActiveCompilation) -> bool {
        true
    }

    fn r#type(self, compiler: &mut ActiveCompilation) -> ValueType {
        // `exists` should always run before `type`
        compiler.get_local(self).unwrap().0
    }

    fn load<T: ValueInner>(self, context: &mut WasmContext) -> T {
        // due to validation, we exist and are of type T
        context.get_local(self).unwrap().read()
    }

    fn store<T: ValueInner>(self, value: T, context: &mut WasmContext) {
        // due to validation, we exist and are of type T
        context.get_local(self).unwrap().write(value)
    }

    fn construct<const A: usize>(
        self,
        bits: ValueBitsType,
        _: &mut ActiveCompilation,
    ) -> CompiledUntypedInstruction {
        match (access_type!(@fetch A), bits) {
            (AccessType::Get, ValueBitsType::I32) => CompiledUntypedInstruction::LocalGet32(self),
            (AccessType::Get, ValueBitsType::I64) => CompiledUntypedInstruction::LocalGet64(self),
            (AccessType::Get, ValueBitsType::V128) => CompiledUntypedInstruction::LocalGet128(self),
            (AccessType::Set, ValueBitsType::I32) => CompiledUntypedInstruction::LocalSet32(self),
            (AccessType::Set, ValueBitsType::I64) => CompiledUntypedInstruction::LocalSet64(self),
            (AccessType::Set, ValueBitsType::V128) => CompiledUntypedInstruction::LocalSet128(self),
            (AccessType::Tee, ValueBitsType::I32) => CompiledUntypedInstruction::LocalTee32(self),
            (AccessType::Tee, ValueBitsType::I64) => CompiledUntypedInstruction::LocalTee64(self),
            (AccessType::Tee, ValueBitsType::V128) => CompiledUntypedInstruction::LocalTee128(self),
        }
    }
}

impl VariableIndex for GlobalIndex {
    fn exists(self, compiler: &mut ActiveCompilation) -> Option<GlobalIndex> {
        compiler.get_global(self).is_some().then_some(self)
    }

    fn mutable(self, compiler: &mut ActiveCompilation) -> bool {
        // `exists` should always run before `mutable`
        compiler.get_global(self).unwrap().mutable
    }

    fn r#type(self, compiler: &mut ActiveCompilation) -> ValueType {
        // `exists` should always run before `type`
        compiler.get_global(self).unwrap().value_type
    }

    fn load<T: ValueInner>(self, context: &mut WasmContext) -> T {
        // due to validation, we exist
        context
            .virtual_machine
            .store
            .load_global(self)
            .and_then(T::from)
            .unwrap()
    }

    fn store<T: ValueInner>(self, value: T, context: &mut WasmContext) {
        // due to validation, we exist and are mutable
        context
            .virtual_machine
            .store
            .store_global(self, value.into())
            .unwrap()
    }

    fn construct<const A: usize>(
        self,
        bits: ValueBitsType,
        _: &mut ActiveCompilation,
    ) -> CompiledUntypedInstruction {
        match (access_type!(@fetch A), bits) {
            (AccessType::Get, ValueBitsType::I32) => CompiledUntypedInstruction::GlobalGet32(self),
            (AccessType::Get, ValueBitsType::I64) => CompiledUntypedInstruction::GlobalGet64(self),
            (AccessType::Get, ValueBitsType::V128) => {
                CompiledUntypedInstruction::GlobalGet128(self)
            }
            (AccessType::Set, ValueBitsType::I32) => CompiledUntypedInstruction::GlobalSet32(self),
            (AccessType::Set, ValueBitsType::I64) => CompiledUntypedInstruction::GlobalSet64(self),
            (AccessType::Set, ValueBitsType::V128) => {
                CompiledUntypedInstruction::GlobalSet128(self)
            }
            (AccessType::Tee, _) => unreachable!(),
        }
    }
}

impl<I: VariableIndex, const A: usize> UntypedInstructionCode<I> for VariableAccess<I, A> {
    type CompiledData = I;

    fn compile(
        index: I,
        compiler: &mut ActiveCompilation,
    ) -> anyhow::Result<CompiledUntypedInstruction> {
        let offset_index = index.exists(compiler).context("variable doesn't exist")?;

        let index_ty = index.r#type(compiler);

        'a: {
            let valid_stack = match access_type!(@fetch A) {
                AccessType::Get => {
                    compiler.push(index_ty);
                    break 'a;
                }
                AccessType::Set => compiler.pop().is_some_and(|ty| ty == index_ty),
                AccessType::Tee => compiler.peek().is_some_and(|&ty| ty == index_ty),
            };
            ensure!(index.mutable(compiler), "variable is not mutable");
            ensure!(
                valid_stack,
                "last element on the stack is not the same as the variable"
            );
        };

        Ok(I::construct::<A>(offset_index, index_ty.bits(), compiler))
    }

    fn call<T: ValueInner>(index: I, context: &mut WasmContext) -> ExecutionResult {
        match access_type!(@fetch A) {
            AccessType::Get => {
                let value = index.load::<T>(context);
                context.push(value);
            }
            AccessType::Set => {
                let value = T::get(context.stack);
                index.store(value, context)
            }
            AccessType::Tee => {
                let value = context.peek::<T>().unwrap();
                index.store(value.read(), context)
            }
        }
        Ok(())
    }
}
