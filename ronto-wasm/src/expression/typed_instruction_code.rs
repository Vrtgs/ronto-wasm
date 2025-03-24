use crate::expression::{ActiveCompilation, ExecutionResult};
use crate::parser::{
    DataIndex, ExternIndex, FunctionIndex, MemoryIndex,
    ReferenceType, TableIndex, TagByte, TypeIndex,
};
use crate::runtime::parameter::sealed::{SealedInput, SealedOutput};
use crate::runtime::parameter::{FunctionInput, FunctionOutput};
use crate::runtime::{ReferenceValue, Trap, ValueInner, WasmContext, WordAlign};
use crate::vector::Index;
use bytemuck::Pod;
use std::marker::PhantomData;

pub(super) trait TypedInstructionCode<Data> {
    fn validate(self, data: Data, compiler: &mut ActiveCompilation) -> bool;
    fn call(self, data: Data, context: &mut WasmContext) -> ExecutionResult;
}

impl<
    Data,
    In: FunctionInput,
    Out: FunctionOutput,
    F: Fn(Data, In, &mut WasmContext) -> ExecutionResult<Out>,
> TypedInstructionCode<Data> for Primitive<Data, In, Out, F>
{
    fn validate(self, _: Data, compiler: &mut ActiveCompilation) -> bool {
        let res = In::get_from_compiler(compiler);
        if res {
            Out::update_compiler(compiler)
        }
        res
    }

    fn call(self, data: Data, context: &mut WasmContext) -> ExecutionResult {
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

pub(super) struct RefNull;

impl TypedInstructionCode<&ReferenceType> for RefNull {
    fn validate(self, ref_ty: &ReferenceType, compiler: &mut ActiveCompilation) -> bool {
        match ref_ty {
            ReferenceType::Function => FunctionIndex::update_compiler(compiler),
            ReferenceType::Extern => ExternIndex::update_compiler(compiler),
        }
        true
    }

    fn call(self, ref_ty: &ReferenceType, context: &mut WasmContext) -> ExecutionResult {
        match ref_ty {
            ReferenceType::Function => context.push(FunctionIndex::NULL),
            ReferenceType::Extern => context.push(ExternIndex::NULL),
        }
        Ok(())
    }
}

pub(super) struct Call;

impl TypedInstructionCode<&FunctionIndex> for Call {
    fn validate(self, &func: &FunctionIndex, compiler: &mut ActiveCompilation) -> bool {
        compiler.simulate_call(func)
    }

    fn call(self, &func: &FunctionIndex, context: &mut WasmContext) -> ExecutionResult {
        let func = context.get_function(func).unwrap();
        context.call(func)
    }
}

impl TypedInstructionCode<(&TypeIndex, &TableIndex)> for Call {
    fn validate(
        self,
        (&ty, &table): (&TypeIndex, &TableIndex),
        comp: &mut ActiveCompilation,
    ) -> bool {
        Index::get_from_compiler(comp)
            && comp
            .get_table(table)
            .is_some_and(|table| table.reftype == ReferenceType::Function)
            && comp.simulate_call_indirect(ty)
    }

    fn call(
        self,
        (&expected_ty, &table_idx): (&TypeIndex, &TableIndex),
        context: &mut WasmContext,
    ) -> ExecutionResult {
        let idx = Index::get(context.stack);
        let ReferenceValue::Function(func_idx) = context.table_load(table_idx, idx).unwrap() else {
            unreachable!();
        };

        let func = context.get_function(func_idx).ok_or_else(Trap::new)?;

        if func.r#type != expected_ty {
            return Err(Trap::new());
        }

        context.call(func)
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



pub(super) struct MemoryAccess<T, const A: usize>(pub PhantomData<[T; A]>);

impl<B, T: ValueInner, const A: usize> TypedInstructionCode<&MemoryArgument<B>> for MemoryAccess<T, A> {
    fn validate(self, &_: &MemoryArgument<B>, compiler: &mut ActiveCompilation) -> bool {
        match access_type!(@fetch A) {
            AccessType::Get => {
                let res = u32::get_from_compiler(compiler);
                if res {
                    T::update_compiler(compiler)
                };
                res
            }
            AccessType::Set => T::get_from_compiler(compiler) && u32::get_from_compiler(compiler),
            AccessType::Tee => {
                let res = T::get_from_compiler(compiler) && u32::get_from_compiler(compiler);
                if res {
                    T::update_compiler(compiler)
                }
                res
            }
        }
    }

    fn call(self, &mem_arg: &MemoryArgument<B>, context: &mut WasmContext) -> ExecutionResult {
        match access_type!(@fetch A) {
            AccessType::Get => {
                let index = Index::get(context.stack);
                let value = context.mem_load::<T>(MemoryIndex::ZERO, mem_arg, index)?;
                context.push(value);
            }
            AccessType::Set => {
                let value = T::get(context.stack);
                let index = Index::get(context.stack);
                context.mem_store::<T>(MemoryIndex::ZERO, mem_arg, index, &value)?;
            }
            AccessType::Tee => {
                let index = Index::get(context.stack);
                let value = context.peek::<T>().unwrap();
                context.mem_store::<WordAlign<T>>(MemoryIndex::ZERO, mem_arg, index, value)?;
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

impl<B, T: ValueInner, E: Extend<T>, const A: usize> TypedInstructionCode<&MemoryArgument<B>>
for CastingMemoryAccess<T, E, A>
{
    #[inline(always)]
    fn validate(self, mem_arg: &MemoryArgument<B>, compiler: &mut ActiveCompilation) -> bool {
        MemoryAccess::<T, A>::validate(MemoryAccess(PhantomData), mem_arg, compiler)
    }

    fn call(self, &mem_arg: &MemoryArgument<B>, context: &mut WasmContext) -> ExecutionResult {
        match access_type!(@fetch A) {
            AccessType::Get => {
                let index = Index::get(context.stack);
                let value = context.mem_load::<E>(MemoryIndex::ZERO, mem_arg, index)?;
                context.push(E::extend(value));
            }
            AccessType::Set => {
                let value = E::narrow(T::get(context.stack));
                let index = Index::get(context.stack);
                context.mem_store::<E>(MemoryIndex::ZERO, mem_arg, index, &value)?;
            }
            AccessType::Tee => unreachable!(),
        }
        Ok(())
    }
}

pub(super) struct MemoryInit;

impl TypedInstructionCode<(&DataIndex, &TagByte<0x00>)> for MemoryInit {
    fn validate(self, (&data, _): (&DataIndex, &TagByte<0x00>), comp: &mut ActiveCompilation) -> bool {
        comp.has_memory(MemoryIndex::ZERO)
            && comp.has_data(data)
            && <(Index, Index, Index)>::get_from_compiler(comp)
    }

    fn call(
        self,
        (&data, _): (&DataIndex, &TagByte<0x00>),
        context: &mut WasmContext,
    ) -> ExecutionResult {
        let (mem_offset, data_offset, n) = <(Index, Index, Index)>::get(context.stack);
        context
            .mem_init(MemoryIndex::ZERO, mem_offset, data, data_offset, n)
            .map_err(Into::into)
    }
}

pub(super) use access_type;
use crate::expression::definitions::MemoryArgument;
