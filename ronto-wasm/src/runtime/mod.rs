use crate::expression::FunctionBody;
use crate::parser::{
    DataIndex, ExportDescription, ExternIndex, FunctionIndex, GlobalIndex, GlobalType, LocalIndex,
    MemoryArgument, MemoryIndex, ReferenceType, TableIndex, TableType, TypeIndex, ValueType,
};
use crate::runtime::memory_buffer::{MemoryBuffer, MemoryError, MemoryFault, OutOfMemory};
use crate::runtime::parameter::{FunctionInput, FunctionOutput};
use crate::vector::{Index, WasmVec};
use crate::Stack;
use anyhow::{ensure, Context};
use bytemuck::{Pod, Zeroable};
use crossbeam::atomic::AtomicCell;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use thiserror::Error;

pub mod trap;

pub mod memory_buffer;
pub mod parameter;
pub mod wasi_snapshot_preview1;

pub mod linker;
pub mod store;

use crate::runtime::linker::{Import, NativeFunction};
pub use {linker::Linker, store::Store, trap::Trap};


#[derive(Debug, Copy, Clone)]
pub enum ReferenceValue {
    Function(FunctionIndex),
    Extern(ExternIndex),
}

impl ReferenceValue {
    pub fn r#type(&self) -> ReferenceType {
        match self {
            ReferenceValue::Function(_) => ReferenceType::Function,
            ReferenceValue::Extern(_) => ReferenceType::Extern,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Value {
    I32(u32),
    I64(u64),
    F32(f32),
    F64(f64),
    V128(u128),
    Ref(ReferenceValue),
}

#[derive(Debug, Copy, Clone)]
pub enum ValueBitsType {
    I32,
    I64,
    V128,
}

#[derive(Debug, Copy, Clone)]
pub enum ValueBits {
    I32(u32),
    I64(u64),
    V128(u128),
}

impl Value {
    pub fn new(ty: ValueType) -> Value {
        match ty {
            ValueType::I32 => Value::I32(0),
            ValueType::I64 => Value::I64(0),
            ValueType::F32 => Value::F32(0.0),
            ValueType::F64 => Value::F64(0.0),
            ValueType::V128 => Value::V128(0),
            ValueType::Function => Value::Ref(ReferenceValue::Function(FunctionIndex::NULL)),
            ValueType::Extern => Value::Ref(ReferenceValue::Extern(ExternIndex::NULL)),
        }
    }

    pub fn r#type(&self) -> ValueType {
        match self {
            Value::I32(_) => ValueType::I32,
            Value::I64(_) => ValueType::I64,
            Value::F32(_) => ValueType::F32,
            Value::F64(_) => ValueType::F64,
            Value::V128(_) => ValueType::V128,
            Value::Ref(reference) => ValueType::ReferenceType(reference.r#type()),
        }
    }

    pub fn to_bits(self) -> ValueBits {
        match self {
            Value::I32(x) => ValueBits::I32(x),
            Value::I64(x) => ValueBits::I64(x),
            Value::F32(floats) => ValueBits::I32(floats.to_bits()),
            Value::F64(floats) => ValueBits::I64(floats.to_bits()),
            Value::V128(vec) => ValueBits::V128(vec),
            Value::Ref(ReferenceValue::Extern(ExternIndex(Index(idx))))
            | Value::Ref(ReferenceValue::Function(FunctionIndex(Index(idx)))) => {
                ValueBits::I32(idx)
            }
        }
    }
}

/// # Safety
/// must have no padding when aligned to 4 bytes
pub(crate) unsafe trait WordAligned: Pod {}

#[derive(Copy, Clone)]
#[repr(C, packed(4))]
pub(crate) struct WordAlign<T: WordAligned>(T);

unsafe impl<T: WordAligned> Zeroable for WordAlign<T> {}
unsafe impl<T: WordAligned> Pod for WordAlign<T> {}

impl<T: WordAligned> WordAlign<T> {
    pub(crate) fn read(&self) -> T {
        self.0
    }
    pub(crate) fn write(&mut self, value: T) {
        self.0 = value
    }
}

pub(crate) trait ValueInner: WordStore {
    const TYPE: ValueType;

    fn into(self) -> Value;
    fn from(data: Value) -> Option<Self>;
}

pub(crate) trait WordStore: WordAligned + Send + Sync + Debug {
    fn from_words(data: &mut Vec<Word>) -> Option<Self>;
    fn to_words(self) -> impl IntoIterator<Item=Word>;
    fn push_words(self, data: &mut Vec<Word>) {
        data.extend(self.to_words())
    }
    fn ref_from_words(data: &[Word]) -> Option<&WordAlign<Self>>;
    fn get_mut(index: LocalIndex, locals: &mut WasmVec<Word>) -> Option<&mut WordAlign<Self>>;
}

macro_rules! impl_word_aligned {
    ($($ty:ty),*) => {$(
    const _: () = {
        const fn assert_pod<T: Pod>() {}
        assert_pod::<$ty>();
        assert!(size_of::<$ty>() % size_of::<Word>() == 0 && align_of::<$ty>() >= align_of::<Word>())
    };

    unsafe impl WordAligned for $ty {}
    )*};
}

impl_word_aligned!(Index, FunctionIndex, ExternIndex, i32, u32, f32, i64, u64, f64, i128, u128);

macro_rules! impl_numeric_value {
    ($($variant: ident => $ty:ty $(; $sty: ty)?),+ $(,)?) => {$(
        impl ValueInner for $ty {
            const TYPE: ValueType = ValueType::$variant;

            fn into(self) -> Value {
                Value::$variant(self)
            }

            fn from(data: Value) -> Option<Self> {
                match data {
                    Value::$variant(inner) => Some(inner),
                    _ => None
                }
            }
        }
        $(
        impl ValueInner for $sty {
            const TYPE: ValueType = <$ty as ValueInner>::TYPE;

            fn into(self) -> Value {
                Value::$variant(self as $ty)
            }

            fn from(data: Value) -> Option<Self> {
                <$ty as ValueInner>::from(data).map(|inner| inner as $sty)
            }
        }

        impl WordStore for $sty {
            fn from_words(data: &mut Vec<Word>) -> Option<Self> {
                <$ty>::from_words(data).map(|inner| inner as $sty)
            }

            fn to_words(self) -> impl IntoIterator<Item=Word> {
                <$ty>::to_words(self as $ty)
            }

            fn ref_from_words(data: &[Word]) -> Option<&WordAlign<Self>> {
                <$ty>::ref_from_words(data).map(bytemuck::must_cast_ref)
            }

            fn get_mut(index: LocalIndex, locals: &mut WasmVec<Word>) -> Option<&mut WordAlign<Self>> {
                <$ty>::get_mut(index, locals).map(bytemuck::must_cast_mut)
            }
        }
        )?
    )+};
}

macro_rules! impl_reference_value {
    ($($variant:ident => $ty:ty),+ $(,)?) => {$(
        impl ValueInner for $ty {
            const TYPE: ValueType = ValueType::$variant;

            fn into(self) -> Value {
                Value::Ref(ReferenceValue::$variant(self))
            }

            fn from(data: Value) -> Option<Self> {
                match data {
                    Value::Ref(ReferenceValue::$variant(inner)) => Some(inner),
                    _ => None
                }
            }
        }

        impl WordStore for $ty {
            fn from_words(data: &mut Vec<Word>) -> Option<Self> {
                <Index>::from_words(data).map(Self)
            }

            fn to_words(self) -> impl IntoIterator<Item=Word> {
                <Index>::to_words(self.0)
            }

            fn ref_from_words(data: &[Word]) -> Option<&WordAlign<Self>> {
                <Index>::ref_from_words(data).map(bytemuck::must_cast_ref)
            }

            fn get_mut(index: LocalIndex, locals: &mut WasmVec<Word>) -> Option<&mut WordAlign<Self>> {
                <Index>::get_mut(index, locals).map(bytemuck::must_cast_mut)
            }
        }
    )+};
}

macro_rules! store_32 {
    ($($ty:ty),*) => {$(
    impl WordStore for $ty {
        fn from_words(data: &mut Vec<Word>) -> Option<Self> {
            data.pop().map(bytemuck::must_cast)
        }

        fn to_words(self) -> impl IntoIterator<Item=Word> {
            [bytemuck::must_cast(self)]
        }

        fn push_words(self, data: &mut Vec<Word>) {
            data.push(bytemuck::must_cast(self))
        }

        fn ref_from_words(data: &[Word]) -> Option<&WordAlign<Self>> {
            data.last().map(bytemuck::must_cast_ref)
        }

        fn get_mut(index: LocalIndex, locals: &mut WasmVec<Word>) -> Option<&mut WordAlign<Self>> {
            locals.get_mut(index.0).map(bytemuck::must_cast_mut)
        }
    }
    )*};
}

macro_rules! store_generic {
    ($($ty:ty),*) => {$(
    impl WordStore for $ty {
        fn from_words(data: &mut Vec<Word>) -> Option<Self> {
            data.pop_n::<{size_of::<$ty>()/size_of::<Word>()}>().map(bytemuck::must_cast)
        }

        fn to_words(self) -> impl IntoIterator<Item=Word> {
            bytemuck::must_cast::<_, [Word; {size_of::<$ty>()/size_of::<Word>()}]>(self)
        }

        fn push_words(self, data: &mut Vec<Word>) {
            let arr = bytemuck::must_cast::<_, [Word; {size_of::<$ty>()/size_of::<Word>()}]>(self);
            data.extend_from_slice(&arr);
        }

        fn ref_from_words(data: &[Word]) -> Option<&WordAlign<Self>> {
            data.last_chunk::<{size_of::<$ty>()/size_of::<Word>()}>().map(bytemuck::must_cast_ref)
        }

        fn get_mut(index: LocalIndex, locals: &mut WasmVec<Word>) -> Option<&mut WordAlign<Self>> {
                (**locals).get_mut(index.0.as_usize()..index.0.as_usize().checked_add(const {size_of::<$ty>()/size_of::<Word>()})?)
                    .map(bytemuck::cast_slice_mut)
                    .map(bytemuck::from_bytes_mut)
        }
    }
    )*};
}

store_32!(u32, f32);

store_generic!(u128, u64, f64);

impl_numeric_value! {
    I32  =>  u32; i32,
    I64  =>  u64; i64,
    F32  =>  f32,
    F64  =>  f64,
    V128 => u128; i128,
}

impl ValueInner for Index {
    const TYPE: ValueType = <u32 as ValueInner>::TYPE;

    fn into(self) -> Value {
        <u32 as ValueInner>::into(self.0)
    }

    fn from(data: Value) -> Option<Self> {
        <u32 as ValueInner>::from(data).map(Index)
    }
}

impl WordStore for Index {
    fn from_words(data: &mut Vec<Word>) -> Option<Self> {
        u32::from_words(data).map(Self)
    }

    fn to_words(self) -> impl IntoIterator<Item=Word> {
        u32::to_words(self.0)
    }

    fn ref_from_words(data: &[Word]) -> Option<&WordAlign<Self>> {
        u32::ref_from_words(data).map(bytemuck::must_cast_ref)
    }

    fn get_mut(index: LocalIndex, locals: &mut WasmVec<Word>) -> Option<&mut WordAlign<Self>> {
        u32::get_mut(index, locals).map(bytemuck::must_cast_mut)
    }
}

impl_reference_value! {
    Function => FunctionIndex,
    Extern => ExternIndex,
}

#[derive(Copy, Clone)]
enum Mut64Type {
    Ref(ReferenceType),
    I32,
    F32,
    I64,
    F64,
}

enum GlobalValueInner {
    Immutable(Value),
    Mutable64(AtomicU64, Mut64Type),
    Mutable128(AtomicCell<u128>),
}

impl GlobalValueInner {
    pub fn new(value: GlobalType) -> Self {
        match (value.mutable, value.value_type) {
            (false, ty) => Self::Immutable(Value::new(ty)),
            (true, ValueType::I32) => Self::Mutable64(AtomicU64::new(0), Mut64Type::I32),
            (true, ValueType::I64) => Self::Mutable64(AtomicU64::new(0), Mut64Type::I64),
            (true, ValueType::F32) => {
                Self::Mutable64(AtomicU64::new(f32::to_bits(0.0) as u64), Mut64Type::F32)
            }
            (true, ValueType::F64) => {
                Self::Mutable64(AtomicU64::new(f64::to_bits(0.0)), Mut64Type::F64)
            }
            (true, ValueType::ReferenceType(ref_ty)) => {
                Self::Mutable64(AtomicU64::new(Index::NULL.0 as u64), Mut64Type::Ref(ref_ty))
            }
            (true, ValueType::V128) => Self::Mutable128(AtomicCell::new(0)),
        }
    }

    fn make_64_bit(value: Value, ty: Mut64Type) -> Result<u64, ()> {
        Ok(match (value, ty) {
            (Value::I64(bits), Mut64Type::I64) => bits,
            (Value::F64(bits), Mut64Type::F64) => bits.to_bits(),
            (Value::F32(bits), Mut64Type::F32) => bits.to_bits() as u64,
            (Value::I32(bits), Mut64Type::I32) => bits as u64,
            (
                Value::Ref(ReferenceValue::Extern(ExternIndex(index))),
                Mut64Type::Ref(ReferenceType::Extern),
            )
            | (
                Value::Ref(ReferenceValue::Function(FunctionIndex(index))),
                Mut64Type::Ref(ReferenceType::Function),
            ) => index.0 as u64,
            _ => return Err(()),
        })
    }

    fn store_mut(&mut self, value: Value) -> Result<(), ()> {
        match *self {
            GlobalValueInner::Immutable(ref mut val) => {
                if val.r#type() != value.r#type() {
                    return Err(());
                }
                *val = value;
            }
            GlobalValueInner::Mutable64(ref mut loc, ty) => {
                *loc.get_mut() = Self::make_64_bit(value, ty)?
            }
            GlobalValueInner::Mutable128(ref mut loc) => {
                let Value::V128(val_128) = value else {
                    return Err(());
                };
                *loc = AtomicCell::new(val_128);
            }
        }

        Ok(())
    }

    fn store(&self, value: Value) -> Result<(), ()> {
        match *self {
            GlobalValueInner::Immutable(_) => Err(()),
            GlobalValueInner::Mutable64(ref loc, ty) => {
                loc.store(Self::make_64_bit(value, ty)?, Ordering::Release);
                Ok(())
            }
            GlobalValueInner::Mutable128(ref loc) => {
                let Value::V128(val_128) = value else {
                    return Err(());
                };
                loc.store(val_128);
                Ok(())
            }
        }
    }

    fn load(&self) -> Value {
        match *self {
            GlobalValueInner::Immutable(val) => val,
            GlobalValueInner::Mutable64(ref loc, ty) => {
                let bits = loc.load(Ordering::Acquire);
                match ty {
                    Mut64Type::I64 => Value::I64(bits),
                    Mut64Type::F64 => Value::F64(f64::from_bits(bits)),
                    Mut64Type::I32 => Value::I32(bits as u32),
                    Mut64Type::F32 => Value::F32(f32::from_bits(bits as u32)),
                    Mut64Type::Ref(ref_ty) => {
                        let idx = Index(bits as u32);
                        Value::Ref(match ref_ty {
                            ReferenceType::Function => ReferenceValue::Function(FunctionIndex(idx)),
                            ReferenceType::Extern => ReferenceValue::Extern(ExternIndex(idx)),
                        })
                    }
                }
            }
            GlobalValueInner::Mutable128(ref loc) => Value::V128(loc.load()),
        }
    }

    fn r#type(&self) -> GlobalType {
        match *self {
            GlobalValueInner::Immutable(val) => GlobalType {
                mutable: false,
                value_type: val.r#type(),
            },
            GlobalValueInner::Mutable64(_, ty) => {
                let value_type = match ty {
                    Mut64Type::Ref(ref_ty) => ValueType::ReferenceType(ref_ty),
                    Mut64Type::I32 => ValueType::I32,
                    Mut64Type::F32 => ValueType::F32,
                    Mut64Type::I64 => ValueType::I64,
                    Mut64Type::F64 => ValueType::F64,
                };
                GlobalType {
                    mutable: true,
                    value_type,
                }
            }
            GlobalValueInner::Mutable128(_) => GlobalType {
                mutable: true,
                value_type: ValueType::V128,
            },
        }
    }
}

#[repr(transparent)]
pub struct GlobalValueRef<'a>(&'a GlobalValueInner);

#[derive(Debug, Error)]
#[error("the global is immutable and therefore can't be changed")]
pub struct ImmutableGlobalError(());

impl GlobalValueRef<'_> {
    pub fn store(&self, value: Value) -> Result<(), ImmutableGlobalError> {
        self.0.store(value).map_err(|()| ImmutableGlobalError(()))
    }

    pub fn load(&self) -> Value {
        self.0.load()
    }
}

impl Debug for GlobalValueRef<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("GlobalValue").field(&self.load()).finish()
    }
}

struct WasmFunction {
    parameters_len: Index,
    locals: WasmVec<Word>,
    body: FunctionBody,
}

enum Body {
    WasmDefined(WasmFunction),
    Import(Arc<NativeFunction>),
}

pub(crate) struct FunctionInner {
    pub(crate) r#type: TypeIndex,
    body: Body,
}

pub enum Table {
    FunctionTable(WasmVec<FunctionIndex>),
    ExternTable(WasmVec<ExternIndex>),
}

impl TryFrom<TableType> for Table {
    type Error = OutOfMemory;

    fn try_from(value: TableType) -> Result<Self, Self::Error> {
        let reserve = value.limits.min.as_usize();
        Ok(match value.reftype {
            ReferenceType::Function => Table::FunctionTable(WasmVec::from_trusted_box(
                vec![FunctionIndex::NULL; reserve].into(),
            )),
            ReferenceType::Extern => Table::ExternTable(WasmVec::from_trusted_box(
                vec![ExternIndex::NULL; reserve].into(),
            )),
        })
    }
}

pub struct VirtualMachineOptions {
    maximum_call_depth: usize,
}

impl Default for VirtualMachineOptions {
    fn default() -> Self {
        Self {
            maximum_call_depth: 1024,
        }
    }
}

pub struct VirtualMachine {
    pub(crate) store: Store,
    pub(crate) options: VirtualMachineOptions,
}

type Constructor = Box<dyn FnOnce(&mut Store) -> anyhow::Result<()>>;

fn validate_vm(vm: &VirtualMachine) -> anyhow::Result<()> {
    ensure!(
        vm.store.start.is_none_or(|func| vm.get_typed_function::<(), ()>(func).is_ok()),
        "invalid start function index"
    );


    for (export_name, &desc) in vm.store.exports.iter() {
        let is_valid = match desc {
            ExportDescription::Function(function) => vm.get_function(function).is_some(),
            ExportDescription::Memory(memory) => vm.get_memory(memory).is_some(),
            ExportDescription::Table(table) => vm.get_table(table).is_some(),
            ExportDescription::Global(global) => vm.get_global(global).is_some(),
        };

        ensure!(is_valid, "invalid export {export_name}")
    }

    Ok(())
}

impl VirtualMachine {
    fn with_options(store: Store, options: VirtualMachineOptions) -> anyhow::Result<Self> {
        let this = VirtualMachine { store, options };
        validate_vm(&this)?;
        Ok(this)
    }

    pub fn new(store: Store) -> anyhow::Result<Self> {
        VirtualMachine::with_options(store, VirtualMachineOptions::default())
    }
}

impl VirtualMachine {
    pub(crate) fn call_unchecked(
        &self,
        function: &FunctionInner,
        stack: &mut ValueStack,
        call_depth: usize,
    ) -> Result<(), Trap> {
        let locals = match &function.body {
            Body::WasmDefined(wasm_func) => {
                let locals = stack
                    .0
                    .drain(stack.len() - wasm_func.parameters_len.as_usize()..)
                    .chain(wasm_func.locals.iter().copied())
                    .collect::<Box<[_]>>();
                WasmVec::from_trusted_box(locals)
            }
            Body::Import(_) => const { WasmVec::new() }
        };

        let mut context = WasmContext {
            virtual_machine: self,
            locals,
            stack,
            call_depth,
        };

        match &function.body {
            Body::WasmDefined(func) => func.body.eval(&mut context),
            Body::Import(imp) => imp(&mut context),
        }
    }
}

#[derive(Debug, Error)]
pub enum GetExportError {
    #[error("export could not be found")]
    ExportNotFound,
    #[error("export type mismatch. expected {expected}; found: {found:?}")]
    ExportTypeError {
        expected: &'static str,
        found: ExportDescription,
    },
}

macro_rules! get_export {
    ($self:expr; find $param: expr;$expected: literal | $name:ident) => {
        $self
            .store
            .exports
            .get($param)
            .and_then(|&interface| match interface {
                ExportDescription::$name(idx) => Some(idx),
                _ => None,
            })
    };
}

pub struct UntypedFunction<'a> {
    vm: &'a VirtualMachine,
    function: &'a FunctionInner,
}

pub struct Function<'a, T, U> {
    vm: &'a VirtualMachine,
    function: &'a FunctionInner,
    _marker: PhantomData<fn(T) -> U>,
}

impl<T: FunctionInput, U: FunctionOutput> Function<'_, T, U> {
    pub fn call(&self, parameter: T) -> Result<U, Trap> {
        let mut stack = parameter.into_input();
        self.vm.call_unchecked(self.function, &mut stack, 0)?;
        let res = U::get_output(&mut stack).unwrap();
        debug_assert!(stack.is_empty());
        Ok(res)
    }
}

#[derive(Debug, Error)]
#[error("call type mismatch; {msg}")]
pub struct MismatchedFunctionType {
    msg: String,
}

impl<'a> UntypedFunction<'a> {
    pub fn cast<T: FunctionInput, U: FunctionOutput>(
        self,
    ) -> Result<Function<'a, T, U>, MismatchedFunctionType> {
        let r#type = self.vm.store.types.get(self.function.r#type.0).unwrap();

        if !T::subtype(&r#type.parameters) || !U::subtype(&r#type.result) {
            return Err(MismatchedFunctionType {
                msg: format!(
                    "expected {type}, found {}",
                    parameter::fmt_fn_signature::<T, U>()
                ),
            });
        }

        Ok(Function {
            vm: self.vm,
            function: self.function,
            _marker: PhantomData,
        })
    }
}

#[derive(Debug, Error)]
pub enum GetFunctionError {
    #[error("function requested does not exist")]
    FunctionDoesntExist,
    #[error(transparent)]
    MismatchedType(#[from] MismatchedFunctionType),
}

#[derive(Debug, Error)]
pub enum CallError {
    #[error(transparent)]
    Trap(Trap),
    #[error(transparent)]
    GetFunctionError(#[from] GetFunctionError),
}

macro_rules! define_resource_getters {
    (
        $resource_type:ident, $capitalized_resource:ty, $return_type:ty,
        $collection:ident,
        $export_type:literal
        $(, $map_expr:expr)?
    ) => {
        paste::paste! {
            pub fn [<get_ $resource_type>](&self, index: [<$capitalized_resource Index>]) -> Option<$return_type> {
                self.store.$collection.get(index.0)$(.map(|x| ($map_expr)(self, x)))?
            }

            pub fn [<get_ $resource_type _by_name>](&self, name: &str) -> Option<$return_type> {
                let index = get_export!(self; find name; $export_type | $capitalized_resource)?;
                Some(
                    self.[<get_ $resource_type>](index)
                        .expect("exports should always contain valid indices")
                )
            }
        }
    };
}

impl VirtualMachine {
    define_resource_getters!(
        function,
        Function,
        UntypedFunction,
        functions,
        "function",
        |this, function| UntypedFunction { vm: this, function }
    );

    define_resource_getters!(table, Table, &Table, tables, "memory");

    define_resource_getters!(memory, Memory, &MemoryBuffer, memory, "memory");

    define_resource_getters!(
        global,
        Global,
        GlobalValueRef,
        globals,
        "memory",
        |_, global| GlobalValueRef(global)
    );

    pub fn get_typed_function<T: FunctionInput, U: FunctionOutput>(
        &self,
        function: FunctionIndex,
    ) -> Result<Function<T, U>, GetFunctionError> {
        let function = self
            .get_function(function)
            .ok_or(GetFunctionError::FunctionDoesntExist)?;

        function.cast().map_err(Into::into)
    }

    pub fn get_typed_function_by_name<T: FunctionInput, U: FunctionOutput>(
        &self,
        function: &str,
    ) -> Result<Function<T, U>, GetFunctionError> {
        let func = get_export!(self; find function; "function" | Function)
            .ok_or(GetFunctionError::FunctionDoesntExist)?;

        match self.get_typed_function(func) {
            Err(GetFunctionError::FunctionDoesntExist) => {
                unreachable!("checked by validation that the exports exist")
            }
            res => res,
        }
    }

    pub fn call<T: FunctionInput, U: FunctionOutput>(
        &self,
        function: FunctionIndex,
        parameter: T,
    ) -> Result<U, CallError> {
        self.get_typed_function(function)?
            .call(parameter)
            .map_err(CallError::Trap)
    }

    pub fn call_by_name<T: FunctionInput, U: FunctionOutput>(
        &self,
        function: &str,
        parameter: T,
    ) -> Result<U, CallError> {
        self.get_typed_function_by_name(function)?
            .call(parameter)
            .map_err(CallError::Trap)
    }

    pub fn start(&self) -> Result<(), CallError> {
        if let Some(start) = self.store.start {
            return self.call(start, ());
        }

        self.call_by_name("main", ())
    }
}

type Word = u32;

#[derive(Debug)]
pub(crate) struct ValueStack(pub(crate) Vec<Word>);

impl ValueStack {
    pub(crate) const fn new() -> Self {
        Self(Vec::new())
    }

    pub(crate) fn len(&self) -> usize {
        self.0.len()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub(crate) fn peek<T: ValueInner>(&self) -> Option<&WordAlign<T>> {
        T::ref_from_words(&self.0)
    }

    pub(crate) fn pop<T: ValueInner>(&mut self) -> Option<T> {
        T::from_words(&mut self.0)
    }

    pub(crate) fn push<T: ValueInner>(&mut self, value: T) {
        T::push_words(value, &mut self.0)
    }
}

pub(crate) fn values_len(values: &[ValueType]) -> anyhow::Result<Index> {
    values
        .iter()
        .try_fold(0_u32, |size, ty| {
            size.checked_add(ty.word_size())
        })
        .map(Index)
        .context("too many arguments")
}

pub struct WasmContext<'a> {
    pub(crate) virtual_machine: &'a VirtualMachine,
    pub(crate) locals: WasmVec<Word>,
    pub(crate) stack: &'a mut ValueStack,
    pub(crate) call_depth: usize,
}

impl<'a> WasmContext<'a> {
    pub(crate) fn call(&mut self, function: &FunctionInner) -> Result<(), Trap> {
        if let Some(call_depth) = self
            .call_depth
            .checked_add(1)
            .filter(|&x| x <= self.virtual_machine.options.maximum_call_depth)
        {
            return self
                .virtual_machine
                .call_unchecked(function, self.stack, call_depth);
        }

        Err(Trap::maximum_recursion_depth())
    }

    pub(crate) fn get_function(&self, function: FunctionIndex) -> Option<&'a FunctionInner> {
        self.virtual_machine.store.functions.get(function.0)
    }

    pub(crate) fn get_local<T: ValueInner>(&mut self, local: LocalIndex) -> Option<&mut WordAlign<T>> {
        T::get_mut(local, &mut self.locals)
    }

    pub(crate) fn table_load(
        &self,
        table_index: TableIndex,
        index: Index,
    ) -> Option<ReferenceValue> {
        self.virtual_machine
            .store
            .tables
            .get(table_index.0)
            .and_then(|table| match table {
                Table::FunctionTable(table) => {
                    table.get(index).copied().map(ReferenceValue::Function)
                }
                Table::ExternTable(table) => table.get(index).copied().map(ReferenceValue::Extern),
            })
    }

    pub(crate) fn mem(&self, mem_index: MemoryIndex) -> Result<&MemoryBuffer, MemoryFault> {
        self.virtual_machine
            .store
            .memory
            .get(mem_index.0)
            .ok_or_else(MemoryFault::new)
    }

    pub(crate) fn mem_load<T: Pod>(
        &self,
        mem_index: MemoryIndex,
        argument: MemoryArgument,
        index: Index,
    ) -> Result<T, MemoryFault> {
        self.mem(mem_index)
            .and_then(|mem| mem.load_internal(argument, index))
    }

    pub(crate) fn mem_store<T: Pod>(
        &self,
        mem_index: MemoryIndex,
        argument: MemoryArgument,
        index: Index,
        value: &T,
    ) -> Result<(), MemoryFault> {
        self.mem(mem_index)
            .and_then(|mem| mem.store_internal(argument, index, value))
    }

    pub(crate) fn mem_grow(&self, mem_index: MemoryIndex, by: Index) -> Result<Index, MemoryError> {
        self.mem(mem_index)
            .map_err(MemoryError::MemoryFault)
            .and_then(|mem| mem.grow(by).map_err(MemoryError::from))
    }

    pub(crate) fn mem_init(
        &self,
        mem_index: MemoryIndex,
        offset: Index,
        data: DataIndex,
        data_offset: Index,
        n: Index,
    ) -> Result<(), MemoryFault> {
        let data = self
            .virtual_machine
            .store
            .data
            .get(data.0)
            .ok_or_else(MemoryFault::new)?;
        let data_end = data_offset
            .0
            .checked_add(n.0)
            .map(Index)
            .ok_or_else(MemoryFault::new)?;
        let data = (*data.init)
            .get(data_offset.as_usize()..data_end.as_usize())
            .ok_or_else(MemoryFault::new)?;
        self.mem(mem_index).and_then(|mem| mem.init(offset, data))
    }

    pub(crate) fn mem_fill(
        &self,
        mem_index: MemoryIndex,
        offset: Index,
        value: u8,
        n: Index,
    ) -> Result<(), MemoryFault> {
        self.mem(mem_index)
            .and_then(|mem| mem.fill(offset, n, value))
    }

    pub(crate) fn mem_copy(
        &self,
        mem_index: MemoryIndex,
        dest: Index,
        src: Index,
        n: Index,
    ) -> Result<(), MemoryFault> {
        self.mem(mem_index).and_then(|mem| mem.copy(src, dest, n))
    }

    pub(crate) fn mem_size(&self, mem_index: MemoryIndex) -> Result<Index, MemoryFault> {
        self.virtual_machine
            .store
            .memory
            .get(mem_index.0)
            .ok_or_else(MemoryFault::new)
            .map(|mem| mem.size())
    }

    pub(crate) fn peek<T: ValueInner>(&self) -> Option<&WordAlign<T>> {
        self.stack.peek()
    }

    pub(crate) fn pop<T: ValueInner>(&mut self) -> Option<T> {
        self.stack.pop()
    }

    pub(crate) fn push<T: ValueInner>(&mut self, value: T) {
        self.stack.push(value)
    }
}
