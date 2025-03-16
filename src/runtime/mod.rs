use crate::expression::{Expression, FunctionBody, WasmCompilationContext};
use crate::parser::{
    Data, DataIndex, Element, ExportDescription, ExternIndex, FunctionIndex, GlobalIndex,
    GlobalType, ImportDescription, InitMode, LocalIndex, MemoryArgument, MemoryIndex, NumericType,
    ReferenceType, TableIndex, TableType, TypeIndex, TypeInfo, ValueType, WasmBinary, WasmSections,
    WasmVersion,
};
use crate::runtime::memory_buffer::{MemoryBuffer, MemoryError, MemoryFault, OutOfMemory};
use crate::runtime::parameter::{FunctionInput, FunctionOutput};
use crate::vector::{Index, WasmVec, vector_from_vec};
use crate::{Stack as _, invalid_data};
use bytemuck::Pod;
use crossbeam::atomic::AtomicCell;
use std::borrow::Cow;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;
use std::panic::AssertUnwindSafe;
use std::sync::atomic::{AtomicU64, Ordering};
use thiserror::Error;

pub mod memory_buffer;
pub mod parameter;
mod wasi_snapshot_preview1;

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
pub enum ValueBits {
    I32(u32),
    I64(u64),
    V128(u128),
}

impl Value {
    pub fn new(ty: ValueType) -> Value {
        match ty {
            ValueType::NumericType(NumericType::I32) => Value::I32(0),
            ValueType::NumericType(NumericType::I64) => Value::I64(0),
            ValueType::NumericType(NumericType::F32) => Value::F32(0.0),
            ValueType::NumericType(NumericType::F64) => Value::F64(0.0),
            ValueType::NumericType(NumericType::V128) => Value::V128(0),
            ValueType::ReferenceType(ReferenceType::Function) => {
                Value::Ref(ReferenceValue::Function(FunctionIndex::NULL))
            }
            ValueType::ReferenceType(ReferenceType::Extern) => {
                Value::Ref(ReferenceValue::Extern(ExternIndex::NULL))
            }
        }
    }

    pub fn r#type(&self) -> ValueType {
        match self {
            Value::I32(_) => ValueType::NumericType(NumericType::I32),
            Value::I64(_) => ValueType::NumericType(NumericType::I64),
            Value::F32(_) => ValueType::NumericType(NumericType::F32),
            Value::F64(_) => ValueType::NumericType(NumericType::F64),
            Value::V128(_) => ValueType::NumericType(NumericType::V128),
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

pub(crate) trait ValueInner: Pod + Send + Sync + Debug {
    const TYPE: ValueType;

    fn into(self) -> Value;
    fn from(data: Value) -> Option<Self>;
    fn from_ref(data: &Value) -> Option<&Self>;
}

macro_rules! impl_numeric_value {
    ($($variant: ident => $ty:ty $(; $sty: ty)?),+ $(,)?) => {$(
        impl ValueInner for $ty {
            const TYPE: ValueType = ValueType::NumericType(NumericType::$variant);

            fn into(self) -> Value {
                Value::$variant(self)
            }

            fn from(data: Value) -> Option<Self> {
                match data {
                    Value::$variant(inner) => Some(inner),
                    _ => None
                }
            }

            fn from_ref(data: &Value) -> Option<&Self> {
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

            fn from_ref(data: &Value) -> Option<&Self> {
                <$ty as ValueInner>::from_ref(data).map(bytemuck::must_cast_ref)
            }
        }
        )?
    )+};
}

macro_rules! impl_reference_value {
    ($($variant:ident => $ty:ty),+ $(,)?) => {$(
        impl ValueInner for $ty {
            const TYPE: ValueType = ValueType::ReferenceType(ReferenceType::$variant);

            fn into(self) -> Value {
                Value::Ref(ReferenceValue::$variant(self))
            }

            fn from(data: Value) -> Option<Self> {
                match data {
                    Value::Ref(ReferenceValue::$variant(inner)) => Some(inner),
                    _ => None
                }
            }

            fn from_ref(data: &Value) -> Option<&Self> {
                match data {
                    Value::Ref(ReferenceValue::$variant(inner)) => Some(inner),
                    _ => None
                }
            }
        }
    )+};
}

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

    fn from_ref(data: &Value) -> Option<&Self> {
        <u32 as ValueInner>::from_ref(data).map(bytemuck::must_cast_ref)
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
            (true, ValueType::NumericType(NumericType::I32)) => {
                Self::Mutable64(AtomicU64::new(0), Mut64Type::I32)
            }
            (true, ValueType::NumericType(NumericType::I64)) => {
                Self::Mutable64(AtomicU64::new(0), Mut64Type::I64)
            }
            (true, ValueType::NumericType(NumericType::F32)) => {
                Self::Mutable64(AtomicU64::new(f32::to_bits(0.0) as u64), Mut64Type::F32)
            }
            (true, ValueType::NumericType(NumericType::F64)) => {
                Self::Mutable64(AtomicU64::new(f64::to_bits(0.0)), Mut64Type::F64)
            }
            (true, ValueType::ReferenceType(ref_ty)) => {
                Self::Mutable64(AtomicU64::new(u32::MAX as u64), Mut64Type::Ref(ref_ty))
            }
            (true, ValueType::NumericType(NumericType::V128)) => {
                Self::Mutable128(AtomicCell::new(0))
            }
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
                    Mut64Type::I32 => NumericType::I32.into(),
                    Mut64Type::F32 => NumericType::F32.into(),
                    Mut64Type::I64 => NumericType::I64.into(),
                    Mut64Type::F64 => NumericType::F64.into(),
                };
                GlobalType {
                    mutable: true,
                    value_type,
                }
            }
            GlobalValueInner::Mutable128(_) => GlobalType {
                mutable: true,
                value_type: NumericType::V128.into(),
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

type NativeFunction = dyn Fn(&mut WasmContext) -> Result<(), Trap> + Send + Sync;

#[expect(dead_code)]
struct ImportedFunction {
    module: Box<str>,
    name: Box<str>,
    body: Box<NativeFunction>,
}

struct WasmFunction {
    locals: WasmVec<ValueType>,
    body: FunctionBody,
}

enum Body {
    WasmDefined(WasmFunction),
    Import(ImportedFunction),
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

pub struct WasmVirtualMachine {
    types: WasmVec<TypeInfo>,
    functions: WasmVec<FunctionInner>,
    tables: WasmVec<Table>,
    memory: WasmVec<MemoryBuffer>,
    globals: WasmVec<GlobalValueInner>,
    data: WasmVec<Data>,
    element: WasmVec<Element>,
    start: Option<FunctionIndex>,
    exports: HashMap<Box<str>, ExportDescription>,
    options: VirtualMachineOptions,
}

type SubTypeCheck = fn(&[ValueType]) -> bool;

struct NativeFunctionSignature {
    input: SubTypeCheck,
    output: SubTypeCheck,
}

enum ImportInner {
    Function(Box<NativeFunction>, NativeFunctionSignature),
    Global(Value),
    Memory(MemoryBuffer),
}

pub struct Import(ImportInner);

impl Import {
    pub fn function<In: FunctionInput, Out: FunctionOutput>(
        fun: impl Fn(In) -> Out + Send + Sync + 'static,
    ) -> Self {
        Import::function_full(move |input, _| fun(input))
    }

    pub fn function_with_mem<In: FunctionInput, Out: FunctionOutput>(
        fun: impl Fn(In, &MemoryBuffer) -> Out + Send + Sync + 'static,
    ) -> Self {
        Import::function_full(move |input, context| {
            fun(
                input,
                context
                    .mem(MemoryIndex::ZERO)
                    .expect("wasm module didn't contain memory"),
            )
        })
    }

    pub(crate) fn function_full<In: FunctionInput, Out: FunctionOutput>(
        fun: impl Fn(In, &mut WasmContext) -> Out + Send + Sync + 'static,
    ) -> Self {
        let function = Box::new(move |context: &mut WasmContext| {
            std::panic::catch_unwind(AssertUnwindSafe(|| {
                let input = In::get(context.stack);
                let output = fun(input, context);
                Out::push(output, context.stack)
            }))
            .map_err(|_| Trap::new())
        });
        let signature = NativeFunctionSignature {
            input: In::subtype,
            output: Out::subtype,
        };

        Import(ImportInner::Function(function, signature))
    }

    pub fn global(value: Value) -> Self {
        Import(ImportInner::Global(value))
    }

    pub fn memory(mem: MemoryBuffer) -> Self {
        Import(ImportInner::Memory(mem))
    }
}

struct Resolve {
    import: ImportInner,
    prelude: bool,
}

type Constructor = Box<dyn FnOnce(&mut WasmVirtualMachine) -> anyhow::Result<()>>;

fn validate_vm(vm: &WasmVirtualMachine) -> anyhow::Result<()> {
    if vm
        .start
        .is_some_and(|func| vm.get_typed_function::<(), ()>(func).is_err())
    {
        return Err(invalid_data("invalid start function index"));
    }

    for (export_name, &desc) in vm.exports.iter() {
        let is_valid = match desc {
            ExportDescription::Function(function) => vm.get_function(function).is_some(),
            ExportDescription::Memory(memory) => vm.get_memory(memory).is_some(),
            ExportDescription::Table(table) => vm.get_table(table).is_some(),
            ExportDescription::Global(global) => vm.get_global(global).is_some(),
        };
        if !is_valid {
            return Err(invalid_data(format!("invalid export {export_name}")));
        }
    }

    Ok(())
}

impl WasmVirtualMachine {
    #[allow(clippy::too_many_arguments)]
    fn create(
        types: WasmVec<TypeInfo>,
        functions: impl IntoIterator<Item = FunctionInner>,
        tables: impl IntoIterator<Item = Table>,
        memory: impl IntoIterator<Item = MemoryBuffer>,
        exports: HashMap<Box<str>, ExportDescription>,
        global_stubs: impl IntoIterator<Item = GlobalValueInner>,
        data: impl IntoIterator<Item = Data>,
        element: impl IntoIterator<Item = Element>,
        start: Option<FunctionIndex>,
        constructors: impl IntoIterator<Item = Constructor>,
    ) -> anyhow::Result<Self> {
        fn collect_wasm_vec<T>(it: impl IntoIterator<Item = T>) -> anyhow::Result<WasmVec<T>> {
            WasmVec::try_from(it.into_iter().collect::<Box<[_]>>())
                .map_err(|_| invalid_data("too many functions in store"))
        }

        let functions = collect_wasm_vec(functions)?;

        if let Some(FunctionIndex(start)) = start {
            if functions.get(start).is_none() {
                return Err(invalid_data(format!("invalid start index {}", start.0)));
            }
        }

        let mut this = WasmVirtualMachine {
            types,
            functions,
            tables: collect_wasm_vec(tables)?,
            memory: collect_wasm_vec(memory)?,
            exports,
            globals: collect_wasm_vec(global_stubs)?,
            data: collect_wasm_vec(data)?,
            element: collect_wasm_vec(element)?,
            start,
            options: VirtualMachineOptions::default(),
        };

        for constructor in constructors {
            constructor(&mut this)?
        }

        validate_vm(&this)?;

        Ok(this)
    }

    fn with_resolver(
        sections: WasmSections,
        mut imports: HashMap<(Cow<str>, Cow<str>), Resolve>,
    ) -> anyhow::Result<Self> {
        let types = sections.r#type.map(|sec| sec.functions).unwrap_or_default();

        let wasm_function_types = sections
            .function
            .map(|fun| fun.signatures)
            .unwrap_or_default();
        let wasm_defined_functions = sections.code.map(|fun| fun.definitions).unwrap_or_default();
        if wasm_function_types.len() != wasm_defined_functions.len() {
            return Err(invalid_data("mismatched function signatures and functions"));
        }

        let import_stubs = sections.import.map(|fun| fun.imports).unwrap_or_default();

        let (imported_functions, imported_globals, _) = import_stubs
            .into_iter()
            .map(|imp| {
                let (module, name) = (imp.module, imp.name);
                let key = (Cow::Owned(module.into()), Cow::Owned(name.into()));
                let import = imports.remove(&key).ok_or_else(|| {
                    invalid_data(format!("unresolved import [{}]::[{}]", key.0, key.1))
                })?;

                let (module, name) = (key.0.into_owned().into(), key.1.into_owned().into());
                Ok(match (imp.description, import.import) {
                    (ImportDescription::Function(r#type), ImportInner::Function(body, signature)) => {
                        let Some(import_type) = types.get(r#type.0) else {
                            return Err(invalid_data("invalid import type index"));
                        };

                        if !(signature.input)(&import_type.parameters) || !(signature.output)(&import_type.result) {
                            return Err(invalid_data(format!(
                                "invalid function [{module}]::[{name}] signature, expected {import_type}"
                            )));
                        }

                        let func = FunctionInner {
                            r#type,
                            body: Body::Import(ImportedFunction { module, name, body }),
                        };
                        (Some(func), None, None)
                    }
                    (ImportDescription::Global(_), ImportInner::Global(value)) => {
                        (None, Some(GlobalValueInner::Immutable(value)), None)
                    }
                    (ImportDescription::Memory(imposed_limit), ImportInner::Memory(buffer)) => {
                        if imposed_limit.min > buffer.min() && imposed_limit.max > buffer.max() {
                            return Err(invalid_data("invalid memory buffer signature"));
                        }
                        (None, None, Some(buffer))
                    }
                    _ => return Err(invalid_data("mismatched import type")),
                })
            })
            .collect::<anyhow::Result<(Vec<_>, Vec<_>, Vec<_>)>>()?;

        let wasm_defined_globals = sections.global.map(|glob| glob.globals).unwrap_or_default();

        let globals_signatures = vector_from_vec(
            imported_globals
                .iter()
                .flatten()
                .map(|glob| glob.r#type())
                .chain(wasm_defined_globals.iter().map(|glob| glob.r#type))
                .collect(),
        )?;

        let function_signatures = vector_from_vec(
            imported_functions
                .iter()
                .flatten()
                .map(|func| func.r#type)
                .chain(wasm_function_types.iter().copied())
                .collect(),
        )?;

        let tables = sections
            .table
            .as_ref()
            .map(|sec| &sec.tables)
            .unwrap_or(const { &WasmVec::new() });

        let mut compiler =
            WasmCompilationContext::new(&globals_signatures, &function_signatures, &types, tables);

        let functions = imported_functions
            .into_iter()
            .flatten()
            .map(anyhow::Ok)
            .chain(
                wasm_defined_functions
                    .into_iter()
                    .zip(wasm_function_types)
                    .map(|(def, r#type)| {
                        Ok(FunctionInner {
                            r#type,
                            body: Body::WasmDefined(WasmFunction {
                                locals: def.locals.clone(),
                                body: FunctionBody::new(def, r#type, &mut compiler)?,
                            }),
                        })
                    }),
            );

        let functions = vector_from_vec(functions.collect::<anyhow::Result<_>>()?)?;

        let (global_constructors, global_stubs) = imported_globals
            .into_iter()
            .flatten()
            .map(|val| (None, val))
            .chain(
                wasm_defined_globals
                    .into_iter()
                    .map(|glob| (Some(glob.expression), GlobalValueInner::new(glob.r#type))),
            )
            .unzip::<_, _, Vec<_>, Vec<_>>();

        let global_constructors = global_constructors
            .into_iter()
            .enumerate()
            .filter_map(|(i, expr)| expr.map(|expr| (i, expr)));

        let global_setter = |this: &mut WasmVirtualMachine| {
            for (i, global_constructor) in global_constructors {
                global_constructor
                    .const_eval(this)
                    .ok_or_else(|| invalid_data("global constructor not available in const"))
                    .and_then(|new_value| {
                        (*this.globals)[i].store_mut(new_value).map_err(|_| {
                            invalid_data("Global initialization failed, invalid return")
                        })
                    })?
            }

            Ok(())
        };

        for ((module, name), resolve) in imports {
            if !resolve.prelude {
                eprintln!("warning unused import: [{module}]::[{name}]")
            }
        }

        let _const_eval_offset = |this: &WasmVirtualMachine, offset: &Expression| {
            offset
                .const_eval(this)
                .ok_or_else(|| invalid_data("active element offset not available in const"))
                .and_then(|val| {
                    <Index as ValueInner>::from(val)
                        .ok_or_else(|| invalid_data("invalid offset type"))
                })
        };

        macro_rules! init_active_segment {
            ($this: expr, $name:ident, |$index:ident, $offset:ident| $expr: expr) => {{
                for $name in $this.$name.iter() {
                    let InitMode::Active {
                        index: $index,
                        offset: ref $offset,
                    } = $name.mode
                    else {
                        continue;
                    };

                    let $offset = _const_eval_offset($this, $offset)?;

                    $expr
                }
                Ok(())
            }};
        }

        let memory_setter = move |this: &mut WasmVirtualMachine| {
            init_active_segment!(this, data, |memory_index, offset| {
                let buff = this
                    .memory
                    .get(memory_index)
                    .ok_or_else(|| invalid_data("invalid memory index offset"))?;

                buff.init(offset, &data.init).map_err(|_| {
                    invalid_data("invalid memory operation performed by active segment")
                })?;
            })
        };

        let table_setter = move |this: &mut WasmVirtualMachine| {
            init_active_segment!(this, element, |table_index, offset| {
                let values = element
                    .init
                    .iter()
                    .map(|expr| match expr.const_eval(this) {
                        Some(Value::Ref(ReferenceValue::Function(func))) => Some(func),
                        _ => None,
                    })
                    .collect::<Option<Vec<_>>>()
                    .ok_or_else(|| invalid_data("invalid element initialization"))?;

                let table = this
                    .tables
                    .get_mut(table_index)
                    .and_then(|table| match table {
                        Table::FunctionTable(table) => Some(&mut **table),
                        _ => None,
                    })
                    .ok_or_else(|| invalid_data("invalid table index"))?;

                let table = table
                    .get_mut(offset.as_usize()..)
                    .ok_or_else(|| invalid_data("invalid offset"))?;
                let len = values.len().min(table.len());
                let table = &mut table[..len];
                table.copy_from_slice(&values)
            })
        };

        let oom_to_error = |_| invalid_data("wasm module requires too much memory");

        Self::create(
            types,
            functions,
            sections
                .table
                .map(|sec| sec.tables.try_map(Table::try_from))
                .transpose()
                .map_err(oom_to_error)?
                .unwrap_or_default(),
            sections
                .memory
                .map(|sec| sec.memories.try_map(MemoryBuffer::new))
                .transpose()
                .map_err(oom_to_error)?
                .unwrap_or_default(),
            sections
                .export
                .map(|sec| {
                    sec.exports
                        .into_iter()
                        .map(|exp| (exp.name, exp.description))
                        .collect()
                })
                .unwrap_or_default(),
            global_stubs,
            sections.data.map(|data| data.data).unwrap_or_default(),
            sections
                .element
                .map(|element| element.elements)
                .unwrap_or_default(),
            sections.start,
            [
                Box::new(global_setter) as Constructor,
                Box::new(memory_setter) as Constructor,
                Box::new(table_setter) as Constructor,
            ],
        )
    }

    pub fn with_imports<'a, S1: Into<Cow<'a, str>>, S2: Into<Cow<'a, str>>>(
        binary: WasmBinary,
        imports: impl IntoIterator<Item = ((S1, S2), Import)>,
    ) -> anyhow::Result<Self> {
        let prelude_imports = match binary.version {
            WasmVersion::Version1 => {
                wasi_snapshot_preview1::import_object()
                    .into_iter()
                    .map(|(name, import)| {
                        (
                            (Cow::Borrowed("wasi_snapshot_preview1"), Cow::Borrowed(name)),
                            Resolve {
                                import: import.0,
                                prelude: true,
                            },
                        )
                    })
            }
        };

        let imports = imports.into_iter().map(|((s1, s2), i)| {
            ((s1.into(), s2.into()), {
                Resolve {
                    import: i.0,
                    prelude: false,
                }
            })
        });

        let mut imports_object = HashMap::new();
        for ((module, name), import) in prelude_imports.chain(imports) {
            match imports_object.entry((module, name)) {
                Entry::Occupied(entry) => {
                    let (module, name) = entry.key();
                    return Err(invalid_data(format!(
                        "duplicate import key [{module}]::[{name}]"
                    )));
                }
                Entry::Vacant(entry) => {
                    entry.insert(import);
                }
            }
        }

        Self::with_resolver(binary.sections, imports_object)
    }

    pub fn new(binary: WasmBinary) -> anyhow::Result<Self> {
        Self::with_imports::<&str, &str>(binary, [])
    }
}

impl WasmVirtualMachine {
    pub(crate) fn load_global(&self, local: GlobalIndex) -> Option<Value> {
        self.globals.get(local.0).map(|glob| glob.load())
    }

    pub(crate) fn store_global(&self, local: GlobalIndex, value: Value) -> Result<(), ()> {
        self.globals
            .get(local.0)
            .ok_or(())
            .and_then(|glob| glob.store(value))
    }

    pub(crate) fn call_unchecked(
        &self,
        function: &FunctionInner,
        stack: &mut Vec<Value>,
        call_depth: usize,
    ) -> Result<(), Trap> {
        let locals = match &function.body {
            Body::WasmDefined(wasm_func) => {
                let params = &*self
                    .types
                    .get(function.r#type.0)
                    .expect("function should have valid type info")
                    .parameters;

                let params_iter = params.iter();
                let params = stack.drain(stack.len() - params.len()..);

                debug_assert!(
                    params_iter
                        .copied()
                        .eq(params.as_slice().iter().map(|val| val.r#type()))
                );

                let locals = params
                    .chain(wasm_func.locals.iter().map(|&ty| Value::new(ty)))
                    .collect::<Box<[_]>>();
                WasmVec::from_trusted_box(locals)
            }
            Body::Import(_) => const { WasmVec::new() },
        };

        let mut context = WasmContext {
            virtual_machine: self,
            locals,
            stack,
            call_depth,
        };

        match &function.body {
            Body::WasmDefined(func) => func.body.eval(&mut context),
            Body::Import(imp) => (imp.body)(&mut context),
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
            .exports
            .get($param)
            .and_then(|&interface| match interface {
                ExportDescription::$name(idx) => Some(idx),
                _ => None,
            })
    };
}

pub struct UntypedFunction<'a> {
    vm: &'a WasmVirtualMachine,
    function: &'a FunctionInner,
}

pub struct Function<'a, T, U> {
    vm: &'a WasmVirtualMachine,
    function: &'a FunctionInner,
    _marker: PhantomData<fn(T) -> U>,
}

#[derive(Debug, Error)]
#[error("wasm execution trapped")]
pub struct Trap(());

impl Trap {
    #[cold]
    pub(crate) fn new() -> Self {
        Trap(())
    }
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
        let r#type = self.vm.types.get(self.function.r#type.0).unwrap();

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
                self.$collection.get(index.0)$(.map(|x| ($map_expr)(self, x)))?
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

impl WasmVirtualMachine {
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
        let Some(function) = self.get_function(function) else {
            return Err(GetFunctionError::FunctionDoesntExist);
        };

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
        if let Some(start) = self.start {
            return self.call(start, ());
        }

        self.call_by_name("main", ())
    }
}

pub struct WasmContext<'a> {
    pub(crate) virtual_machine: &'a WasmVirtualMachine,
    pub(crate) locals: WasmVec<Value>,
    pub(crate) stack: &'a mut Vec<Value>,
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
        eprintln!("maximum recursion depth reached");
        Err(Trap::new())
    }

    pub(crate) fn get_function(&self, function: FunctionIndex) -> Option<&'a FunctionInner> {
        self.virtual_machine.functions.get(function.0)
    }

    pub(crate) fn get_local(&mut self, local: LocalIndex) -> Option<&mut Value> {
        self.locals.get_mut(local.0)
    }

    pub(crate) fn table_load(
        &self,
        table_index: TableIndex,
        index: Index,
    ) -> Option<ReferenceValue> {
        self.virtual_machine
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
            .memory
            .get(mem_index.0)
            .ok_or_else(MemoryFault::new)
            .map(|mem| mem.size())
    }

    pub(crate) fn peek(&self) -> Option<&Value> {
        self.stack.last()
    }

    pub(crate) fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }

    pub(crate) fn pop_n<const N: usize>(&mut self) -> Option<[Value; N]> {
        self.stack.pop_n()
    }

    pub(crate) fn push(&mut self, value: Value) {
        self.stack.push(value)
    }
}
