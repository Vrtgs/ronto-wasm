use crate::instruction::ExecutionError;
use crate::parser::{
    Data, ExportDescription, Expression, ExternIndex, FunctionIndex, GlobalIndex, GlobalType,
    ImportDescription, LabelIndex, LocalIndex, MemoryArgument, MemoryIndex, NumericType,
    RefrenceType, TableIndex, TableValue, TypeIndex, TypeInfo, ValueType, WasmBinary,
};
use crate::runtime::memory_buffer::MemoryBuffer;
use crate::vector::{Index, WasmVec};
use crate::{Stack as _, invalid_data};
use bytemuck::Pod;
use crossbeam::atomic::AtomicCell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io;
use std::marker::PhantomData;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

mod memory_buffer;

pub use memory_buffer::{MemoryError, MemoryFault};

#[derive(Debug, Copy, Clone)]
pub enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    V128(i128),
    FunctionRef(FunctionIndex),
    ExternRef(ExternIndex),
}

impl Value {
    pub fn new(ty: ValueType) -> Value {
        match ty {
            ValueType::NumericType(NumericType::I32) => Value::I32(0),
            ValueType::NumericType(NumericType::I64) => Value::I64(0),
            ValueType::NumericType(NumericType::F32) => Value::F32(0.0),
            ValueType::NumericType(NumericType::F64) => Value::F64(0.0),
            ValueType::NumericType(NumericType::V128) => Value::V128(0),
            ValueType::RefrenceType(RefrenceType::FunctionRef) => {
                Value::FunctionRef(FunctionIndex::NULL)
            }
            ValueType::RefrenceType(RefrenceType::ExternRef) => Value::ExternRef(ExternIndex::NULL),
        }
    }

    pub fn r#type(&self) -> ValueType {
        match self {
            Value::I32(_) => ValueType::NumericType(NumericType::I32),
            Value::I64(_) => ValueType::NumericType(NumericType::I64),
            Value::F32(_) => ValueType::NumericType(NumericType::F32),
            Value::F64(_) => ValueType::NumericType(NumericType::F64),
            Value::V128(_) => ValueType::NumericType(NumericType::V128),
            Value::FunctionRef(_) => ValueType::RefrenceType(RefrenceType::FunctionRef),
            Value::ExternRef(_) => ValueType::RefrenceType(RefrenceType::ExternRef),
        }
    }
}

pub trait ValueInner: Pod + Send + Sync {
    fn into(self) -> Value;
    fn r#type() -> ValueType;
    fn from(data: Value) -> Option<Self>;
    fn from_ref(data: &Value) -> Option<&Self>;
    fn from_mut(data: &mut Value) -> Option<&mut Self>;
}

macro_rules! execute_expr {
    ($expr:expr, $context: ident) => {
        'exec_res: {
            for instruction in ($expr).instructions.iter() {
                match instruction.execute(&mut $context) {
                    Ok(()) => (),
                    Err(ExecutionError::Unwind(_)) => break 'exec_res Ok(()),
                    Err(ExecutionError::Trap) => break 'exec_res Err(()),
                }
            }
            Ok(())
        }
    };
}

macro_rules! impl_value {
    ($($type_variant:ident $variant: ident => $ty:ty $(; $uty: ty)?),+ $(,)?) => {$(
        impl ValueInner for $ty {
            fn into(self) -> Value {
                Value::$variant(self)
            }

            fn r#type() -> ValueType {
                paste::paste! { ValueType::[<$type_variant Type>]([<$type_variant Type>]::$variant) }
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


            fn from_mut(data: &mut Value) -> Option<&mut Self> {
                match data {
                    Value::$variant(inner) => Some(inner),
                    _ => None
                }
            }
        }
        $(
        impl ValueInner for $uty {
            fn into(self) -> Value {
                Value::$variant(self as $ty)
            }

            fn r#type() -> ValueType {
                paste::paste! { ValueType::[<$type_variant Type>]([<$type_variant Type>]::$variant) }
            }

            fn from(data: Value) -> Option<Self> {
                <$ty as ValueInner>::from(data).map(|inner| inner as $uty)
            }

            fn from_ref(data: &Value) -> Option<&Self> {
                <$ty as ValueInner>::from_ref(data).map(bytemuck::must_cast_ref)
            }

            fn from_mut(data: &mut Value) -> Option<&mut Self> {
                <$ty as ValueInner>::from_mut(data).map(bytemuck::must_cast_mut)
            }
        }
        )?
    )+};
}

impl_value! {
    Numeric I32  =>  i32; u32,
    Numeric I64  =>  i64; u64,
    Numeric F32  =>  f32,
    Numeric F64  =>  f64,
    Numeric V128 => i128; u128,
    Refrence FunctionRef => FunctionIndex,
    Refrence ExternRef => ExternIndex,
}

#[derive(Copy, Clone)]
enum Mut64Type {
    Ref(RefrenceType),
    I32,
    F32,
    I64,
    F64,
}

enum GlobalValue {
    Immutable(Value),
    Mutable64(AtomicU64, Mut64Type),
    Mutable128(AtomicCell<i128>),
}

impl GlobalValue {
    fn new(value: GlobalType) -> Self {
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
            (true, ValueType::RefrenceType(ref_ty)) => {
                Self::Mutable64(AtomicU64::new(u32::MAX as u64), Mut64Type::Ref(ref_ty))
            }
            (true, ValueType::NumericType(NumericType::V128)) => {
                Self::Mutable128(AtomicCell::new(0))
            }
        }
    }
}

struct Import {
    name: Box<str>,
    body: Box<dyn Fn(&mut WasmContext) -> Result<(), ()>>,
}

struct WasmFunction {
    locals: WasmVec<ValueType>,
    body: Expression,
}

enum Body {
    WasmDefined(WasmFunction),
    Import(Import),
}

struct Function {
    r#type: TypeIndex,
    body: Body,
}

impl Function {
    fn wasm(&self) -> Option<&WasmFunction> {
        match self.body {
            Body::WasmDefined(ref func) => Some(func),
            _ => None,
        }
    }
}

pub struct WasmEnvironment {
    types: WasmVec<TypeInfo>,
    functions: WasmVec<Function>,
    tables: WasmVec<TableValue>,
    memory: WasmVec<MemoryBuffer>,
    globals: WasmVec<GlobalValue>,
    data: WasmVec<Data>,
    start: Option<FunctionIndex>,
    exports: HashMap<Box<str>, ExportDescription>,
}

impl WasmEnvironment {
    fn new(
        bin: WasmBinary,
        mut imports: HashMap<String, Box<dyn Fn(&mut WasmContext) -> Result<(), ()>>>,
    ) -> io::Result<Self> {
        let sections = dbg!(bin).sections;

        let function_types = sections
            .function
            .map(|fun| fun.signatures)
            .unwrap_or_default();
        let wasm_functions = sections.code.map(|fun| fun.definitions).unwrap_or_default();
        if function_types.len() != wasm_functions.len() {
            return Err(invalid_data("mismatched function signatures and functions"));
        }

        let import_stubs = sections.import.map(|fun| fun.imports).unwrap_or_default();

        let functions = import_stubs
            .into_iter()
            .map(|imp| {
                let name = format!("{module}::{name}", module = imp.module, name = imp.name)
                    .into_boxed_str();
                let body = imports.remove(&*name).unwrap_or_else(|| {
                    let name = name.clone();
                    Box::new(move |_| {
                        eprintln!("unresolved import ({name})");
                        Err(())
                    })
                });
                Function {
                    r#type: match imp.description {
                        ImportDescription::Function(r#type) => TypeIndex(r#type.0),
                        _ => unreachable!(),
                    },
                    body: Body::Import(Import { name, body }),
                }
            })
            .chain(
                wasm_functions
                    .into_iter()
                    .zip(function_types)
                    .map(|(def, r#type)| Function {
                        r#type,
                        body: Body::WasmDefined(WasmFunction {
                            locals: def.locals,
                            body: def.body,
                        }),
                    }),
            )
            .collect::<Box<[_]>>();

        let (global_constructors, global_stubs) = sections
            .global
            .map(|glob| {
                glob.globals
                    .into_iter()
                    .map(|glob| (glob.expression, GlobalValue::new(glob.r#type)))
                    .unzip::<_, _, Vec<_>, Vec<_>>()
            })
            .unwrap_or_default();

        let mut this = WasmEnvironment {
            types: sections.r#type.map(|sec| sec.functions).unwrap_or_default(),
            functions: WasmVec::from_trusted_box(functions),
            tables: sections
                .table
                .map(|tables| tables.tables)
                .unwrap_or_default(),
            memory: sections
                .memory
                .map(|sec| sec.memories.try_map(MemoryBuffer::new))
                .transpose()
                .unwrap()
                .unwrap_or_default(),
            exports: sections
                .export
                .map(|sec| {
                    sec.exports
                        .into_iter()
                        .map(|export| (export.name, export.description))
                        .collect()
                })
                .unwrap_or_default(),
            globals: WasmVec::from_trusted_box(global_stubs.into()),
            data: sections.data.map(|sec| sec.data).unwrap_or_default(),
            start: sections.start,
        };

        let mut globals_stack = vec![];
        for (i, global_constructor) in global_constructors.into_iter().enumerate() {
            let mut context = WasmContext {
                environment: &this,
                locals: const { WasmVec::new() },
                stack: Stack::Parent(&mut globals_stack),
            };
            execute_expr!(global_constructor, context)
                .map_err(|_| invalid_data("Global initialization trapped"))?;

            globals_stack
                .pop()
                .and_then(|x| globals_stack.is_empty().then_some(x))
                .and_then(|new_value| {
                    let global = &mut (*this.globals)[i];
                    match *global {
                        GlobalValue::Immutable(ref mut val) => {
                            if val.r#type() != new_value.r#type() {
                                return None;
                            }
                            *val = new_value;
                        }
                        GlobalValue::Mutable64(ref mut loc, ty) => {
                            let loc = loc.get_mut();
                            match (new_value, ty) {
                                (Value::I64(bits), Mut64Type::I64) => *loc = bits as u64,
                                (Value::F64(bits), Mut64Type::F64) => *loc = bits.to_bits(),
                                (Value::F32(bits), Mut64Type::F32) => *loc = bits.to_bits() as u64,
                                (Value::I32(bits), Mut64Type::I32) => *loc = bits as u32 as u64,
                                (
                                    Value::ExternRef(ExternIndex(index)),
                                    Mut64Type::Ref(RefrenceType::ExternRef),
                                )
                                | (
                                    Value::FunctionRef(FunctionIndex(index)),
                                    Mut64Type::Ref(RefrenceType::FunctionRef),
                                ) => *loc = index.0 as u64,
                                _ => return None,
                            }
                        }
                        GlobalValue::Mutable128(ref mut loc) => {
                            let Value::V128(val_128) = new_value else {
                                return None;
                            };
                            *loc = AtomicCell::new(val_128);
                        }
                    }
                    Some(())
                })
                .ok_or_else(|| invalid_data("Global initialization failed, invalid return"))?;
        }

        Ok(this)
    }
}

#[derive(Debug)]
pub enum CallByNameError {
    Trap,
    ExportNotFound,
    ExportTypeError,
}

impl WasmEnvironment {
    fn get_type(&self, index: TypeIndex) -> Option<&TypeInfo> {
        self.types.get(index.0)
    }

    pub fn _call(&self, function: FunctionIndex, stack: Option<&mut Vec<Value>>) -> Result<(), ()> {
        let Some(function) = self.functions.get(function.0) else {
            return Err(());
        };

        let locals = match &function.body {
            Body::WasmDefined(wasm_func) => wasm_func.locals.map_ref(|&ty| Value::new(ty)),
            Body::Import(_) => const { WasmVec::new() },
        };
        let stack = stack
            .map(Stack::Parent)
            .unwrap_or(const { Stack::Owned(vec![]) });
        let mut context = WasmContext {
            environment: self,
            locals,
            stack,
        };

        match &function.body {
            Body::WasmDefined(func) => execute_expr!(func.body, context),
            Body::Import(imp) => (imp.body)(&mut context),
        }
    }

    pub fn call(&self, function: FunctionIndex) -> Result<(), ()> {
        self._call(function, None)
    }

    pub fn call_by_name(&self, function: &str) -> Result<(), CallByNameError> {
        self.exports
            .get(function)
            .ok_or(CallByNameError::ExportNotFound)
            .and_then(|interface| match *interface {
                ExportDescription::Function(idx) => {
                    self.call(idx).map_err(|()| CallByNameError::Trap)
                }
                _ => Err(CallByNameError::ExportTypeError),
            })
    }

    pub fn start(&self) -> Result<(), CallByNameError> {
        if let Some(start) = self.start {
            return self.call(start).map_err(|()| CallByNameError::Trap);
        }

        self.call_by_name("main")
    }
}

#[derive(Debug)]
enum Stack<'a> {
    Owned(Vec<Value>),
    Parent(&'a mut Vec<Value>),
}

struct Frame {
    locals: WasmVec<ValueType>,
    labels: u32,
}

pub struct Validator<'a> {
    environment: &'a WasmEnvironment,
    verification_stack: Vec<ValueType>,
    hit_unreachable: bool,
    frames: Rc<RefCell<Vec<Frame>>>,
}

#[clippy::has_significant_drop]
pub(crate) struct FrameGuard<'a> {
    env: PhantomData<&'a WasmEnvironment>,
    guard: Rc<RefCell<Vec<Frame>>>,
}

impl Drop for FrameGuard<'_> {
    fn drop(&mut self) {
        self.guard.borrow_mut().pop();
    }
}

#[clippy::has_significant_drop]
pub(crate) struct LabelGuard<'a> {
    env: PhantomData<&'a WasmEnvironment>,
    guard: Rc<RefCell<Vec<Frame>>>,
}

impl Drop for LabelGuard<'_> {
    fn drop(&mut self) {
        self.guard.borrow_mut().last_mut().unwrap().labels -= 1;
    }
}

impl<'a> Validator<'a> {
    pub(crate) fn take_unreachable(&mut self) -> bool {
        std::mem::replace(&mut self.hit_unreachable, false)
    }

    pub(crate) fn set_unreachable(&mut self) {
        self.hit_unreachable = true;
    }

    pub(crate) fn is_unreachable(&self) -> bool {
        self.hit_unreachable
    }

    pub(crate) fn stack(&mut self) -> &mut Vec<ValueType> {
        &mut self.verification_stack
    }

    pub(crate) fn get_type_input(&self, r#type: TypeIndex) -> Option<&'a [ValueType]> {
        self.environment
            .types
            .get(r#type.0)
            .map(|ty| &*ty.parameters)
    }

    pub(crate) fn get_type_output(&self, r#type: TypeIndex) -> Option<&'a [ValueType]> {
        self.environment.types.get(r#type.0).map(|ty| &*ty.result)
    }

    pub(crate) fn pop_type_input(&mut self, r#type: TypeIndex) -> bool {
        self.get_type_input(r#type)
            .is_some_and(|ty| self.pop_slice(ty))
    }

    pub(crate) fn push_type_output(&mut self, r#type: TypeIndex) -> bool {
        self.get_type_output(r#type)
            .map(|ty| self.push_slice(ty))
            .is_some()
    }

    pub(crate) fn enter_function(&mut self, function: FunctionIndex) -> Option<FrameGuard<'a>> {
        let guard = Rc::clone(&self.frames);
        guard.borrow_mut().push(Frame {
            locals: self
                .environment
                .functions
                .get(function.0)
                .and_then(Function::wasm)?
                .locals
                .clone(),
            labels: 0,
        });

        Some(FrameGuard {
            env: PhantomData,
            guard,
        })
    }

    pub(crate) fn add_label(&mut self) -> Option<LabelGuard<'a>> {
        self.frames.borrow_mut().last_mut().map(|frame| {
            frame.labels += 1;
            let guard = Rc::clone(&self.frames);
            LabelGuard {
                env: PhantomData,
                guard,
            }
        })
    }

    pub(crate) fn contains_label(&self, label: LabelIndex) -> bool {
        Index::from_usize(self.frames.borrow().len()) > label.0
    }

    pub(crate) fn contains_table(&self, label: TableIndex) -> bool {
        self.environment.tables.get(label.0).is_some()
    }

    pub(crate) fn simulate_call(&mut self, function: FunctionIndex) -> bool {
        if self.environment.functions.get(function.0).is_some() {
            if let Some(func) = self.environment.functions.get(function.0) {
                let type_index = func.r#type;
                return self.pop_type_input(type_index) && self.push_type_output(type_index);
            }
        }
        false
    }

    pub(crate) fn get_global(&self, global: GlobalIndex) -> Option<GlobalType> {
        self.environment
            .globals
            .get(global.0)
            .map(|global| match *global {
                GlobalValue::Immutable(val) => GlobalType {
                    mutable: false,
                    value_type: val.r#type(),
                },
                GlobalValue::Mutable64(_, ty) => {
                    let value_type = match ty {
                        Mut64Type::Ref(ref_ty) => ValueType::RefrenceType(ref_ty),
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
                GlobalValue::Mutable128(_) => GlobalType {
                    mutable: true,
                    value_type: NumericType::V128.into(),
                },
            })
    }

    pub(crate) fn get_local(&self, local: LocalIndex) -> Option<ValueType> {
        self.frames
            .borrow_mut()
            .last_mut()
            .and_then(|frame| frame.locals.get(local.0).copied())
    }

    pub(crate) fn environment(&self) -> &'a WasmEnvironment {
        self.environment
    }

    pub(crate) fn peek(&mut self) -> Option<&ValueType> {
        self.stack().last()
    }

    pub(crate) fn pop(&mut self) -> Option<ValueType> {
        self.stack().pop()
    }

    pub(crate) fn pop_n<const N: usize>(&mut self) -> Option<[ValueType; N]> {
        self.stack().pop_n()
    }

    pub(crate) fn push(&mut self, value: ValueType) {
        self.stack().push(value)
    }

    pub(crate) fn push_n<const N: usize>(&mut self, data: [ValueType; N]) {
        self.stack().push_n(data)
    }

    pub(crate) fn push_slice(&mut self, data: &[ValueType]) {
        self.stack().extend_from_slice(data)
    }

    pub(crate) fn pop_slice(&mut self, data: &[ValueType]) -> bool {
        let stack = self.stack();
        if data.len() > stack.len() {
            stack.clear();
            return false;
        }
        stack
            .drain(stack.len() - data.len()..)
            .rev()
            .eq(data.iter().copied())
    }
}

pub struct WasmContext<'a> {
    pub(crate) environment: &'a WasmEnvironment,
    pub(crate) locals: WasmVec<Value>,
    stack: Stack<'a>,
}

impl WasmContext<'_> {
    pub fn call(&mut self, function: FunctionIndex) -> Result<(), ()> {
        self.environment._call(function, Some(self.stack()))
    }

    fn stack(&mut self) -> &mut Vec<Value> {
        match &mut self.stack {
            Stack::Owned(stack) => stack,
            Stack::Parent(par) => par,
        }
    }

    fn stack_ref(&self) -> &[Value] {
        match &self.stack {
            Stack::Owned(stack) => stack,
            Stack::Parent(par) => par,
        }
    }

    pub(crate) fn get_local(&mut self, local: LocalIndex) -> Option<&mut Value> {
        self.locals.get_mut(local.0)
    }

    pub(crate) fn mem_load<T: Pod>(
        &self,
        mem_index: MemoryIndex,
        argument: MemoryArgument,
        index: Index,
    ) -> Result<T, MemoryFault> {
        self.environment
            .memory
            .get(mem_index.0)
            .ok_or_else(MemoryFault::new)
            .and_then(|mem| mem.load(argument, index))
    }

    pub(crate) fn mem_store<T: Pod>(
        &self,
        mem_index: MemoryIndex,
        argument: MemoryArgument,
        index: Index,
        value: &T,
    ) -> Result<(), MemoryFault> {
        self.environment
            .memory
            .get(mem_index.0)
            .ok_or_else(MemoryFault::new)
            .and_then(|mem| mem.store(argument, index, value))
    }

    pub(crate) fn mem_grow(&self, mem_index: MemoryIndex, by: Index) -> Result<Index, MemoryError> {
        self.environment
            .memory
            .get(mem_index.0)
            .ok_or_else(|| MemoryError::MemoryFault(MemoryFault::new()))
            .and_then(|mem| mem.grow(by).map_err(MemoryError::from))
    }

    pub(crate) fn mem_size(&self, mem_index: MemoryIndex) -> Result<Index, MemoryFault> {
        self.environment
            .memory
            .get(mem_index.0)
            .ok_or_else(MemoryFault::new)
            .map(|mem| mem.size())
    }

    pub(crate) fn load_global(&self, local: GlobalIndex) -> Option<Value> {
        self.environment
            .globals
            .get(local.0)
            .map(|glob| match *glob {
                GlobalValue::Immutable(val) => val,
                GlobalValue::Mutable64(ref loc, ty) => {
                    let bits = loc.load(Ordering::Acquire);
                    match ty {
                        Mut64Type::I64 => Value::I64(bits as i64),
                        Mut64Type::F64 => Value::F64(f64::from_bits(bits)),
                        Mut64Type::I32 => Value::I32(bits as u32 as i32),
                        Mut64Type::F32 => Value::F32(f32::from_bits(bits as u32)),
                        Mut64Type::Ref(ref_ty) => {
                            let idx = Index(bits as u32);
                            match ref_ty {
                                RefrenceType::FunctionRef => Value::FunctionRef(FunctionIndex(idx)),
                                RefrenceType::ExternRef => Value::ExternRef(ExternIndex(idx)),
                            }
                        }
                    }
                }
                GlobalValue::Mutable128(ref loc) => Value::V128(loc.load()),
            })
    }

    pub(crate) fn store_global(&self, local: GlobalIndex, value: Value) -> Result<(), ()> {
        self.environment
            .globals
            .get(local.0)
            .ok_or(())
            .and_then(|glob| match *glob {
                GlobalValue::Immutable(_) => Err(()),
                GlobalValue::Mutable64(ref loc, ty) => {
                    let ord = Ordering::Release;
                    match (value, ty) {
                        (Value::I64(bits), Mut64Type::I64) => loc.store(bits as u64, ord),
                        (Value::F64(bits), Mut64Type::F64) => loc.store(bits.to_bits(), ord),
                        (Value::F32(bits), Mut64Type::F32) => loc.store(bits.to_bits() as u64, ord),
                        (Value::I32(bits), Mut64Type::I32) => loc.store(bits as u32 as u64, ord),
                        (
                            Value::ExternRef(ExternIndex(index)),
                            Mut64Type::Ref(RefrenceType::ExternRef),
                        )
                        | (
                            Value::FunctionRef(FunctionIndex(index)),
                            Mut64Type::Ref(RefrenceType::FunctionRef),
                        ) => loc.store(index.0 as u64, ord),
                        _ => return Err(()),
                    }
                    Ok(())
                }
                GlobalValue::Mutable128(ref loc) => {
                    let Value::V128(val_128) = value else {
                        return Err(());
                    };
                    loc.store(val_128);
                    Ok(())
                }
            })
    }

    pub(crate) fn peek(&self) -> Option<&Value> {
        self.stack_ref().last()
    }

    pub(crate) fn pop(&mut self) -> Option<Value> {
        self.stack().pop()
    }

    pub(crate) fn pop_n<const N: usize>(&mut self) -> Option<[Value; N]> {
        self.stack().pop_n()
    }

    pub(crate) fn push(&mut self, value: Value) {
        self.stack().push(value)
    }

    pub(crate) fn push_n<const N: usize>(&mut self, data: [Value; N]) {
        self.stack().push_n(data)
    }
}

pub fn execute(wasm: WasmBinary) {
    let mapa_hash = HashMap::from([(
        "console::log".to_string(),
        Box::new(|cntx: &mut WasmContext| {
            let Some(Value::I32(int)) = cntx.pop() else {
                unreachable!()
            };
            println!("{}", int);
            Ok(())
        }) as Box<_>,
    )]);

    let env = WasmEnvironment::new(wasm, mapa_hash).unwrap();
    env.start().unwrap()
}
