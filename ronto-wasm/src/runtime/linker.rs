use crate::parser::ValueType;
use crate::runtime::memory_buffer::MemoryBuffer;
use crate::runtime::parameter::{FunctionInput, FunctionOutput};
use crate::runtime::{Trap, Value, ValueStack};
use crate::{Index, VirtualMachine};
use std::collections::HashMap;
use std::panic::AssertUnwindSafe;
use std::sync::Arc;
use thiserror::Error;

#[derive(Debug, Error)]
#[error("namespace collision during linker creation")]
pub struct NameSpaceCollision(());

pub(super) type NativeFunction = dyn Fn(&VirtualMachine, &mut ValueStack) -> Result<(), Trap> + Send + Sync;

pub(super) type SubTypeCheck = fn(&[ValueType]) -> bool;

#[derive(Copy, Clone)]
pub(super) struct NativeFunctionSignature {
    pub(super) input_check: SubTypeCheck,
    pub(super) output_check: SubTypeCheck,
}

#[derive(Clone)]
pub(super) enum Import {
    Function(Arc<NativeFunction>, NativeFunctionSignature),
    Global(Value),
    Memory(MemoryBuffer),
}

pub struct ModuleImports {
    imports: HashMap<Arc<str>, Import>,
}

pub struct ModuleImportsBuilder<T> {
    data: Arc<T>,
    imports: HashMap<Arc<str>, Import>,
}

impl<T: 'static + Send + Sync> ModuleImportsBuilder<T> {
    pub fn new(data: T) -> Self {
        Self {
            imports: HashMap::new(),
            data: Arc::new(data),
        }
    }

    fn add(mut self, name: Arc<str>, imp: Import) -> Result<Self, NameSpaceCollision> {
        match self.imports.insert(name, imp) {
            None => Ok(self),
            Some(_) => Err(NameSpaceCollision(())),
        }
    }

    pub fn function<In: FunctionInput, Out: FunctionOutput>(
        self,
        name: impl Into<Arc<str>>,
        fun: impl Fn(&T, Option<&MemoryBuffer>, In) -> Result<Out, Trap> + Send + Sync + 'static,
    ) -> Result<Self, NameSpaceCollision> {
        let data = Arc::clone(&self.data);
        let function = Arc::new(move |vm: &VirtualMachine, stack: &mut ValueStack| {
            std::panic::catch_unwind(AssertUnwindSafe(|| {
                let input = In::get(stack);
                let mem = vm.store.memory.get(Index(0));
                let output = fun(&data, mem, input);
                output.map(|output| Out::push(output, stack))
            }))
                .map_err(|_| Trap::new())?
        });

        let signature = NativeFunctionSignature {
            input_check: In::subtype,
            output_check: Out::subtype,
        };

        self.add(name.into(), Import::Function(function, signature))
    }

    pub fn global(
        self,
        name: impl Into<Arc<str>>,
        value: Value,
    ) -> Result<Self, NameSpaceCollision> {
        self.add(name.into(), Import::Global(value))
    }

    pub fn memory(
        self,
        name: impl Into<Arc<str>>,
        mem: MemoryBuffer,
    ) -> Result<Self, NameSpaceCollision> {
        self.add(name.into(), Import::Memory(mem))
    }

    pub fn build(self) -> ModuleImports {
        ModuleImports {
            imports: self.imports,
        }
    }
}

pub struct Linker {
    modules: HashMap<Arc<str>, ModuleImports>,
}

impl Default for Linker {
    fn default() -> Self {
        Self::new()
    }
}

impl Linker {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
        }
    }

    pub fn from_modules<S: Into<Arc<str>>>(
        iter: impl IntoIterator<Item=(S, ModuleImports)>,
    ) -> Result<Self, NameSpaceCollision> {
        iter.into_iter()
            .try_fold(Linker::new(), |mut linker, (str, module)| {
                linker.add_module(str, module)?;
                Ok(linker)
            })
    }

    pub fn add_module(
        &mut self,
        module_name: impl Into<Arc<str>>,
        imports: ModuleImports,
    ) -> Result<&mut Self, NameSpaceCollision> {
        match self.modules.insert(module_name.into(), imports) {
            None => Ok(self),
            Some(_) => Err(NameSpaceCollision(())),
        }
    }

    pub(super) fn get(&self, module: &str, name: &str) -> Option<&Import> {
        self.modules
            .get(module)
            .and_then(|imports| imports.imports.get(name))
    }
}
