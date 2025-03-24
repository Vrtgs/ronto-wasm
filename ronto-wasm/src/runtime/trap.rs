use std::error::Error;
use std::fmt::Debug;
use thiserror::Error;

#[derive(Debug, Error)]
#[error("wasm execution trapped; {0}")]
pub struct Trap(anyhow::Error);

impl Default for Trap {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Error)]
#[error("explicit trap")]
pub struct ExplicitTrap(());

#[derive(Debug, Error)]
#[error("attempted to divide by zero")]
pub struct DivideByZero(());

#[derive(Debug, Error)]
#[error("invalid memory access")]
pub struct MemoryFault(());

#[derive(Debug, Error)]
#[error("maximum recursion depth reached")]
pub struct MaximumRecursionDepth(());

impl Trap {
    #[cold]
    pub fn new() -> Self {
        Self::custom(ExplicitTrap(()))
    }

    #[cold]
    pub fn divide_by_zero() -> Self {
        Self::custom(DivideByZero(()))
    }

    #[cold]
    pub fn memory_fault() -> Self {
        Self::custom(MemoryFault(()))
    }

    #[cold]
    pub fn maximum_recursion_depth() -> Self {
        Self::custom(MaximumRecursionDepth(()))
    }

    #[cold]
    pub fn custom(error: impl Error + Send + Sync + 'static) -> Self {
        Trap(anyhow::Error::new(error))
    }

    pub fn is<E: Error + Debug + Send + Sync + 'static>(&self) -> bool {
        self.0.is::<E>()
    }

    pub fn downcast_ref<E: Error + Send + Sync + 'static>(&self) -> Option<&E> {
        self.0.downcast_ref()
    }

    pub fn downcast_mut<E: Error + Send + Sync + 'static>(&mut self) -> Option<&mut E> {
        self.0.downcast_mut()
    }

    pub fn downcast<E: Error + Send + Sync + 'static>(self) -> Result<E, Self> {
        self.0.downcast().map_err(Self)
    }

    pub fn backtrace(&self) -> &std::backtrace::Backtrace {
        self.0.backtrace()
    }
}
