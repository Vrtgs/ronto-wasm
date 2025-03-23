use crate::parser;
use crate::parser::Limit;
use crate::vector::Index;
use bytemuck::Pod;
use std::cell::RefCell;
use std::ffi::{CStr, CString};
use std::fmt::Formatter;

pub const PAGE_SIZE: u32 = 65536;

macro_rules! make_memory_error {
    ($($name: ident($error:literal);)+) => {$(
        #[derive(thiserror::Error)]
        #[error($error)]
        pub struct $name {
            _priv: ()
        }

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                f.debug_tuple(stringify!($name)).field(&$error).finish()
            }
        }

        impl $name {
            #[cold]
            pub(crate) fn new() -> Self {
                Self {
                    _priv: ()
                }
            }
        }
    )+};
}

make_memory_error! {
    OutOfMemory("OOM emitted, ran out of memory during execution");
    MemoryFault("invalid memory operation");
}

#[derive(Debug, thiserror::Error)]
pub enum MemoryError {
    #[error(transparent)]
    OutOfMemory(#[from] OutOfMemory),
    #[error(transparent)]
    MemoryFault(#[from] MemoryFault),
}

pub(crate) trait MemoryArgument: Copy {
    fn offset(self) -> Index;
    fn align(self) -> usize;
}

impl MemoryArgument for parser::MemoryArgument {
    fn offset(self) -> Index {
        self.offset
    }

    fn align(self) -> usize {
        1 << self.align.0
    }
}

#[derive(Copy, Clone)]
struct UnalignedAccess;

impl MemoryArgument for UnalignedAccess {
    fn offset(self) -> Index {
        Index::ZERO
    }

    fn align(self) -> usize {
        1
    }
}

#[derive(Clone, Debug)]
pub struct MemoryBuffer {
    limit: Limit,
    buffer: RefCell<Vec<u8>>,
}

macro_rules! access {
    (&$(mut $(@$_mut:tt)?)? $buffer:expr, $memory_argument:expr, $addr:expr $(, $size:expr)?; try { $map: expr }) => {{
        paste::paste! { let buffer = & $(mut $(@$_mut)?)? ** $buffer.[<borrow $(_mut $($_mut)?)?>](); }

        let addr = $memory_argument
            .offset()
            .0
            .checked_add($addr.0)
            .map(Index)
            .ok_or_else(MemoryFault::new)?;
        
        $(let end  = addr.as_usize().checked_add($size).ok_or_else(MemoryFault::new)?;)?

        let range = addr.as_usize()..$({
            if false {
                let _ = stringify!($size);
            }
            end
        })?;

        paste::paste! { let bytes = buffer.[<get $(_mut $($_mut)?)?>](range).ok_or_else(MemoryFault::new)?;  }

        if (bytes.as_ptr().addr() % $memory_argument.align()) != 0 {
            #[cold]
            fn cold() {}
            cold()
        }

        ($map)(bytes)
    }};

    (&$(mut $(@$_mut:tt)?)? $buffer:expr, $memory_argument:expr, $addr:expr $(, $size:expr)?; $map: expr) => {{
        access!(&$(mut $(@$_mut)?)? $buffer, $memory_argument, $addr $(, $size)?; try { |buffer| Ok($map(buffer)) })
    }};
}

impl MemoryBuffer {
    pub fn new(limit: Limit) -> Result<Self, OutOfMemory> {
        let this = Self {
            limit,
            buffer: RefCell::new(Vec::new()),
        };
        if limit.min != Index::ZERO {
            this.grow(limit.min)?;
        }
        Ok(this)
    }

    pub fn min(&self) -> Index {
        self.limit.min
    }

    pub fn max(&self) -> Index {
        self.limit.max
    }

    pub fn size(&self) -> Index {
        Index(Index::from_usize(self.buffer.borrow().len()).0 / PAGE_SIZE)
    }

    pub fn grow(&self, additional: Index) -> Result<Index, OutOfMemory> {
        let buffer = &mut *self.buffer.borrow_mut();
        let top = Index(Index::from_usize(buffer.len()).0 / PAGE_SIZE);

        let additional = additional
            .0
            .checked_mul(PAGE_SIZE)
            .map(Index)
            .ok_or_else(OutOfMemory::new)?;

        if Index::from_usize(buffer.len())
            .0
            .checked_add(additional.0)
            .is_none_or(|idx| idx > self.limit.max.0)
        {
            return Err(OutOfMemory::new());
        }

        buffer
            .try_reserve_exact(additional.as_usize())
            .map_err(|_| OutOfMemory::new())?;
        let spare_capacity = &mut buffer.spare_capacity_mut()[..additional.as_usize()];
        let additional_len = spare_capacity.len();
        unsafe {
            // Safety: Bytes are zeroed out and initialized before setting the length
            std::ptr::write_bytes(spare_capacity.as_mut_ptr(), 0, additional_len);
            buffer.set_len(buffer.len() + additional_len);
        }

        Ok(top)
    }

    pub fn alloc(&self, bytes: Index) -> Result<Index, OutOfMemory> {
        self.grow(Index(bytes.0.div_ceil(PAGE_SIZE)))
            .map(|sz| Index(sz.0 * PAGE_SIZE))
    }

    pub fn place<T: Pod>(&self, data: &T) -> Result<Index, OutOfMemory> {
        self.place_bytes(bytemuck::bytes_of(data))
    }

    pub fn place_bytes(&self, bytes: &[u8]) -> Result<Index, OutOfMemory> {
        let size = Index(bytes.len().try_into().map_err(|_| OutOfMemory::new())?);
        let ptr = self.alloc(size)?;
        self.store_bytes(ptr, bytes)
            .expect("there needs to be enough space after alloc");
        Ok(ptr)
    }

    pub fn load_ctr(&self, addr: Index) -> Result<CString, MemoryFault> {
        access!(&self.buffer, UnalignedAccess, addr; try {
            |buffer: &[u8]| {
                CStr::from_bytes_until_nul(buffer)
                    .map_err(|_| MemoryFault::new())
                    .map(CStr::to_owned)
            }
        })
    }

    pub fn store_bytes(&self, addr: Index, value: &[u8]) -> Result<(), MemoryFault> {
        self.store_bytes_internal(UnalignedAccess, addr, value)
    }

    pub fn load_bytes(&self, addr: Index, n: Index) -> Result<Box<[u8]>, MemoryFault> {
        access!(&self.buffer, UnalignedAccess, addr, n.as_usize(); Box::<[u8]>::from)
    }

    pub(crate) fn load_internal<T: Pod>(
        &self,
        memory_argument: impl MemoryArgument,
        addr: Index,
    ) -> Result<T, MemoryFault> {
        access!(&self.buffer, memory_argument, addr, size_of::<T>(); bytemuck::pod_read_unaligned)
    }

    pub fn load<T: Pod>(&self, addr: Index) -> Result<T, MemoryFault> {
        self.load_internal(UnalignedAccess, addr)
    }

    #[inline]
    fn store_bytes_internal(
        &self,
        memory_argument: impl MemoryArgument,
        addr: Index,
        value: &[u8],
    ) -> Result<(), MemoryFault> {
        self.fill_with_inner(
            memory_argument,
            addr,
            Index::from_usize(value.len()),
            |bytes: &mut [u8]| bytes.copy_from_slice(value),
        )
    }

    pub(crate) fn store_internal<T: Pod>(
        &self,
        memory_argument: impl MemoryArgument,
        addr: Index,
        value: &T,
    ) -> Result<(), MemoryFault> {
        self.store_bytes_internal(memory_argument, addr, bytemuck::bytes_of(value))
    }

    pub fn store<T: Pod>(&self, addr: Index, value: &T) -> Result<(), MemoryFault> {
        self.store_internal(UnalignedAccess, addr, value)
    }

    pub fn fill(&self, addr: Index, size: Index, byte: u8) -> Result<(), MemoryFault> {
        self.fill_with(addr, size, |bytes| {
            // Safety:
            // slices are perfectly valid for reads and writes for |slice| and are always aligned
            // and u8 ... is a valid layout for u8
            unsafe { std::ptr::write_bytes(bytes.as_mut_ptr(), byte, size_of_val(bytes)) }
        })
    }

    pub(crate) fn fill_with(
        &self,
        addr: Index,
        size: Index,
        func: impl FnOnce(&mut [u8]),
    ) -> Result<(), MemoryFault> {
        self.fill_with_inner(UnalignedAccess, addr, size, func)
    }

    fn fill_with_inner(
        &self,
        memory_arg: impl MemoryArgument,
        addr: Index,
        size: Index,
        func: impl FnOnce(&mut [u8]),
    ) -> Result<(), MemoryFault> {
        access!(&mut self.buffer, memory_arg, addr, size.as_usize(); |bytes: &mut [u8]| {
            func(bytes)
        })
    }

    pub fn copy(&self, src: Index, dest: Index, n: Index) -> Result<(), MemoryFault> {
        let buffer = &mut **self.buffer.borrow_mut();

        let src_start = src;
        let src_end = src.0.checked_add(n.0).map(Index).ok_or_else(MemoryFault::new)?;

        if src_end.as_usize() > buffer.len() || dest.as_usize() > buffer.len() - n.as_usize() {
            return Err(MemoryFault::new());
        }

        buffer.copy_within(src_start.as_usize()..src_end.as_usize(), dest.as_usize());
        Ok(())
    }

    pub fn init(&self, offset: Index, data: &[u8]) -> Result<(), MemoryFault> {
        access!(&mut self.buffer, UnalignedAccess, offset, data.len(); |bytes: &mut [u8]| {
            bytes.copy_from_slice(data)
        })
    }
}
