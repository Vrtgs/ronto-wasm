use std::cell::RefCell;
use std::fmt::Formatter;
use bytemuck::Pod;
use crate::parser;
use crate::parser::Limit;
use crate::vector::Index;

const PAGE_SIZE: u32 = 65536;


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
            pub fn new() -> Self {
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

pub trait MemoryArgument: Copy {
    fn offset(self) -> Index;
    fn align(self) -> usize;
}

impl MemoryArgument for parser::MemoryArgument {
    fn offset(self) -> Index {
        self.offset
    }

    fn align(self) -> usize {
        self.align.as_usize()
    }
}

pub struct MemoryBuffer {
    limit: Limit,
    buffer: RefCell<Vec<u8>>
}

macro_rules! assign_or {
    ($name: ident = $expr:expr; $fail:expr) => {
        let Some($name) = $expr else {
            return Err($fail)
        };
    };
}

macro_rules! assign_or_fault {
    ($name: ident = $expr:expr) => {
        assign_or!($name = $expr; MemoryFault::new())
    };
}

macro_rules! access {
    (&$(mut $(@$_mut:tt)?)? $buffer:expr, $memory_argument:expr, $addr:expr, $size:expr; $map: expr) => {{
        paste::paste! { let buffer = & $(mut $(@$_mut)?)? ** $buffer.[<borrow $(_mut $($_mut)?)?>](); }

        assign_or_fault!(addr = $memory_argument.offset().checked_add($addr));
        assign_or_fault!(end  = addr.as_usize().checked_add($size));

        paste::paste! { assign_or_fault!(bytes = buffer.[<get $(_mut $($_mut)?)?>](addr.as_usize()..end));  }

        if ($memory_argument.align() & bytes.as_ptr().addr()) != 0 {
            return Err(MemoryFault::new());
        }

        Ok(($map)(bytes))
    }};
}

impl MemoryBuffer {
    pub fn new(limit: Limit) -> Result<Self, OutOfMemory> {
        let this = Self {
            limit,
            buffer: RefCell::new(Vec::new())
        };
        if limit.min != Index::ZERO {
            this.grow(limit.min)?;
        }
        Ok(this)
    }
    
    pub fn size(&self) -> Index {
        Index::from_usize(self.buffer.borrow().len())
    }
    
    pub fn grow(&self, additional: Index) -> Result<Index, OutOfMemory> {
        let buffer = &mut *self.buffer.borrow_mut();

        assign_or!(additional = additional.0.checked_mul(PAGE_SIZE).map(Index); OutOfMemory::new());
        
        if Index::from_usize(buffer.len()).checked_add(additional).is_none_or(|idx| idx > self.limit.max) {
            return Err(OutOfMemory::new())
        }

        buffer.try_reserve_exact(additional.as_usize())
            .map_err(|_| OutOfMemory::new())?;
        let spare_capacity = &mut buffer.spare_capacity_mut()[..additional.as_usize()];
        let additional_len = spare_capacity.len();
        unsafe {
            // Safety: Bytes are zeroed out and initialized before setting the length
            std::ptr::write_bytes(spare_capacity.as_mut_ptr(), 0, additional_len);
            buffer.set_len(buffer.len() + additional_len);
        }

        Ok(Index::from_usize(buffer.len()))
    }
    
    pub fn load<T: Pod>(&self, memory_argument: impl MemoryArgument, addr: Index) -> Result<T, MemoryFault> {
        access!(&self.buffer, memory_argument, addr, size_of::<T>(); bytemuck::pod_read_unaligned)
    }

    pub fn store<T: Pod>(&self, memory_argument: impl MemoryArgument, addr: Index, value: &T) -> Result<(), MemoryFault> {
        access!(&mut self.buffer, memory_argument, addr, size_of::<T>(); |bytes: &mut [u8]| {
            bytes.copy_from_slice(bytemuck::bytes_of(value))
        })
    }

    pub fn fill(&self, memory_argument: impl MemoryArgument, addr: Index, size: Index, byte: u8) -> Result<(), MemoryFault> {
        access!(&mut self.buffer, memory_argument, addr, size.as_usize(); |bytes: &mut [u8]| {
            // Safety:
            // slices are perfectly valid for reads and writes for |slice| and are always aligned
            // and u8 ... is a valid layout for u8
            unsafe { std::ptr::write_bytes(bytes.as_mut_ptr(), byte, size_of_val(bytes)) }
        })
    }
}