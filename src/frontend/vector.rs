use std::fmt;
use std::fmt::Debug;
use std::io::Read;
use std::mem::ManuallyDrop;
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;
use crate::frontend::Decode;
use crate::read_tape::ReadTape;


#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Index(u32);

impl Decode for Index {
    fn decode(file: &mut ReadTape<impl Read>) -> std::io::Result<Self> {
        Ok(Index(u32::decode(file)?))
    }
}

impl Index {
    pub fn from_usize(index: usize) -> Index {
        u32::try_from(index).map(Self).expect("index has to fit in a u32")
    }

    pub fn as_usize(self) -> usize {
        const { assert!(usize::BITS >= u32::BITS, "architecture unsupported, pointer width should be at least 32") }
        self.0 as usize
    }
}

pub struct WasmVec<T> {
    ptr: NonNull<T>,
    len: Index
}


// Safety: WasmVec<T> owns T, and behaves like T
unsafe impl<T: Send> Send for WasmVec<T> {}
unsafe impl<T: Sync> Sync for WasmVec<T> {}

impl<T> WasmVec<T> {
    /// # Safety
    /// must ensure the vec isn't dropped afterward
    unsafe fn take_box(&mut self) -> Box<[T]> {
        let ptr = std::ptr::slice_from_raw_parts_mut(
            self.ptr.as_ptr(),
            self.len.as_usize()
        );
        Box::from_raw(ptr)
    }
}

impl<T> Deref for WasmVec<T> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        // Safety: self owns this pointer that was previously allocated by Box
        unsafe {
            std::slice::from_raw_parts(
                self.ptr.as_ptr(),
                self.len.as_usize()
            )
        }
    }
}

impl<T> DerefMut for WasmVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // Safety: self owns this pointer that was previously allocated by Box
        unsafe {
            std::slice::from_raw_parts_mut(
                self.ptr.as_ptr(),
                self.len.as_usize()
            )
        }
    }
}

impl<T: Clone> Clone for WasmVec<T> {
    fn clone(&self) -> Self {
        Self::from(Box::<[T]>::from(&**self))
    }
}

impl<T> From<Box<[T]>> for WasmVec<T> {
    fn from(value: Box<[T]>) -> Self {
        let len = Index::from_usize(value.len());
        let ptr = Box::into_raw(value) as *mut T;
        WasmVec {
            ptr: unsafe { NonNull::new_unchecked(ptr) },
            len
        }
    }
}

impl<T> From<WasmVec<T>> for Box<[T]> {
    fn from(value: WasmVec<T>) -> Self {
        let mut value = ManuallyDrop::new(value);
        // Safety: value is wrapped in a ManuallyDrop, and so it won't be dropped afterward
        unsafe { value.take_box() }
    }
}

impl<T> Drop for WasmVec<T> {
    fn drop(&mut self) {
        // Safety: self is currently being dropped, and so it won't be dropped afterward
        drop(unsafe { self.take_box() })
    }
}

impl<T: Decode> Decode for WasmVec<T> {
    fn decode(file: &mut ReadTape<impl Read>) -> std::io::Result<Self> {
        let len = u32::decode(file)?;
        (0..len).map(|_| T::decode(file)).collect::<Result<Box<[_]>, _>>().map(Self::from)
    }
}


macro_rules! deref_trait {
    ($trait: ident; fn $name: ident(&self $(, $arg_name: ident : $arg_ty:ty)*) $(-> $ret: ty)?) => {
        impl<T: $trait> $trait for WasmVec<T> {
            fn $name(&self $(, $arg_name : $arg_ty)*) $(-> $ret)? {
                $trait::$name(&**self $(, $arg_name)*)
            }
        }
    };
}

deref_trait!(Debug; fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result);
