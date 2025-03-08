use crate::invalid_data;
use crate::parser::Decode;
use crate::read_tape::ReadTape;
use std::cmp::Ordering;
use std::error::Error;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::io::Read;
use std::mem::ManuallyDrop;
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;

#[derive(
    Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, bytemuck::Pod, bytemuck::Zeroable,
)]
#[repr(transparent)]
pub struct Index(pub u32);

pub(crate) fn vector_from_vec<T>(vec: Vec<T>) -> std::io::Result<WasmVec<T>> {
    WasmVec::try_from(vec.into_boxed_slice()).map_err(invalid_data)
}


impl Decode for Index {
    fn decode(file: &mut ReadTape<impl Read>) -> std::io::Result<Self> {
        Ok(Index(u32::decode(file)?))
    }
}

#[cold]
#[inline(never)]
#[track_caller]
const fn out_of_range_panic() -> ! {
    panic!("index has to fit in a u32")
}

macro_rules! unwrap_index_error {
    ($res: expr) => {
        match $res {
            Some(x) => x,
            None => out_of_range_panic(),
        }
    };
}

const _: () = assert!(
    usize::BITS >= u32::BITS,
    "architecture unsupported, pointer width should be at least 32"
);

impl Index {
    pub const ZERO: Self = Self(0);
    pub const MAX: Self = Self(u32::MAX);

    pub const fn try_from_usize(index: usize) -> Option<Index> {
        if index >= u32::MAX as usize {
            return None;
        }

        Some(Index(index as u32))
    }

    pub const fn from_usize(index: usize) -> Index {
        unwrap_index_error!(Self::try_from_usize(index))
    }

    pub const fn as_usize(self) -> usize {
        self.0 as usize
    }
}

pub struct WasmVec<T> {
    ptr: NonNull<T>,
    len: Index,
}

// Safety: WasmVec<T> owns T, and behaves like T
unsafe impl<T: Send> Send for WasmVec<T> {}
unsafe impl<T: Sync> Sync for WasmVec<T> {}

impl<T> Default for WasmVec<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> WasmVec<T> {
    pub const fn new() -> Self {
        Self {
            ptr: NonNull::dangling(),
            len: Index::ZERO,
        }
    }

    pub fn from_trusted_box(bx: Box<[T]>) -> Self {
        unwrap_index_error!(Self::try_from(bx).ok())
    }

    pub fn len_idx(&self) -> Index {
        self.len
    }

    pub fn get(&self, index: Index) -> Option<&T> {
        if self.len <= index {
            return None;
        }
        Some(unsafe { &*self.ptr.as_ptr().add(index.as_usize()) })
    }

    pub fn get_mut(&mut self, index: Index) -> Option<&mut T> {
        if self.len <= index {
            return None;
        }
        Some(unsafe { &mut *self.ptr.as_ptr().add(index.as_usize()) })
    }

    pub fn map_ref<U>(&self, map: impl FnMut(&T) -> U) -> WasmVec<U> {
        WasmVec::from_trusted_box(self.iter().map(map).collect())
    }

    pub fn map<U>(self, map: impl FnMut(T) -> U) -> WasmVec<U> {
        WasmVec::from_trusted_box(self.into_iter().map(map).collect())
    }

    pub fn try_map<U, E>(self, map: impl FnMut(T) -> Result<U, E>) -> Result<WasmVec<U>, E> {
        self.into_iter()
            .map(map)
            .collect::<Result<Box<[U]>, E>>()
            .map(WasmVec::from_trusted_box)
    }

    /// # Safety
    /// must ensure the vec isn't dropped afterward
    unsafe fn take_box(&mut self) -> Box<[T]> {
        let ptr = std::ptr::slice_from_raw_parts_mut(self.ptr.as_ptr(), self.len.as_usize());
        unsafe { Box::from_raw(ptr) }
    }
}

impl<T> Deref for WasmVec<T> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        // Safety: self owns this pointer that was previously allocated by Box
        unsafe { std::slice::from_raw_parts(self.ptr.as_ptr(), self.len.as_usize()) }
    }
}

impl<T> DerefMut for WasmVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // Safety: self owns this pointer that was previously allocated by Box
        unsafe { std::slice::from_raw_parts_mut(self.ptr.as_ptr(), self.len.as_usize()) }
    }
}

#[cold]
#[inline(never)]
#[track_caller]
fn panic_out_of_bounds(index: Index, length: Index) -> ! {
    panic!(
        "index: {} out of bounds for WasmVec of length {}",
        index.0, length.0
    )
}

impl<T: Clone> Clone for WasmVec<T> {
    fn clone(&self) -> Self {
        Self::from_trusted_box(Box::<[T]>::from(&**self))
    }
}

#[derive(Debug)]
pub struct VectorTooLong(());

impl Display for VectorTooLong {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("\"vector type too long\"")
    }
}

impl Error for VectorTooLong {}

impl<T> TryFrom<Box<[T]>> for WasmVec<T> {
    type Error = VectorTooLong;
    fn try_from(value: Box<[T]>) -> Result<Self, Self::Error> {
        let len = Index::try_from_usize(value.len()).ok_or(VectorTooLong(()))?;
        let ptr = Box::into_raw(value) as *mut T;
        Ok(WasmVec {
            ptr: unsafe { NonNull::new_unchecked(ptr) },
            len,
        })
    }
}

impl<T> From<WasmVec<T>> for Box<[T]> {
    fn from(value: WasmVec<T>) -> Self {
        let mut value = ManuallyDrop::new(value);
        // Safety: value is wrapped in a ManuallyDrop, and so it won't be dropped afterward
        unsafe { value.take_box() }
    }
}

impl<T> IntoIterator for WasmVec<T> {
    type Item = T;
    type IntoIter = <Box<[T]> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        Box::<[T]>::from(self).into_iter()
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
        (0..len)
            .map(|_| T::decode(file))
            .collect::<Result<Box<[_]>, _>>()
            .map(Self::from_trusted_box)
    }
}

macro_rules! deref_trait {
    ($trait: ident $(; fn $name: ident(&self $(, $arg_name: ident : $arg_ty:ty)*) $(-> $ret: ty)?)*) => {
        impl<T: $trait> $trait for WasmVec<T> {
            $(
            #[allow(clippy::partialeq_ne_impl)]
            fn $name(&self $(, $arg_name : $arg_ty)*) $(-> $ret)? {
                <[T] as $trait>::$name(&**self $(, $arg_name)*)
            }
            )*
        }
    };
}

deref_trait!(Debug; fn fmt(&self, f: &mut Formatter) -> fmt::Result);
deref_trait!(PartialEq; fn eq(&self, other: &Self) -> bool; fn ne(&self, other: &Self) -> bool);
deref_trait!(Eq);
deref_trait!(PartialOrd; fn partial_cmp(&self, other: &Self) -> Option<Ordering>);
deref_trait!(Ord; fn cmp(&self, other: &Self) -> Ordering);
