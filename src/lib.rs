#![recursion_limit = "1024"]

use std::error::Error;
use std::io;
use std::mem::MaybeUninit;

mod instruction;
mod parser;
mod read_tape;
mod runtime;
mod vector;

pub use parser::parse_file;
pub use runtime::execute;

pub(crate) trait Stack<T> {
    fn pop_n<const N: usize>(&mut self) -> Option<[T; N]>;

    fn push_n<const N: usize>(&mut self, data: [T; N]);
}

impl<T> Stack<T> for Vec<T> {
    fn pop_n<const N: usize>(&mut self) -> Option<[T; N]> {
        if self.len() < N {
            self.clear();
            return None;
        }

        let mut ret = MaybeUninit::<[T; N]>::uninit();

        unsafe {
            let new_len = self.len().unchecked_sub(N);
            std::ptr::copy_nonoverlapping(
                self.as_ptr().add(new_len),
                ret.as_mut_ptr().cast::<T>(),
                N,
            );
            self.set_len(new_len);
            Some(ret.assume_init())
        }
    }

    fn push_n<const N: usize>(&mut self, data: [T; N]) {
        self.extend(data)
    }
}

pub(crate) fn invalid_data(err: impl Into<Box<dyn Error + Send + Sync>>) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, err)
}
