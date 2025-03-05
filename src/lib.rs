#![recursion_limit = "1024"]

use std::error::Error;
use std::io;
use std::mem::MaybeUninit;

mod instruction;
pub mod parser;
mod read_tape;
pub mod runtime;
mod vector;

pub use parser::parse_file;
pub use runtime::WasmVirtualMachine;

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

#[cfg(test)]
mod tests {
    use crate::instruction::{FunctionInput, FunctionOutput};
    use crate::parse_file;
    use crate::runtime::WasmVirtualMachine;
    use crate::runtime::memory_buffer::MemoryBuffer;
    use crate::vector::Index;
    use base64::Engine;
    use base64::prelude::BASE64_STANDARD;
    use bytemuck::{Pod, Zeroable};
    use std::ffi::CString;
    use std::fmt::Debug;
    use std::fs::File;
    use std::io;
    use std::path::Path;

    fn get_vm(path: impl AsRef<Path>) -> io::Result<WasmVirtualMachine> {
        WasmVirtualMachine::new(parse_file(File::open(
            Path::new("./test-files/test-modules")
                .join(path)
                .with_extension("wasm"),
        )?)?)
    }

    fn read_cstr(ptr: u32, mem: &MemoryBuffer) -> CString {
        let mut str = Vec::new();
        for i in (0..).map(|i| Index(ptr + i)) {
            let byte = mem.load::<u8>(i).unwrap();
            if byte == 0 {
                break;
            }
            str.push(byte)
        }
        CString::new(str).unwrap()
    }

    fn test<In: FunctionInput, Out: FunctionOutput + PartialEq + Debug>(
        path: impl AsRef<Path>,
        function: &str,
        input: In,
        expected_output: Out,
    ) -> io::Result<()> {
        let vm = get_vm(path)?;
        let output = vm.call_by_name::<In, Out>(function, input).unwrap();
        assert_eq!(output, expected_output);
        Ok(())
    }

    #[test]
    fn test_fib() -> io::Result<()> {
        test("fib", "fib", 24_i32, 75025_i64)
    }

    #[test]
    fn test_factorial() -> io::Result<()> {
        test("factorial", "fac", 5.0_f64, 120.0_f64)
    }

    #[test]
    fn test_rust_factorial() -> io::Result<()> {
        test("rust-factorial", "factorial", 5_i32, 120_i64)
    }

    #[test]
    fn test_to_double_digits() -> io::Result<()> {
        test(
            "to_double_digit",
            "to_double_digit",
            ('7' as u32, '8' as u32),
            78_u32,
        )
    }

    #[test]
    fn test_double_args() -> io::Result<()> {
        test("test_double_arguments", "first_arg", (7_u32, 8_u32), 7_u32)?;
        test("test_double_arguments", "second_arg", (7_u32, 8_u32), 8_u32)
    }

    #[test]
    fn test_cstr() -> io::Result<()> {
        let vm = get_vm("test_cstr")?;
        let ptr = vm.call_by_name::<(), u32>("get_cstr", ()).unwrap();
        let mem = vm.get_memory_by_name("memory").unwrap();
        let cstr = read_cstr(ptr, mem);
        assert_eq!(*c"Hello World", *cstr);
        Ok(())
    }

    #[test]
    fn test_make_cstr() -> io::Result<()> {
        let vm = get_vm("make_cstr")?;
        let mem = vm.get_memory_by_name("memory").unwrap();

        for original in [c"FOO", c"BAR", c"{ yay_son: \"json\" }"] {
            let bytes = original.to_bytes();
            let ptr = mem.place_bytes(bytes).unwrap();
            let len = Index::from_usize(bytes.len());

            let ptr = vm
                .call_by_name::<(u32, u32), u32>("make_cstr", (ptr.0, len.0))
                .unwrap();
            let cstr = read_cstr(ptr, mem);
            assert_eq!(*original, *cstr);
        }

        Ok(())
    }

    #[test]
    fn test_base64_encode() -> io::Result<()> {
        let vm = get_vm("encode_base64")?;
        let mem = vm.get_memory_by_name("memory").unwrap();

        let data = std::array::from_fn::<_, 12, _>(|i| {
            let extra = usize::from((getrandom::u32().unwrap() >> 22) as u16);
            let size = i + extra;
            let mut bytes = vec![0; size].into_boxed_slice();
            getrandom::fill(&mut bytes).unwrap();
            bytes
        });

        for original in data {
            let ptr = mem.place_bytes(&original).unwrap();
            let len = Index::from_usize(original.len());

            #[derive(Debug, Copy, Clone, Pod, Zeroable)]
            #[repr(C)]
            struct Bytes {
                ptr: u32,
                len: u32,
            }

            let result_location = mem.place(&Bytes::zeroed()).unwrap();

            vm.call_by_name::<(u32, u32, u32), ()>(
                "encode_base64",
                (result_location.0, ptr.0, len.0),
            )
            .unwrap();
            let Bytes { ptr, len } = mem.load(result_location).unwrap();
            let bytes = BASE64_STANDARD
                .decode(mem.load_bytes(Index(ptr), Index(len)).unwrap())
                .unwrap();
            assert_eq!(*original, *bytes);
        }

        Ok(())
    }
}
