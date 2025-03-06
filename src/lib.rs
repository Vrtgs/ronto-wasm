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
    use crate::parse_file;
    use crate::runtime::memory_buffer::MemoryBuffer;
    use crate::runtime::parameter::{FunctionInput, FunctionOutput};
    use crate::runtime::WasmVirtualMachine;
    use crate::vector::Index;
    use base64::prelude::BASE64_STANDARD;
    use base64::Engine;
    use bytemuck::{Pod, Zeroable};
    use std::ffi::CString;
    use std::fmt::Debug;
    use std::fs::File;
    use std::io;
    use std::path::Path;
    use wasm_testsuite::data::SpecVersion;

    fn get_vm(path: impl AsRef<Path>) -> io::Result<WasmVirtualMachine> {
        WasmVirtualMachine::new(parse_file(File::open(
            Path::new("./test-files/test-modules")
                .join(path)
                .with_extension("wasm"),
        )?)?)
    }

    fn read_cstr(ptr: Index, mem: &MemoryBuffer) -> CString {
        let mut str = Vec::new();
        for i in (0..).map(|i| Index(ptr.0 + i)) {
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
    #[cfg_attr(miri, ignore)]
    fn fib() -> io::Result<()> {
        test("fib", "fib", 24_i32, 75025_i64)
    }

    #[test]
    fn factorial() -> io::Result<()> {
        test("factorial", "fac", 5.0_f64, 120.0_f64)
    }

    #[test]
    fn rust_factorial() -> io::Result<()> {
        test("rust-factorial", "factorial", 5_i32, 120_i64)
    }

    #[test]
    fn to_double_digits() -> io::Result<()> {
        test(
            "to_double_digit",
            "to_double_digit",
            ('7' as u32, '8' as u32),
            78_u32,
        )
    }

    #[test]
    fn double_args() -> io::Result<()> {
        test("test_double_arguments", "first_arg", (7_u32, 8_u32), 7_u32)?;
        test("test_double_arguments", "second_arg", (7_u32, 8_u32), 8_u32)
    }

    #[test]
    fn select_t() -> io::Result<()> {
        test("select_t", "implicit_select", (7_i64, 8_i64, 1_i32), 7_i64)?;
        test("select_t", "implicit_select", (7_i64, 8_i64, 0_i32), 8_i64)?;
        test("select_t", "explicit_select", (9.11_f64, 69.0_f64, 1_i32), 9.11_f64)?;
        test("select_t", "explicit_select", (9.11_f64, 69.0_f64, 0_i32), 69.0_f64)
    }

    #[test]
    #[should_panic(expected = "call type mismatch")]
    fn mismatched_type() {
        test("select_t", "implicit_select", (7_i64, 8_i64, 1_i32), 7.0_f64).unwrap()
    }

    #[test]
    fn cstr() -> io::Result<()> {
        let vm = get_vm("test_cstr")?;
        let ptr = vm.call_by_name::<(), Index>("get_cstr", ()).unwrap();
        let mem = vm.get_memory_by_name("memory").unwrap();
        let cstr = read_cstr(ptr, mem);
        assert_eq!(*c"Hello World", *cstr);
        Ok(())
    }

    #[test]
    fn make_cstr() -> io::Result<()> {
        let vm = get_vm("make_cstr")?;
        let mem = vm.get_memory_by_name("memory").unwrap();

        for original in [c"FOO", c"BAR", cr###"{ "yay_son": "json" }"###] {
            let bytes = original.to_bytes();
            let ptr = mem.place_bytes(bytes).unwrap();
            let len = Index::from_usize(bytes.len());

            let ptr = vm
                .call_by_name::<(Index, Index), Index>("make_cstr", (ptr, len))
                .unwrap();
            let cstr = read_cstr(ptr, mem);
            assert_eq!(*original, *cstr);
        }

        Ok(())
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn base64_encode() -> io::Result<()> {
        let vm = get_vm("encode_base64")?;
        let mem = vm.get_memory_by_name("memory").unwrap();

        let data = std::array::from_fn::<_, 12, _>(|i| {
            let extra = usize::from((getrandom::u32().unwrap() >> 22) as u16);
            let size = i + extra;
            let mut bytes = Box::new_uninit_slice(size);
            getrandom::fill_uninit(&mut bytes).unwrap();
            // if fill_uninit returns, then it has fully initialized `bytes`
            unsafe { bytes.assume_init() }
        });

        for original in data {
            let ptr = mem.place_bytes(&original).unwrap();
            let len = Index::from_usize(original.len());

            #[derive(Debug, Copy, Clone, Pod, Zeroable)]
            #[repr(C)]
            struct Bytes {
                ptr: Index,
                len: Index,
            }

            let result_location = mem.place(&Bytes::zeroed()).unwrap();

            vm.call_by_name::<(Index, Index, Index), ()>(
                "encode_base64",
                (result_location, ptr, len),
            )
                .unwrap();
            let Bytes { ptr, len } = mem.load(result_location).unwrap();
            let bytes = BASE64_STANDARD
                .decode(mem.load_bytes(ptr, len).unwrap())
                .unwrap();
            assert_eq!(*original, *bytes);
        }

        Ok(())
    }

    // TODO: actually run the tests
    #[test]
    #[cfg_attr(miri, ignore)]
    fn testsuite() -> io::Result<()> {
        for _ in wasm_testsuite::data::spec(&SpecVersion::V1) {}
        Ok(())
    }
}
