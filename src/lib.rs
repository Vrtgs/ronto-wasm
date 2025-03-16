#![recursion_limit = "1024"]

use std::error::Error;
use std::io;
use std::mem::MaybeUninit;

mod expression;
pub mod parser;
mod read_tape;
pub mod runtime;
mod vector;

pub use parser::parse_file;
pub use runtime::WasmVirtualMachine;
pub use vector::Index;

pub(crate) trait Stack<T> {
    fn pop_n<const N: usize>(&mut self) -> Option<[T; N]>;
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
}

pub(crate) fn invalid_data(err: impl Into<Box<dyn Error + Send + Sync>>) -> anyhow::Error {
    io::Error::new(io::ErrorKind::InvalidData, err).into()
}

#[cfg(test)]
mod tests {
    use crate::parse_file;
    use crate::runtime::parameter::{FunctionInput, FunctionOutput};
    use crate::runtime::{CallError, GetFunctionError, WasmVirtualMachine};
    use crate::vector::Index;
    use base64::Engine;
    use base64::prelude::BASE64_STANDARD;
    use bytemuck::{Pod, Zeroable};
    use image::GenericImageView;
    use std::fmt::Debug;
    use std::fs::File;
    use std::io;
    use std::path::Path;
    use wasm_testsuite::data::SpecVersion;

    fn get_vm(path: impl AsRef<Path>) -> anyhow::Result<WasmVirtualMachine> {
        WasmVirtualMachine::new(parse_file(File::open(
            Path::new("./test-files/test-modules")
                .join(path)
                .with_extension("wasm"),
        )?)?)
    }

    fn test<In: FunctionInput, Out: FunctionOutput + PartialEq + Debug>(
        path: impl AsRef<Path>,
        function: &str,
        input: In,
        expected_output: Out,
    ) -> Result<(), CallError> {
        let vm = get_vm(path).unwrap();
        vm.call_by_name::<In, Out>(function, input)
            .map(|output| assert_eq!(output, expected_output))
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn fib() {
        test("fib", "fib", 24_i32, 75025_i64).unwrap();
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn fib_rust() {
        test("fib-rust", "fib", 24_i32, 75025_i64).unwrap();
    }

    #[test]
    fn factorial() {
        test("factorial", "fac", 5.0_f64, 120.0_f64).unwrap();
    }

    #[test]
    fn rust_factorial() {
        test("rust-factorial", "factorial", 5_i32, 120_i64).unwrap();
    }

    #[test]
    fn to_double_digits() {
        test(
            "to_double_digit",
            "to_double_digit",
            ('7' as u32, '8' as u32),
            78_u32,
        )
        .unwrap();
    }

    #[test]
    fn double_args() {
        test("test_double_arguments", "first_arg", (7_u32, 8_u32), 7_u32).unwrap();
        test("test_double_arguments", "second_arg", (7_u32, 8_u32), 8_u32).unwrap();
    }

    #[test]
    fn rotate_tests() {
        test("rot", "rotl", (0x12345678_u32, 8_u32), 0x34567812_u32).unwrap();
        test("rot", "rotr", (0x12345678_u32, 8_u32), 0x78123456_u32).unwrap();
        test(
            "rot",
            "rotl64",
            (0x123456789ABCDEF0_u64, 16_u64),
            0x56789ABCDEF01234_u64,
        )
        .unwrap();
        test(
            "rot",
            "rotr64",
            (0x123456789ABCDEF0_u64, 16_u64),
            0xDEF0123456789ABC_u64,
        )
        .unwrap();
    }

    #[test]
    fn select_t() {
        test("select_t", "implicit_select", (7_i64, 8_i64, 1_i32), 7_i64).unwrap();
        test("select_t", "implicit_select", (7_i64, 8_i64, 0_i32), 8_i64).unwrap();
        test(
            "select_t",
            "explicit_select",
            (9.11_f64, 69.0_f64, 1_i32),
            9.11_f64,
        )
        .unwrap();
        test(
            "select_t",
            "explicit_select",
            (9.11_f64, 69.0_f64, 0_i32),
            69.0_f64,
        )
        .unwrap()
    }

    #[test]
    fn mismatched_type() {
        assert!(matches!(
            test(
                "select_t",
                "implicit_select",
                (7_i64, 8_i64, 1_i32),
                7.0_f64
            ),
            Err(CallError::GetFunctionError(
                GetFunctionError::MismatchedType(_),
            ))
        ))
    }

    #[test]
    fn cstr() -> anyhow::Result<()> {
        let vm = get_vm("test_cstr")?;
        let ptr = vm.call_by_name::<(), Index>("get_cstr", ())?;
        let mem = vm.get_memory_by_name("memory").unwrap();
        let cstr = mem.load_ctr(ptr)?;
        assert_eq!(*c"Hello World", *cstr);
        Ok(())
    }

    #[test]
    fn make_cstr() -> anyhow::Result<()> {
        let vm = get_vm("make_cstr")?;
        let mem = vm.get_memory_by_name("memory").unwrap();

        for original in [c"FOO", c"BAR", cr###"{ "yay_son": "json" }"###] {
            let bytes = original.to_bytes();
            let ptr = mem.place_bytes(bytes)?;
            let len = Index::from_usize(bytes.len());

            let ptr = vm.call_by_name::<(Index, Index), Index>("make_cstr", (ptr, len))?;
            let cstr = mem.load_ctr(ptr)?;
            assert_eq!(*original, *cstr);
        }

        Ok(())
    }

    #[derive(Debug, Copy, Clone, Pod, Zeroable)]
    #[repr(C)]
    struct Bytes {
        ptr: Index,
        len: Index,
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn base64_encode() -> anyhow::Result<()> {
        let vm = get_vm("encode_base64")?;
        let mem = vm.get_memory_by_name("memory").unwrap();

        let data = std::array::from_fn::<_, 12, _>(|i| {
            let extra = usize::from((getrandom::u32().unwrap() >> 16) as u16);
            let size = i + extra;
            let mut bytes = Box::new_uninit_slice(size);
            getrandom::fill_uninit(&mut bytes).unwrap();
            // if fill_uninit returns, then it has fully initialized `bytes`
            unsafe { bytes.assume_init() }
        });

        for original in data {
            let ptr = mem.place_bytes(&original)?;
            let len = Index::from_usize(original.len());

            let result_location = mem.place(&Bytes::zeroed())?;

            vm.call_by_name::<(Index, Index, Index), ()>(
                "encode_base64",
                (result_location, ptr, len),
            )?;
            let Bytes { ptr, len } = mem.load(result_location)?;
            let bytes = BASE64_STANDARD.decode(mem.load_bytes(ptr, len).unwrap())?;
            assert_eq!(*original, *bytes);
        }

        Ok(())
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn png_decode() -> anyhow::Result<()> {
        let vm = get_vm("decode_image")?;
        let mem = vm.get_memory_by_name("memory").unwrap();

        for image in std::fs::read_dir("./test-files/assets/images")? {
            let image = image?.path();
            let image_bytes = std::fs::read(image)?;
            let image = image::load_from_memory(&image_bytes)?;

            let ptr = mem.place_bytes(&image_bytes)?;
            let len = Index::from_usize(image_bytes.len());
            drop(image_bytes);

            #[derive(Debug, Copy, Clone, Pod, Zeroable)]
            #[repr(C)]
            struct Image {
                width: u32,
                height: u32,
                bytes: Bytes,
            }

            let result_location = mem.place(&Image::zeroed())?;

            vm.call_by_name::<(Index, Index, Index), ()>(
                "decode_image",
                (result_location, ptr, len),
            )?;

            let result_image = mem.load::<Image>(result_location)?;
            assert_eq!(
                (result_image.width, result_image.height),
                image.dimensions()
            );

            let bytes = mem.load_bytes(result_image.bytes.ptr, result_image.bytes.len)?;
            assert_eq!(&*bytes, image.as_bytes())
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
