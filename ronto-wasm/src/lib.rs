#![recursion_limit = "256"]

use std::mem::MaybeUninit;

mod expression;
pub mod parser;
mod read_tape;
pub mod runtime;
mod vector;

pub use parser::parse_module;
pub use runtime::VirtualMachine;
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
                self.as_ptr().add(new_len).cast::<[T; N]>(),
                ret.as_mut_ptr(),
                1,
            );
            self.set_len(new_len);
            if N > 0 {
                std::hint::assert_unchecked(self.len() < self.capacity())
            }
            Some(ret.assume_init())
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parse_module;
    use crate::runtime::parameter::{FunctionInput, FunctionOutput};
    use crate::runtime::{
        CallError, GetFunctionError, Linker, Store, VirtualMachine, wasi_snapshot_preview1,
    };
    use crate::vector::Index;
    use anyhow::bail;
    use base64::Engine;
    use base64::prelude::BASE64_STANDARD;
    use bytemuck::{Pod, Zeroable};
    use image::GenericImageView;
    use std::ffi::OsStr;
    use std::fmt::Debug;
    use std::fs::File;
    use std::io;
    use std::path::Path;
    use std::time::Instant;
    use wasm_testsuite::data::SpecVersion;

    fn get_vm(path: impl AsRef<Path>) -> anyhow::Result<VirtualMachine> {
        let prefix = match path.as_ref().extension().map(OsStr::as_encoded_bytes) {
            Some(b"wast" | b"wat") => Path::new("../test-files/hardcoded-tests"),
            Some(b"wasm") => Path::new("../test-files/test-modules"),
            _ => bail!("unknown wasm test module"),
        };

        let mut linker = Linker::new();
        wasi_snapshot_preview1::add_to_linker(&mut linker)
            .expect("new linker made, impossible to have namespace collision already");

        let store = Store::with_linker(parse_module(File::open(prefix.join(path))?)?, &linker)?;
        VirtualMachine::new(store)
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
        for test_module in ["fib.wast", "fib-rust.wasm"] {
            test(test_module, "fib", 24_i32, 75025_i64).unwrap();
        }
    }

    #[test]
    fn factorial() {
        for test_module in ["factorial.wast", "rust-factorial.wasm"] {
            test(test_module, "factorial", 5_i32, 120_i64).unwrap();
        }
    }

    #[test]
    fn to_double_digits() {
        test(
            "to_double_digit.wasm",
            "to_double_digit",
            ('7' as u32, '8' as u32),
            78_u32,
        )
        .unwrap();
    }

    #[test]
    fn double_args() {
        test(
            "test_double_arguments.wast",
            "first_arg",
            (7_u32, 8_u32),
            7_u32,
        )
        .unwrap();
        test(
            "test_double_arguments.wast",
            "second_arg",
            (7_u32, 8_u32),
            8_u32,
        )
        .unwrap();
    }

    #[test]
    fn rotate_tests() {
        test("rot.wast", "rotl", (0x12345678_u32, 8_u32), 0x34567812_u32).unwrap();
        test("rot.wast", "rotr", (0x12345678_u32, 8_u32), 0x78123456_u32).unwrap();
        test(
            "rot.wast",
            "rotl64",
            (0x123456789ABCDEF0_u64, 16_u64),
            0x56789ABCDEF01234_u64,
        )
        .unwrap();
        test(
            "rot.wast",
            "rotr64",
            (0x123456789ABCDEF0_u64, 16_u64),
            0xDEF0123456789ABC_u64,
        )
        .unwrap();
    }

    #[test]
    fn select_t() {
        test(
            "select_t.wast",
            "implicit_select",
            (7_i64, 8_i64, 1_i32),
            7_i64,
        )
        .unwrap();
        test(
            "select_t.wast",
            "implicit_select",
            (7_i64, 8_i64, 0_i32),
            8_i64,
        )
        .unwrap();
        test(
            "select_t.wast",
            "explicit_select",
            (9.11_f64, 69.0_f64, 1_i32),
            9.11_f64,
        )
        .unwrap();
        test(
            "select_t.wast",
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
                "select_t.wast",
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
        let vm = get_vm("test_cstr.wasm")?;
        let ptr = vm.call_by_name::<(), Index>("get_cstr", ())?;
        let mem = vm.get_memory_by_name("memory").unwrap();
        let cstr = mem.load_ctr(ptr)?;
        assert_eq!(*c"Hello World", *cstr);
        Ok(())
    }

    #[test]
    fn make_cstr() -> anyhow::Result<()> {
        let vm = get_vm("make_cstr.wasm")?;
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
        let vm = get_vm("encode_base64.wasm")?;
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
        let vm = get_vm("decode_image.wasm")?;
        let mem = vm.get_memory_by_name("memory").unwrap();

        for image in std::fs::read_dir("../test-files/assets/images")? {
            let image_path = image?.path();
            let image_bytes = std::fs::read(&image_path)?;
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

            let start = Instant::now();
            vm.call_by_name::<(Index, Index, Index), ()>(
                "decode_image",
                (result_location, ptr, len),
            )?;
            let elapsed = start.elapsed();
            eprintln!(
                "decoding took: {elapsed:?}; for image: {}",
                image_path.display()
            );

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
