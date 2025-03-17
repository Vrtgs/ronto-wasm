use bytemuck::{Pod, Zeroable};
use image::GenericImageView;
use ronto_wasm::{parse_file, Index, WasmVirtualMachine};
use std::fs::File;
use std::path::Path;
use std::time::Instant;

fn get_vm(path: impl AsRef<Path>) -> anyhow::Result<WasmVirtualMachine> {
    WasmVirtualMachine::new(parse_file(File::open(
        Path::new("./test-files/test-modules")
            .join(path)
            .with_extension("wasm"),
    )?)?)
}

#[derive(Debug, Copy, Clone, Pod, Zeroable)]
#[repr(C)]
struct Bytes {
    ptr: Index,
    len: Index,
}

fn main() -> anyhow::Result<()> {
    let vm = get_vm("decode_image")?;
    let mem = vm.get_memory_by_name("memory").unwrap();

    for image in std::fs::read_dir("./test-files/assets/images")? {
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
        eprintln!("decoding took: {elapsed:?}; for image: {}", image_path.display());

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