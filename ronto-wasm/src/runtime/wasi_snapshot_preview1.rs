use crate::Index;
use crate::runtime::linker::{ModuleImportsBuilder, NameSpaceCollision};
use crate::runtime::memory_buffer::MemoryBuffer;
use crate::runtime::{Linker, Trap};
use bytemuck::{Pod, Zeroable};
use std::convert::Infallible;
use std::io::{IoSlice, Write};
use thiserror::Error;

const WASI_ESUCCESS: u32 = 0;
const WASI_EFAULT: u32 = 21;

type Result<T, E = Trap> = std::result::Result<T, E>;

pub fn clock_time_get(
    _: &(),
    _: Option<&MemoryBuffer>,
    (_clock_id, _precision, _ptr): (u32, u64, Index),
) -> u32 {
    WASI_ESUCCESS
}

type Fd = i32;

#[derive(Debug, Copy, Clone, Pod, Zeroable)]
#[repr(C, packed)]
struct WasmIoVec {
    buf: Index,
    len: Index,
}

pub fn fd_write(
    _data: &(),
    mem: Option<&MemoryBuffer>,
    (fd, io_vectors, io_vectors_len, ptr_written): (Fd, Index, Index, Index),
) -> u32 {
    let mem = mem.unwrap();
    let Some(len) = io_vectors_len
        .0
        .checked_mul(const { Index::from_usize(size_of::<WasmIoVec>()).0 })
    else {
        return WASI_EFAULT;
    };

    let Ok(bx) = mem.load_bytes(io_vectors, Index(len)) else {
        return WASI_EFAULT;
    };

    let io_vectors = bytemuck::cast_slice_box::<u8, WasmIoVec>(bx);

    let buffers = io_vectors
        .into_iter()
        .map(|buffer| mem.load_bytes(buffer.buf, buffer.len))
        .collect::<Result<Vec<_>, _>>();

    let Ok(buffers) = buffers else {
        return WASI_EFAULT;
    };

    let buffers = buffers
        .iter()
        .map(|slice| IoSlice::new(slice))
        .collect::<Vec<_>>();

    let written = match fd {
        1 => std::io::stdout().write_vectored(&buffers).unwrap(),
        2 => std::io::stderr().write_vectored(&buffers).unwrap(),
        _ => return WASI_EFAULT,
    };

    mem.store(ptr_written, &(written as u32))
        .map_or(WASI_EFAULT, |()| WASI_ESUCCESS)
}

pub fn sched_yield(
    _: &(),
    _: Option<&MemoryBuffer>, // TODO: list of mem buffers
    _: (),
) -> u32 {
    std::thread::yield_now();
    WASI_ESUCCESS
}

pub fn environ_get(_: &(), _: Option<&MemoryBuffer>, _: (Index, Index)) -> u32 {
    WASI_ESUCCESS
}

pub fn environ_sizes_get(_: &(), _: Option<&MemoryBuffer>, _: (Index, Index)) -> u32 {
    WASI_ESUCCESS
}

pub fn random_get(_: &(), mem: Option<&MemoryBuffer>, (ptr, len): (Index, Index)) -> u32 {
    let mem = mem.unwrap();
    let mut err = None;
    let mem_access = mem.fill_with(ptr, len, |buff| {
        if let Err(e) = getrandom::fill(buff) {
            err = Some(e)
        }
    });

    if mem_access.is_err() {
        return WASI_EFAULT;
    }

    err.map_or(WASI_ESUCCESS, |_| WASI_EFAULT)
}

#[derive(Debug, Error)]
#[error("uncaught exit with code {0}")]
pub struct Exit(pub i32);

pub fn add_to_linker(linker: &mut Linker) -> Result<(), NameSpaceCollision> {
    let mut module_builder = ModuleImportsBuilder::new(());

    macro_rules! imports {
        ([$($name:ident),+ $(,)?]) => {
            module_builder = paste::paste!(module_builder$(
                .function(stringify!($name), |data, mem, input| Ok($name(data, mem, input))).unwrap())*);
        };
    }

    imports!([
        clock_time_get,
        fd_write,
        sched_yield,
        random_get,
        environ_get,
        environ_sizes_get,
    ]);

    module_builder = module_builder
        .function("proc_exit", |(), _, exit_code: i32| {
            Err::<Infallible, _>(Trap::custom(Exit(exit_code)))
        })
        .expect(
            "no way for this to fail, no other module exists with the name proc_exit currently",
        );

    linker
        .add_module("wasi_snapshot_preview1", module_builder.build())
        .map(drop)
}
