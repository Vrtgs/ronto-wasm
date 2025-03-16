use crate::Index;
use crate::runtime::Import;
use crate::runtime::memory_buffer::MemoryBuffer;
use bytemuck::{Pod, Zeroable};
use std::convert::Infallible;
use std::io::{IoSlice, Write};
// sections.import.map(|fun| fun.imports).unwrap_or_default() = [
//     Import {
//         module: "wasi_snapshot_preview1",
//         name: "clock_time_get",
//         description: Function(
//             TypeIndex(
//                 Index(
//                     13,
//                 ),
//             ),
//         ),
//     },
//     Import {
//         module: "wasi_snapshot_preview1",
//         name: "fd_write",
//         description: Function(
//             TypeIndex(
//                 Index(
//                     14,
//                 ),
//             ),
//         ),
//     },
//     Import {
//         module: "wasi_snapshot_preview1",
//         name: "sched_yield",
//         description: Function(
//             TypeIndex(
//                 Index(
//                     15,
//                 ),
//             ),
//         ),
//     },
//     Import {
//         module: "wasi_snapshot_preview1",
//         name: "random_get",
//         description: Function(
//             TypeIndex(
//                 Index(
//                     8,
//                 ),
//             ),
//         ),
//     },
//     Import {
//         module: "wasi_snapshot_preview1",
//         name: "environ_get",
//         description: Function(
//             TypeIndex(
//                 Index(
//                     8,
//                 ),
//             ),
//         ),
//     },
//     Import {
//         module: "wasi_snapshot_preview1",
//         name: "environ_sizes_get",
//         description: Function(
//             TypeIndex(
//                 Index(
//                     8,
//                 ),
//             ),
//         ),
//     },
//     Import {
//         module: "wasi_snapshot_preview1",
//         name: "proc_exit",
//         description: Function(
//             TypeIndex(
//                 Index(
//                     0,
//                 ),
//             ),
//         ),
//     },
// ]

const WASI_ESUCCESS: u32 = 0;
const WASI_EFAULT: u32 = 21;

pub fn clock_time_get((_clock_id, _precision, _ptr): (u32, u64, Index)) -> i32 {
    0
}

type Fd = i32;

#[derive(Debug, Copy, Clone, Pod, Zeroable)]
#[repr(C, packed)]
struct WasmIoVec {
    buf: Index,
    len: Index,
}

pub fn fd_write(
    (fd, io_vectors, io_vectors_len, ptr_written): (Fd, Index, Index, Index),
    mem: &MemoryBuffer,
) -> u32 {
    let Some(len) = io_vectors_len
        .0
        .checked_mul(Index::from_usize(size_of::<WasmIoVec>()).0)
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
        .map_or(WASI_ESUCCESS, |_| WASI_EFAULT)
}

pub fn sched_yield(_: ()) -> u32 {
    std::thread::yield_now();
    WASI_ESUCCESS
}

pub fn proc_exit(code: i32) -> Infallible {
    std::process::exit(code)
}

pub fn environ_get(_: (Index, Index)) -> u32 {
    unreachable!()
}

pub fn environ_sizes_get(_: (Index, Index)) -> u32 {
    0
}

pub fn random_get((ptr, len): (Index, Index), mem: &MemoryBuffer) -> u32 {
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

pub fn import_object() -> impl IntoIterator<Item = (&'static str, Import)> {
    macro_rules! imports {
        ([$($name:ident $(w/mem $(@$mem:tt)?)? ),+ $(,)?]) => {
            [$((stringify!($name), paste::paste!(Import::[<function $(_with_mem $(@$mem)?)?>]($name)))),*]
        };
    }

    imports!([
        clock_time_get,
        fd_write w/mem,
        sched_yield,
        random_get w/mem,
        environ_get,
        environ_sizes_get,
        proc_exit
    ])
}
