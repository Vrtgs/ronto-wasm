use clap::Parser;
use ronto_wasm::WasmVirtualMachine;
use std::fs::File;
use std::io;
use std::path::PathBuf;

#[derive(Parser)]
struct Cli {
    file: PathBuf,
}

fn main() -> io::Result<()> {
    let args = Cli::parse();
    let reader = File::open(args.file)?;
    let vm = WasmVirtualMachine::new(ronto_wasm::parse_file(reader)?)?;
    vm.start().unwrap();
    Ok(())
}
