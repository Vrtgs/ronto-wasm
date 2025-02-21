use clap::Parser;
use std::fs::File;
use std::io::BufReader;
use std::path::PathBuf;

mod backend;
mod bufferedread;
mod frontend;

#[derive(Parser)]
struct Cli {
    file: PathBuf,
}

fn main() {
    let args = Cli::parse();
    let reader = BufReader::new(File::open(args.file).expect("file exists"));
    let wasm = frontend::parse_file(reader).expect("invalid WASM");
    backend::execute(wasm);
}
