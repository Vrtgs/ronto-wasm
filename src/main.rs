use clap::Parser;
use std::fs::File;
use std::io;
use std::io::BufReader;
use std::path::PathBuf;

#[derive(Parser)]
struct Cli {
    file: PathBuf,
}

fn main() -> io::Result<()> {
    let args = Cli::parse();
    let reader = BufReader::new(File::open(args.file)?);

    let wasm = ronto_wasm::parse_file(reader)?;
    ronto_wasm::execute(wasm);
    Ok(())
}
