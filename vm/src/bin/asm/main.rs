use std::env;
use std::fs::File;
use std::io;
use std::io::{BufReader, Read, Write};
use std::path::Path;

use simplevm::pp::macros;
use simplevm::pp::PreProcessor;

use simplevm::binfmt::BinaryFile;

mod args;

fn main() -> Result<(), String> {
    // ./asm file.asm
    let args_vec: Vec<_> = env::args().collect();
    let args = args::process_cli(&args_vec).map_err(|x| x.to_string())?;
    if !args.validate() {
        println!("{}", args.usage());
        return Ok(());
    }

    let file = File::open(Path::new(&args.input_file.unwrap()))
        .map_err(|x| format!("failed to open: {}", x))?;
    let mut output: Vec<u8> = Vec::new();

    let mut processor = PreProcessor::default();
    macros::setup_std_macros(&mut processor);
    let mut reader = BufReader::new(file);
    let mut content = String::new();
    reader
        .read_to_string(&mut content)
        .map_err(|_| "failed to read file".to_string())?;
    processor
        .handle(&content)
        .map_err(|e| format!("failed to resolve: {e}"))?;
    if args.preprocess_only {
        todo!("wip rebuilding");
    } else {
        let bin: BinaryFile = processor
            .try_into()
            .map_err(|e| format!("build binary: {e}"))?;
        bin.to_bytes(&mut output);
    }
    let mut stdout = io::stdout().lock();
    stdout.write_all(&output).map_err(|x| format!("{}", x))?;
    Ok(())
}
