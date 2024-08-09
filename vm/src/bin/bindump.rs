use std::env;
use std::fs::File;
use std::io::{stdin, BufReader, Read};
use std::path::Path;

use simplevm::binfmt::BinaryFile;

fn main() -> Result<(), String> {
    let args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        panic!("usage: {} <input>", args[0]);
    }

    let reader: Box<dyn Read> = match args[1].as_ref() {
        "-" => Box::new(stdin()),
        _ => {
            Box::new(File::open(Path::new(&args[1])).map_err(|x| format!("failed to open: {}", x))?)
        }
    };

    let mut reader = BufReader::new(reader);
    let mut bin: Vec<u8> = Vec::new();
    reader
        .read_to_end(&mut bin)
        .map_err(|x| format!("read: {}", x))?;

    let file = BinaryFile::from_bytes(&bin)?;
    println!("{file}");
    Ok(())
}
