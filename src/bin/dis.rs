use std::env;
use std::fs::File;
use std::io::{BufReader, Read};
use std::path::Path;

use simplevm::Instruction;

fn main() -> Result<(), String> {
    // ./asm file.asm

    let args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        panic!("usage: {} <input>", args[0]);
    }

    let file = File::open(Path::new(&args[1])).map_err(|x| format!("failed to open: {}", x))?;
    let mut reader = BufReader::new(file);
    let mut program: Vec<u8> = Vec::new();
    reader
        .read_to_end(&mut program)
        .map_err(|x| format!("read: {}", x))?;
    unsafe {
        let (_, instructions, _) = program.align_to::<u16>();
        for ins in instructions.iter() {
            let value = Instruction::try_from(*ins)?;
            println!("{value}")
        }
    }

    Ok(())
}
