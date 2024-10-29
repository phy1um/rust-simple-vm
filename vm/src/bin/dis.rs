use std::env;
use std::fs::File;
use std::io::{stdin, BufReader, Read};
use std::path::Path;

use simplevm::{binfmt::BinaryFile, Instruction};

fn main() -> Result<(), String> {
    // ./asm file.asm

    let args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        panic!("usage: {} <input>", args[0]);
    }

    let tgt: Box<dyn Read> = match args[1].as_ref() {
        "-" => Box::new(stdin()),
        _ => {
            Box::new(File::open(Path::new(&args[1])).map_err(|x| format!("failed to open: {}", x))?)
        }
    };

    let mut reader = BufReader::new(tgt);
    let mut program: Vec<u8> = Vec::new();
    reader
        .read_to_end(&mut program)
        .map_err(|x| format!("read: {}", x))?;
    let bin = BinaryFile::from_bytes(&program)?;
    let code_section = bin.sections.get(0).unwrap();
    let file_start = code_section.file_offset as usize - bin.get_header_size();
    let file_end = file_start + (code_section.size as usize);
    let code_slice = &bin.data[file_start..file_end];
    unsafe {
        let (_, instructions, _) = code_slice.align_to::<u16>();
        for (i, ins) in instructions.iter().enumerate() {
            let value =
                Instruction::try_from(*ins).map_err(|x| format!("@ {}({:04X}): {}", i, ins, x))?;
            println!("{:04}: {value}", (i as u32) + code_section.address)
        }
    }

    Ok(())
}
