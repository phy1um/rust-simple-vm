use std::env;
use std::fs::File;
use std::io;
use std::io::{BufRead, BufReader, Write};
use std::path::Path;
use std::str::FromStr;

mod pp;
mod macros;
use crate::pp::PreProcessor;

use simplevm::{Instruction, InstructionParseError};

fn main() -> Result<(), String> {
    // ./asm file.asm
    
    let preprocess_only = true;

    let args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        panic!("usage: {} <input>", args[0]);
    }

    let file = File::open(Path::new(&args[1])).map_err(|x| format!("failed to open: {}", x))?;
    let mut output: Vec<u8> = Vec::new();
    /*
     * Push 10
     * Push 240
     * AddStack
     * PopRegister A
     * Signal $f0
     *
     */
    let mut processor = PreProcessor::new();
    processor.define_macro("defvar", macros::defvar);
    processor.define_macro("include", macros::include);
    for (i, line) in BufReader::new(file).lines().enumerate() {
        // TODO: wtf
        let line_inner = line.map_err(|_x| "foo")?;
        if line_inner.is_empty() {
            continue;
        }
        if line_inner.chars().next().unwrap() == ';' {
            continue;
        }
        // preprocess here
        let processed = match processor.resolve(&line_inner).map_err(|x| x.to_string()) {
            Err(e) => panic!("line {}: {}", i, e),
            Ok(s) => s,
        };
        if preprocess_only && !processed.is_empty() {
            for &b in processed.as_bytes() {
                output.push(b);  
            }
            output.push(b'\n');
        } else {
            match Instruction::from_str(&processed) {
                Ok(instruction) => {
                    let raw_instruction: u16 = instruction.encode_u16();
                    // assumption: >>8 needs to mask for u16
                    output.push((raw_instruction & 0xff) as u8);
                    output.push((raw_instruction >> 8) as u8);
                }
                Err(InstructionParseError::Fail(s)) => {
                    panic!("line {}: {}", i, s);
                }
                _ => continue,
            }
        }
    }
    let mut stdout = io::stdout().lock();
    stdout.write_all(&output).map_err(|x| format!("{}", x))?;
    Ok(())
}
