use std::env;
use std::fs::File;
use std::io;
use std::io::{BufReader, Read, Write};
use std::path::Path;
use std::str::FromStr;

use simplevm::pp::macros;
use simplevm::pp::PreProcessor;

use simplevm::{Instruction, InstructionParseError};

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
    let processed = processor
        .resolve(&content)
        .map_err(|_| "failed to resolve".to_string())?;
    for line in processed {
        let resolved = processor
            .resolve_pass2(&line)
            .map_err(|_| format!("failed to resolve line: {}", line.get_line_number()))?;
        if args.preprocess_only {
            for &b in format!("{}: {}", line.get_line_number(), resolved).as_bytes() {
                output.push(b);
            }
            output.push(b'\n');
        } else {
            if resolved.is_empty() {
                continue;
            }
            if let Some(';') = resolved.chars().next() {
                continue;
            }
            match Instruction::from_str(&resolved) {
                Ok(instruction) => {
                    let raw_instruction: u16 = instruction.encode_u16();
                    // assumption: >>8 needs to mask for u16
                    output.push((raw_instruction & 0xff) as u8);
                    output.push((raw_instruction >> 8) as u8);
                }
                Err(InstructionParseError::Fail(s)) => {
                    panic!("line {} ({}): {}", line.get_line_number(), resolved, s);
                }
                _ => panic!("line {} ({}): error", line.get_line_number(), resolved),
            }
        }
    }
    let mut stdout = io::stdout().lock();
    stdout.write_all(&output).map_err(|x| format!("{}", x))?;
    Ok(())
}
