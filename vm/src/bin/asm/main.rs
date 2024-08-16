use std::env;
use std::fs::File;
use std::io;
use std::io::{BufReader, Read, Write};
use std::path::Path;
use std::str::FromStr;

use simplevm::pp::macros;
use simplevm::pp::PreProcessor;

use simplevm::{
    binfmt::{BinaryFile, Section, SectionMode},
    Instruction, InstructionParseError,
};

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
    if args.preprocess_only {
        for line in processed {
            let resolved = processor
                .resolve_pass2(&line)
                .map_err(|_| format!("failed to resolve line: {}", line.get_line_number()))?;
            for &b in format!("{}: {}", line.get_line_number(), resolved).as_bytes() {
                output.push(b);
            }
            output.push(b'\n');
        }
    } else {
        let mut program_bytes = Vec::<u8>::new();
        for line in processed {
            let resolved = processor
                .resolve_pass2(&line)
                .map_err(|_| format!("failed to resolve line: {}", line.get_line_number()))?;
            if resolved.is_empty() {
                continue;
            }
            if let Some(';') = resolved.chars().next() {
                continue;
            }
            match Instruction::from_str(&resolved) {
                Ok(instruction) => {
                    let raw_instruction: u16 = instruction.encode_u16();
                    program_bytes.extend_from_slice(&raw_instruction.to_le_bytes());
                }
                Err(InstructionParseError::Fail(s)) => {
                    panic!("line {} ({}): {}", line.get_line_number(), resolved, s);
                }
                _ => panic!("line {} ({}): error", line.get_line_number(), resolved),
            }
        }
        let mut bin = BinaryFile {
            entrypoint: 0,
            version: 99,
            ..BinaryFile::default()
        };
        bin.sections.push(Section {
            size: program_bytes.len() as u16,
            mode: SectionMode::RW,
            address: 0,
            file_offset: 1,
        });
        bin.sections.push(Section {
            size: 0x8000,
            mode: SectionMode::Heap,
            address: 0x1000,
            ..Section::default()
        });
        let header_size = bin.get_header_size();
        bin.sections.get_mut(0).unwrap().file_offset = header_size as u32;
        bin.data = program_bytes;
        bin.to_bytes(&mut output);
    }
    let mut stdout = io::stdout().lock();
    stdout.write_all(&output).map_err(|x| format!("{}", x))?;
    Ok(())
}
