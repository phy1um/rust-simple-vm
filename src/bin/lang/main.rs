pub mod parse;
pub mod combinator;
pub mod character;
pub mod ast;
mod language;

mod compile;

use crate::parse::run_parser;
use crate::language::*;
use crate::compile::compile;

use std::io::{Write, Read, stdin, stdout};
use std::env;
use std::fs::File;
use std::path::Path;

const LOADED_PROGRAM_OFFSET: u32 = 0x0;

fn main() -> Result<(), String> {
    let args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        panic!("usage: {} <input>", args[0]);
    }

    let mut reader: Box<dyn Read> = match args[1].as_ref() {
        "-" => Box::new(stdin()),
        _ => {
            Box::new(File::open(Path::new(&args[1])).map_err(|x| format!("failed to open: {}", x))?)
        }
    };

    let mut code = Vec::new();
    reader.read_to_end(&mut code);
    let code_str = std::str::from_utf8(&code).map_err(|_| "not utf8")?;

    let program = run_parser(parse_ast, code_str)?;
    let res = compile(program, LOADED_PROGRAM_OFFSET).map_err(|x| format!("compiling: {x:?}"))?;
    let instructions = res.get_instructions().map_err(|x| format!("resolving: {x:?}"))?;
    let mut output: Vec<u8> = Vec::new();
    for i in instructions {
        let raw_instruction: u16 = i.encode_u16();
        // assumption: >>8 needs to mask for u16
        output.push((raw_instruction & 0xff) as u8);
        output.push((raw_instruction >> 8) as u8);
    }
    let mut stdout = stdout().lock();
    stdout.write_all(&output).map_err(|x| format!("{}", x))?;
    Ok(())
}

