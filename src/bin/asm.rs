use std::env;
use std::fs::File;
use std::io;
use std::io::{BufRead, BufReader, Write};
use std::path::Path;
use std::str::FromStr;

use simplevm::{Instruction, OpCode, Register};

fn parse_numeric(s: &str) -> Result<u8, String> {
    if s.is_empty() {
        return Err("string has no length".to_string());
    }
    let fst = s.chars().next().unwrap();
    let (num, radix) = match fst {
        '$' => (&s[1..], 16),
        '%' => (&s[1..], 2),
        _ => (s, 10),
    };
    u8::from_str_radix(num, radix).map_err(|x| format!("{}", x))
}

fn parse_register(s: &str) -> Result<Register, String> {
    match s {
        "A" => Ok(Register::A),
        _ => Err(format!("unknown register: {}", s)),
    }
}

fn assert_length(parts: &Vec<&str>, n: usize) -> Result<(), String> {
    if parts.len() == n {
        Ok(())
    } else {
        Err(format!("expected {} got {}", parts.len(), n))
    }
}

fn handle_line(parts: Vec<&str>) -> Result<Instruction, String> {
    let opcode = OpCode::from_str(parts[0])?;
    match opcode {
        OpCode::Nop => Ok(Instruction::Nop),
        OpCode::Push => {
            assert_length(&parts, 2)?;
            Ok(Instruction::Push(parse_numeric(parts[1])?))
        }
        OpCode::AddStack => {
            assert_length(&parts, 1)?;
            Ok(Instruction::AddStack)
        }
        OpCode::AddRegister => {
            assert_length(&parts, 3)?;
            Ok(Instruction::AddRegister(
                parse_register(parts[1])?,
                parse_register(parts[2])?,
            ))
        }
        OpCode::PopRegister => {
            assert_length(&parts, 2)?;
            Ok(Instruction::PopRegister(parse_register(parts[1])?))
        }
        OpCode::PushRegister => {
            assert_length(&parts, 2)?;
            Ok(Instruction::PushRegister(parse_register(parts[1])?))
        }
        OpCode::Signal => {
            assert_length(&parts, 2)?;
            Ok(Instruction::Signal(parse_numeric(parts[1])?))
        }
    }
}

fn main() -> Result<(), String> {
    // ./asm file.asm

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
    for line in BufReader::new(file).lines() {
        let line_inner = line.map_err(|_x| "foo")?;
        if line_inner.is_empty() {
            continue;
        }
        if line_inner.starts_with(';') {
            continue;
        }
        let parts: Vec<_> = line_inner.split(' ').filter(|x| !x.is_empty()).collect();
        if parts.is_empty() {
            continue;
        }
        let instruction = handle_line(parts)?;
        let raw_instruction: u16 = instruction.encode_u16();
        // assumption: >>8 needs to mask for u16
        output.push((raw_instruction & 0xff) as u8);
        output.push((raw_instruction >> 8) as u8);
    }
    let mut stdout = io::stdout().lock();
    stdout.write_all(&output).map_err(|x| format!("{}", x))?;
    Ok(())
}
