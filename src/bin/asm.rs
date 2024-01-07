
use std::env;
use std::io;
use std::fs::File;
use std::io::{BufReader, BufRead, Write};
use std::path::Path;

fn main() -> Result<(), String> {
    // ./asm file.asm 

    let args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        panic!("usage: {} <input>", args[0]);
    }

    let file = File::open(Path::new(&args[1])).map_err(|x| format!("failed to open: {}", x))?;

    // for each line in file
    //  for each space separated token
    //    try to parse as base-16 number
    //    append to output if number
    //    else die
    //
    // eg
    // 00 01 02 03 04 05 10
    let mut output: Vec<u8> = Vec::new();
    for line in BufReader::new(file).lines() {
        let line_inner = line.map_err(|_x| "foo")?;
        for t in line_inner.split(" ").filter(|x| x.len() > 0) {
            let b = u8::from_str_radix(t, 16).map_err(|x| format!("parse int: {}", x))?;
            output.push(b);
        }
    }
    let mut stdout = io::stdout().lock();
    stdout.write_all(&output).map_err(|x| format!("{}", x))?;
    Ok(())
} 
