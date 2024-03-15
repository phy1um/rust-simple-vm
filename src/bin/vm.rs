use std::env;
use std::fs::File;
use std::io::{BufReader, Read};
use std::path::Path;

use simplevm::{Machine, Register};

fn signal_halt(vm: &mut Machine, _: u16) -> Result<(), String> {
    vm.halt = true;
    Ok(())
}

pub fn main() -> Result<(), String> {
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

    let mut vm = Machine::new();
    vm.set_register(Register::SP, 0x1000);
    vm.define_handler(0xf0, signal_halt);
    vm.memory.load_from_vec(&program, 0);
    while !vm.halt {
        println!("{}", vm.state());
        vm.step()?;
    }
    println!("A = {}", vm.get_register(Register::A));
    Ok(())
}
