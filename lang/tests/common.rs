use lang::*;
use simplevm::*;

#[macro_export]
macro_rules! assert_reg {
    ($vm: expr, $r:expr, $v:expr) => {
        assert!(
            $vm.get_register($r) == $v,
            "expected {:X}, {} == {:X}",
            $v,
            $r,
            $vm.get_register($r)
        );
    };
}

#[macro_export]
macro_rules! assert_mem {
    ($vm: expr, $addr:expr, $v:expr) => {{
        let result = $vm.memory.read(($addr) as u32)?;
        assert!(
            result == $v,
            "expected {:X} @{:X}, got {:X}",
            $v,
            $addr,
            result
        );
    }};
}

pub const SIGHALT: u8 = 0xf0;

pub fn signal_halt(vm: &mut VM, _: u16) -> Result<(), String> {
    vm.halt = true;
    Ok(())
}

const MAX_TEST_CYCLES: u32 = 100_000;
pub fn run_program_with_memory_size(program: &str, memory: usize) -> Result<Machine, String> {
    let mut vm = Machine::default();
    vm.set_register(Register::SP, 1024 * 3);
    vm.define_handler(SIGHALT, signal_halt);

    let prog = run_parser(parse_ast, program).unwrap();
    let res = compile(prog, 0).unwrap();
    println!("{res}");
    let instructions = res.get_instructions().unwrap();
    let program_words: Vec<_> = instructions.iter().map(|x| x.encode_u16()).collect();
    unsafe {
        vm.map(0, memory, Box::new(LinearMemory::new(memory)))?;
        let program_bytes = program_words.align_to::<u8>().1;
        vm.vm
            .memory
            .load_from_vec(&program_bytes, res.program_start_offset)
            .map_err(|x| x.to_string())?;
    }
    vm.set_register(Register::PC, res.program_start_offset as u16);
    let mut cycle_count = 0;
    while !vm.is_halt() {
        vm.step()?;
        cycle_count += 1;
        if cycle_count >= MAX_TEST_CYCLES {
            return Err("max cycles exceeded".to_string());
        }
    }
    Ok(vm)
}

pub fn run_program(program: &str) -> Result<Machine, String> {
    run_program_with_memory_size(program, 0x8000)
}
