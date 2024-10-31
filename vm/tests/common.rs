use simplevm::binfmt::BinaryFile;
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
        let result = $vm.vm.memory.read2(($addr) as u32)?;
        assert!(
            result == $v,
            "expected {:X} @{:X}, got {:X}",
            $v,
            $addr,
            result
        );
    }};
}

#[macro_export]
macro_rules! assert_flag_set {
    ($vm: expr, $flg:expr) => {
        assert!($vm.test_flag($flg), "expected flag {:?} set", $flg);
    };
}

#[macro_export]
macro_rules! assert_flag_unset {
    ($vm: expr, $flg:expr) => {
        assert!(!$vm.test_flag($flg), "expected flag {:?} unset", $flg);
    };
}

#[macro_export]
macro_rules! run_with {
    ($vm:expr, $pre:block, $($prog:expr),+) => {
        $vm.reset();
        $pre;
        run_program_code($vm, &[$($prog),*])?;
    }
}

pub fn make_test_vm(memory: usize) -> Result<Machine, String> {
    let mut vm = Machine::default();
    vm.define_handler(SIGHALT, signal_halt);
    if memory > 0 {
        vm.map(0x0, memory, Box::new(LinearMemory::new(memory)))?;
    }
    Ok(vm)
}

pub const SIGHALT: u8 = 0xf;

pub fn signal_halt(vm: &mut VM, _: u16) -> Result<(), String> {
    vm.halt = true;
    Ok(())
}

#[allow(dead_code)]
pub fn run(m: &mut Machine, program: &[Instruction]) -> Result<(), String> {
    m.reset();
    run_program_code(m, program)
}

pub fn run_program_code(m: &mut Machine, program: &[Instruction]) -> Result<(), String> {
    let program_words: Vec<_> = program.iter().map(|x| x.encode_u16()).collect();
    unsafe {
        let program_bytes = program_words.align_to::<u8>().1;
        m.vm.memory
            .load_from_vec(&program_bytes, 0)
            .map_err(|x| x.to_string())?;
    }
    m.set_register(Register::SP, 1024 * 3);
    while !m.vm.halt {
        m.step()?;
    }
    Ok(())
}

const MAX_TEST_CYCLES: u32 = 100_000;
pub fn run_binary(m: &mut Machine, bin: &BinaryFile) -> Result<(), String> {
    bin.load_to_vm(m)?;
    let mut cycle_count = 0;
    while !m.is_halt() {
        m.step().map_err(|s| error_with_context(m, &s))?;
        cycle_count += 1;
        if cycle_count >= MAX_TEST_CYCLES {
            return Err("max cycles exceeded".to_string());
        }
    }
    Ok(())
}

fn error_with_context(vm: &Machine, s: &str) -> String {
    format!(
        "!! VM ERROR !!: {s} @ PC={}\nA: {} | B: {} | C: {} | D: {} |\n M: {} | BP: {} | SP: {}",
        vm.get_program_counter(),
        vm.get_register(Register::A),
        vm.get_register(Register::B),
        vm.get_register(Register::C),
        vm.get_register(Register::D),
        vm.get_register(Register::M),
        vm.get_register(Register::BP),
        vm.get_register(Register::SP),
    )
}
