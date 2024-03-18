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
        let result = $vm
            .memory
            .read2(($addr) as u32)
            .ok_or("invalid read".to_string())?;
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

pub const SIGHALT: u8 = 0x1;

pub fn signal_halt(vm: &mut Machine, _: u16) -> Result<(), String> {
    vm.halt = true;
    Ok(())
}

pub fn run(m: &mut Machine, program: &[Instruction]) -> Result<(), String> {
    m.reset();
    run_program_code(m, program)
}

pub fn run_program_code(m: &mut Machine, program: &[Instruction]) -> Result<(), String> {
    let program_words: Vec<_> = program.iter().map(|x| x.encode_u16()).collect();
    unsafe {
        let program_bytes = program_words.align_to::<u8>().1;
        m.memory.load_from_vec(&program_bytes, 0);
    }
    m.set_register(Register::SP, 0x1000);
    m.define_handler(SIGHALT, signal_halt);
    while !m.halt {
        m.step()?;
    }
    Ok(())
}
