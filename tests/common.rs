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

pub const SIGHALT: u8 = 0x1;

pub fn signal_halt(vm: &mut Machine, _: u16) -> Result<(), String> {
    vm.halt = true;
    Ok(())
}

pub fn run(m: &mut Machine, program: &[Instruction]) -> Result<(), String> {
    m.reset();
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
