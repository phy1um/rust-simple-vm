
use simplevm::*;
use simplevm::Instruction::*;
use simplevm::Register::*;


const SIGHALT: u8 = 0x1;

fn signal_halt(vm: &mut Machine, _: u16) -> Result<(), String> {
    vm.halt = true;
    Ok(())
}

fn run(m: &mut Machine, program: &[Instruction]) -> Result<(), String> {
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

#[test]
fn test_add() -> Result<(), String> {
    let mut vm = Machine::new(1024*4);
    vm.reset();
    run(&mut vm, &[
        Imm(A, 11),
        Imm(B, 15),
        Add(A, B, C),
        System(Zero, Zero, Nibble::new(SIGHALT)),
    ])?;
    assert!(vm.get_register(C) == 26, "expected {}, C == {}", 26, vm.get_register(C));
    Ok(())
}

#[test]
fn test_sub() -> Result<(), String> {
    let mut vm = Machine::new(1024*4);
    vm.reset();
    run(&mut vm, &[
        Imm(A, 20),
        Imm(B, 15),
        Sub(A, B, C),
        System(Zero, Zero, Nibble::new(SIGHALT)),
    ])?;
    assert!(vm.get_register(C) == 5, "expected {}, C == {}", 5, vm.get_register(C));

    vm.reset();
    run(&mut vm, &[
        Imm(A, 10),
        Imm(B, 52),
        Sub(A, B, C),
        System(Zero, Zero, Nibble::new(SIGHALT)),
    ])?;
    assert!(vm.get_register(C) == u16::MAX - 41, "expected {}, C == {}", u16::MAX - 41, vm.get_register(C));

    Ok(())
   
}
