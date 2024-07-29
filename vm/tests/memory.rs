use simplevm::Instruction::*;
use simplevm::Register::*;
use simplevm::*;

mod common;
use common::*;

#[test]
fn test_load() -> Result<(), String> {
    let mut vm = make_test_vm(1024 * 5)?;
    run_with!(
        &mut vm,
        {
            vm.vm.memory.write2(0x100, 0x77)?;
            vm.vm.memory.write2(0x1000, 0x999)?;
        },
        Imm(B, Literal12Bit::new_checked(0x100)?),
        Imm(C, Literal12Bit::new_checked(0x100)?),
        ShiftLeft(C, C, Nibble::new_checked(4)?),
        LoadWord(A, B, Zero),
        LoadWord(M, C, Zero),
        System(Zero, Zero, Nibble::new_checked(SIGHALT)?)
    );
    assert_reg!(vm, A, 0x77);
    assert_reg!(vm, M, 0x999);
    Ok(())
}

#[test]
fn test_store() -> Result<(), String> {
    let mut vm = make_test_vm(1024 * 4)?;
    vm.map(0x0, 1024 * 5, Box::new(LinearMemory::new(1024 * 5)))?;
    run_with!(
        &mut vm,
        {},
        Imm(A, Literal12Bit::new_checked(0x99)?),
        Imm(B, Literal12Bit::new_checked(0x11)?),
        StoreWord(A, B, Zero),
        Imm(B, Literal12Bit::new_checked(0x22)?),
        StoreWord(A, B, Zero),
        System(Zero, Zero, Nibble::new_checked(SIGHALT)?)
    );
    assert_mem!(vm, 0x11, 0x99);
    assert_mem!(vm, 0x22, 0x99);
    assert_mem!(vm, 0x30, 0x00);
    Ok(())
}
