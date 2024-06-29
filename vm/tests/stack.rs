use simplevm::Instruction::*;
use simplevm::Register::*;
use simplevm::*;

mod common;
use common::*;

#[test]
fn test_push() -> Result<(), String> {
    let mut vm = make_test_vm(1024 * 4)?;
    run(
        &mut vm,
        &[
            Imm(A, Literal12Bit::new_checked(123)?),
            Stack(A, SP, StackOp::Push),
            Imm(B, Literal12Bit::new_checked(222)?),
            Stack(B, SP, StackOp::Push),
            Imm(A, Literal12Bit::new_checked(1)?),
            Stack(A, SP, StackOp::Push),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_mem!(vm, vm.get_register(SP) - 2, 1);
    assert_mem!(vm, vm.get_register(SP) - 4, 222);
    assert_mem!(vm, vm.get_register(SP) - 6, 123);
    Ok(())
}

#[test]
fn test_pop() -> Result<(), String> {
    let mut vm = make_test_vm(1024 * 4)?;
    run(
        &mut vm,
        &[
            Imm(A, Literal12Bit::new_checked(188)?),
            Stack(A, SP, StackOp::Push),
            Imm(A, Literal12Bit::new_checked(521)?),
            Stack(A, SP, StackOp::Push),
            Stack(B, SP, StackOp::Pop),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_reg!(vm, B, 521);
    assert_mem!(vm, vm.get_register(SP) - 2, 188);
    Ok(())
}

#[test]
fn test_swap() -> Result<(), String> {
    let mut vm = make_test_vm(1024 * 4)?;
    run(
        &mut vm,
        &[
            Imm(A, Literal12Bit::new_checked(333)?),
            Imm(B, Literal12Bit::new_checked(111)?),
            Stack(A, SP, StackOp::Push),
            Stack(B, SP, StackOp::Push),
            Stack(Zero, SP, StackOp::Swap),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_mem!(vm, vm.get_register(SP) - 2, 333);
    assert_mem!(vm, vm.get_register(SP) - 4, 111);
    Ok(())
}

#[test]
fn test_peek() -> Result<(), String> {
    let mut vm = make_test_vm(1024 * 4)?;
    run(
        &mut vm,
        &[
            Imm(A, Literal12Bit::new_checked(188)?),
            Stack(A, SP, StackOp::Push),
            Imm(A, Literal12Bit::new_checked(521)?),
            Stack(A, SP, StackOp::Push),
            Stack(B, SP, StackOp::Peek),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_reg!(vm, B, 521);
    assert_mem!(vm, vm.get_register(SP) - 2, 521);
    Ok(())
}

#[test]
fn test_dup() -> Result<(), String> {
    let mut vm = make_test_vm(1024 * 4)?;
    run(
        &mut vm,
        &[
            Imm(A, Literal12Bit::new_checked(88)?),
            Stack(A, SP, StackOp::Push),
            Stack(Zero, SP, StackOp::Dup),
            Stack(Zero, SP, StackOp::Dup),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_mem!(vm, vm.get_register(SP) - 2, 88);
    assert_mem!(vm, vm.get_register(SP) - 4, 88);
    assert_mem!(vm, vm.get_register(SP) - 6, 88);
    Ok(())
}

#[test]
fn test_rot() -> Result<(), String> {
    let mut vm = make_test_vm(1024 * 4)?;
    run(
        &mut vm,
        &[
            Imm(A, Literal12Bit::new_checked(11)?),
            Stack(A, SP, StackOp::Push),
            Imm(A, Literal12Bit::new_checked(22)?),
            Stack(A, SP, StackOp::Push),
            Imm(A, Literal12Bit::new_checked(33)?),
            Stack(A, SP, StackOp::Push),
            Stack(Zero, SP, StackOp::Rotate),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_mem!(vm, vm.get_register(SP) - 2, 22);
    assert_mem!(vm, vm.get_register(SP) - 4, 11);
    assert_mem!(vm, vm.get_register(SP) - 6, 33);
    Ok(())
}

#[test]
fn test_stack_add() -> Result<(), String> {
    let mut vm = make_test_vm(1024 * 4)?;
    run(
        &mut vm,
        &[
            Imm(A, Literal12Bit::new_checked(40)?),
            Stack(A, SP, StackOp::Push),
            Imm(A, Literal12Bit::new_checked(60)?),
            Stack(A, SP, StackOp::Push),
            Stack(Zero, SP, StackOp::Add),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_mem!(vm, vm.get_register(SP) - 2, 100);
    Ok(())
}

#[test]
fn test_stack_sub() -> Result<(), String> {
    let mut vm = make_test_vm(1024 * 4)?;
    run(
        &mut vm,
        &[
            Imm(A, Literal12Bit::new_checked(40)?),
            Stack(A, SP, StackOp::Push),
            Imm(A, Literal12Bit::new_checked(60)?),
            Stack(A, SP, StackOp::Push),
            Stack(Zero, SP, StackOp::Sub),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_mem!(vm, vm.get_register(SP) - 2, 20);
    Ok(())
}

#[test]
fn test_load_offset() -> Result<(), String> {
    let mut vm = make_test_vm(1024 * 4)?;
    run(
        &mut vm,
        &[
            Imm(A, Literal12Bit::new_checked(0x33)?),
            Stack(A, SP, StackOp::Push),
            Imm(A, Literal12Bit::new_checked(0x22)?),
            Stack(A, SP, StackOp::Push),
            Imm(A, Literal12Bit::new_checked(0x11)?),
            Stack(A, SP, StackOp::Push),
            LoadStackOffset(C, SP, Nibble::new_checked(3)?),
            LoadStackOffset(B, SP, Nibble::new_checked(2)?),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_reg!(vm, C, 0x33);
    assert_reg!(vm, B, 0x22);
    Ok(())
}
