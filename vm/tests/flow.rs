use simplevm::Instruction::*;
use simplevm::Register::*;
use simplevm::*;

mod common;
use common::*;

#[test]
fn test_branch() -> Result<(), String> {
    let mut vm = make_test_vm(1024 * 4)?;
    run(
        &mut vm,
        &[
            Imm(A, Literal12Bit::new_checked(12)?),
            Imm(B, Literal12Bit::new_checked(13)?),
            Test(A, B, TestOp::Neq),
            BranchIf(Literal10Bit::new_checked(8)?),
            Invalid,
            Invalid,
            Invalid,
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;

    run(
        &mut vm,
        &[
            Imm(A, Literal12Bit::new_checked(12)?),
            Imm(B, Literal12Bit::new_checked(13)?),
            Test(A, B, TestOp::Neq),
            BranchIf(Literal10Bit::new_checked(6)?),
            Invalid,
            Invalid,
            BranchIf(Literal10Bit::new_checked(0xf)?),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    Ok(())
}

#[test]
fn test_jump() -> Result<(), String> {
    let mut vm = make_test_vm(1024 * 4)?;
    run(
        &mut vm,
        &[
            Imm(C, Literal12Bit::new_checked(999)?),
            Jump(Literal10Bit::new_checked(1)?),
            Imm(C, Literal12Bit::new_checked(7)?),
            Invalid,
            Invalid,
            Invalid,
            Invalid,
            Invalid,
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_reg!(vm, C, 999);
    Ok(())
}

#[test]
fn test_jump_register() -> Result<(), String> {
    let mut vm = make_test_vm(1024 * 4)?;
    run(
        &mut vm,
        &[
            Imm(B, Literal12Bit::new_checked(10)?),
            Imm(A, Literal12Bit::new_checked(82)?),
            JumpRegister(Zero, B),
            Imm(A, Literal12Bit::new_checked(999)?),
            Invalid,
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_reg!(vm, A, 82);
    Ok(())
}
