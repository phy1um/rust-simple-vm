use simplevm::Instruction::*;
use simplevm::Register::*;
use simplevm::*;

mod common;
use common::*;

#[test]
fn test_jump() -> Result<(), String> {
    let mut vm = Machine::new(1024 * 4);
    run(
        &mut vm,
        &[
            Imm(PC, Literal12Bit::new_checked(10)?),
            Invalid(0),
            Invalid(0),
            Invalid(0),
            Invalid(0),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_reg!(vm, PC, 12);
    Ok(())
}

#[test]
fn test_jump_offset() -> Result<(), String> {
    let mut vm = Machine::new(1024 * 4);
    run(
        &mut vm,
        &[
            Add(Zero, Zero, Zero),
            Add(Zero, Zero, Zero),
            Add(Zero, Zero, Zero),
            JumpOffset(Literal10Bit::new_checked(10)?),
            Invalid(0),
            Invalid(0),
            Invalid(0),
            Invalid(0),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_reg!(vm, PC, 18);
    Ok(())
}

#[test]
fn test_branch() -> Result<(), String> {
    let mut vm = Machine::new(1024 * 4);
    run(
        &mut vm,
        &[
            Imm(A, Literal12Bit::new_checked(12)?),
            Imm(B, Literal12Bit::new_checked(13)?),
            Test(A, B, TestOp::Neq),
            AddIf(PC, PC, Nibble::new_checked(0x4)?),
            Invalid(0),
            Invalid(0),
            Invalid(0),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;

    run(
        &mut vm,
        &[
            Imm(A, Literal12Bit::new_checked(12)?),
            Imm(B, Literal12Bit::new_checked(13)?),
            Test(A, B, TestOp::Neq),
            AddIf(PC, PC, Nibble::new_checked(0x3)?),
            Invalid(0),
            Invalid(0),
            AddIf(PC, PC, Nibble::new_checked(0xf)?),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    Ok(())
}

#[test]
fn test_jump_and_link() -> Result<(), String> {
    let mut vm = Machine::new(1024 * 4);
    run(
        &mut vm,
        &[
            Imm(B, Literal12Bit::new_checked(4)?),
            SetAndSave(PC, B, C),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_reg!(vm, C, 2);

    run(
        &mut vm,
        &[
            Imm(A, Literal12Bit::new_checked(8)?),
            AddAndSave(PC, A, B),
            Invalid(0),
            Invalid(0),
            Invalid(0),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_reg!(vm, B, 2);
    Ok(())
}
