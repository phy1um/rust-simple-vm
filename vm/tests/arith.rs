use simplevm::Instruction::*;
use simplevm::Register::*;
use simplevm::*;

mod common;
use common::*;

#[test]
fn test_add() -> Result<(), String> {
    let mut vm = make_test_vm(1024 * 4)?;
    run(
        &mut vm,
        &[
            Imm(A, Literal12Bit::new_checked(11)?),
            Imm(B, Literal12Bit::new_checked(15)?),
            Add(A, B, C),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_reg!(vm, C, 26);
    Ok(())
}

#[test]
fn test_sub() -> Result<(), String> {
    let mut vm = make_test_vm(1024 * 4)?;
    run(
        &mut vm,
        &[
            Imm(A, Literal12Bit::new_checked(20)?),
            Imm(B, Literal12Bit::new_checked(15)?),
            Sub(A, B, C),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert!(
        vm.get_register(C) == 5,
        "expected {}, C == {}",
        5,
        vm.get_register(C)
    );

    run(
        &mut vm,
        &[
            Imm(A, Literal12Bit::new_checked(10)?),
            Imm(B, Literal12Bit::new_checked(52)?),
            Sub(A, B, C),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_reg!(vm, C, u16::MAX - 41);

    Ok(())
}

#[test]
fn test_add_imm() -> Result<(), String> {
    let mut vm = make_test_vm(1024 * 4)?;
    run(
        &mut vm,
        &[
            Imm(A, Literal12Bit::new_checked(23)?),
            AddImm(A, Literal7Bit::new_checked(4)?),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_reg!(vm, A, 27);
    Ok(())
}

#[test]
fn test_add_imm_signed() -> Result<(), String> {
    let mut vm = make_test_vm(1024 * 4)?;
    run(
        &mut vm,
        &[
            Imm(C, Literal12Bit::new_checked(0x5E)?),
            AddImmSigned(C, Literal7Bit::from_signed(-5)?),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_reg!(vm, C, 0x59);

    run(
        &mut vm,
        &[
            Imm(B, Literal12Bit::new_checked(29)?),
            AddImmSigned(B, Literal7Bit::from_signed(-29)?),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_reg!(vm, B, 0);
    Ok(())
}

#[test]
fn test_shift_left() -> Result<(), String> {
    let mut vm = make_test_vm(1024 * 4)?;
    run(
        &mut vm,
        &[
            Imm(C, Literal12Bit::new_checked(0xff)?),
            ShiftLeft(C, B, Nibble::new_checked(4)?),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_reg!(vm, B, 0xff0);
    Ok(())
}

#[test]
fn test_shift_right() -> Result<(), String> {
    let mut vm = make_test_vm(1024 * 4)?;
    run(
        &mut vm,
        &[
            Imm(A, Literal12Bit::new_checked(0x8fc)?),
            ShiftLeft(A, A, Nibble::new_checked(4)?),
            AddImm(A, Literal7Bit::new_checked(0x7)?),
            // A = 0x8FC7
            ShiftRightLogical(A, C, Nibble::new_checked(3)?),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_reg!(vm, C, 0x11f8);

    run(
        &mut vm,
        &[
            Imm(A, Literal12Bit::new_checked(0xff0)?),
            ShiftLeft(A, A, Nibble::new_checked(4)?),
            AddImm(A, Literal7Bit::new_checked(0x70)?),
            // A = 0xff70
            ShiftRightArithmetic(A, C, Nibble::new_checked(2)?),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_reg!(vm, C, 0xffdc);
    Ok(())
}
