use simplevm::Instruction::*;
use simplevm::Register::*;
use simplevm::*;

mod common;
use common::*;

#[test]
fn test_add() -> Result<(), String> {
    let mut vm = Machine::new(1024 * 4);
    run(
        &mut vm,
        &[
            Imm(A, 11),
            Imm(B, 15),
            Add(A, B, C),
            System(Zero, Zero, Nibble::new(SIGHALT)),
        ],
    )?;
    assert_reg!(vm, C, 26);
    Ok(())
}

#[test]
fn test_sub() -> Result<(), String> {
    let mut vm = Machine::new(1024 * 4);
    run(
        &mut vm,
        &[
            Imm(A, 20),
            Imm(B, 15),
            Sub(A, B, C),
            System(Zero, Zero, Nibble::new(SIGHALT)),
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
            Imm(A, 10),
            Imm(B, 52),
            Sub(A, B, C),
            System(Zero, Zero, Nibble::new(SIGHALT)),
        ],
    )?;
    assert_reg!(vm, C, u16::MAX - 41);

    Ok(())
}

#[test]
fn test_add_imm() -> Result<(), String> {
    let mut vm = Machine::new(1024 * 4);
    run(
        &mut vm,
        &[
            Imm(A, 23),
            AddImm(A, Literal7Bit::new(4)),
            System(Zero, Zero, Nibble::new(SIGHALT)),
        ],
    )?;
    assert_reg!(vm, A, 27);
    Ok(())
}

#[test]
fn test_shift_left() -> Result<(), String> {
    let mut vm = Machine::new(1024 * 4);
    run(
        &mut vm,
        &[
            Imm(C, 0xff),
            ShiftLeft(C, B, Nibble::new(4)),
            System(Zero, Zero, Nibble::new(SIGHALT)),
        ],
    )?;
    assert_reg!(vm, B, 0xff0);
    Ok(())
}

#[test]
fn test_shift_right() -> Result<(), String> {
    let mut vm = Machine::new(1024 * 4);
    run(
        &mut vm,
        &[
            Imm(A, 0x8fc),
            ShiftLeft(A, A, Nibble::new(4)),
            AddImm(A, Literal7Bit::new(0x7)),
            // A = 0x8FC7
            ShiftRightLogical(A, C, Nibble::new(3)),
            System(Zero, Zero, Nibble::new(SIGHALT)),
        ],
    )?;
    assert_reg!(vm, C, 0x11f8);

    run(
        &mut vm,
        &[
            Imm(A, 0xff0),
            ShiftLeft(A, A, Nibble::new(4)),
            AddImm(A, Literal7Bit::new(0x70)),
            // A = 0xff70
            ShiftRightArithmetic(A, C, Nibble::new(2)),
            System(Zero, Zero, Nibble::new(SIGHALT)),
        ],
    )?;
    assert_reg!(vm, C, 0xffdc);
    Ok(())
}
