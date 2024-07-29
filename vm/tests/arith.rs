use simplevm::Instruction::*;
use simplevm::Register::*;
use simplevm::*;

mod common;
use common::*;

const CASES: [(u16, u16); 10] = [
    (1, 1),
    (2, 2),
    (12, 1),
    (2, 4),
    (32, 33),
    (111, 112),
    (1000, 52),
    (201, 97),
    (333, 333),
    (300, 20),
];

#[test]
fn test_add() {
    for (a, b) in CASES {
        let mut vm = make_test_vm(1024 * 4).unwrap();
        run(
            &mut vm,
            &[
                Imm(C, Literal12Bit::new_checked(a).unwrap()),
                Imm(B, Literal12Bit::new_checked(b).unwrap()),
                Add(A, B, C),
                System(Zero, Zero, Nibble::new_checked(SIGHALT).unwrap()),
            ],
        )
        .unwrap();
        assert_reg!(vm, A, a + b);
    }
}

#[test]
fn test_mul() {
    for (a, b) in CASES {
        let mut vm = make_test_vm(1024 * 4).unwrap();
        run(
            &mut vm,
            &[
                Imm(C, Literal12Bit::new_checked(a).unwrap()),
                Imm(B, Literal12Bit::new_checked(b).unwrap()),
                Mul(A, B, C),
                System(Zero, Zero, Nibble::new_checked(SIGHALT).unwrap()),
            ],
        )
        .unwrap();
        assert_reg!(vm, A, a.wrapping_mul(b));
    }
}

#[test]
fn test_and() {
    let mut vm = make_test_vm(1024 * 4).unwrap();
    for (a, b) in CASES.iter() {
        run(
            &mut vm,
            &[
                Imm(C, Literal12Bit::new_checked(*a).unwrap()),
                Imm(B, Literal12Bit::new_checked(*b).unwrap()),
                And(A, B, C),
                System(Zero, Zero, Nibble::new_checked(SIGHALT).unwrap()),
            ],
        )
        .unwrap();
        assert_reg!(vm, A, a & b);
    }
}

#[test]
fn test_or() {
    let cases = vec![(1, 12), (42, 51), (1000, 52), (32, 33), (111, 97)];
    let mut vm = make_test_vm(1024 * 4).unwrap();
    for (a, b) in cases.iter() {
        run(
            &mut vm,
            &[
                Imm(C, Literal12Bit::new_checked(*a).unwrap()),
                Imm(B, Literal12Bit::new_checked(*b).unwrap()),
                Or(A, B, C),
                System(Zero, Zero, Nibble::new_checked(SIGHALT).unwrap()),
            ],
        )
        .unwrap();
        assert_reg!(vm, A, a | b);
    }
}

#[test]
fn test_xor() {
    let mut vm = make_test_vm(1024 * 4).unwrap();
    for (a, b) in CASES.iter() {
        run(
            &mut vm,
            &[
                Imm(C, Literal12Bit::new_checked(*a).unwrap()),
                Imm(B, Literal12Bit::new_checked(*b).unwrap()),
                Xor(A, B, C),
                System(Zero, Zero, Nibble::new_checked(SIGHALT).unwrap()),
            ],
        )
        .unwrap();
        assert_reg!(vm, A, a ^ b);
    }
}

#[test]
fn test_mod() {
    let mut vm = make_test_vm(1024 * 4).unwrap();
    for (a, b) in CASES.iter() {
        run(
            &mut vm,
            &[
                Imm(C, Literal12Bit::new_checked(*a).unwrap()),
                Imm(B, Literal12Bit::new_checked(*b).unwrap()),
                Mod(A, C, B),
                System(Zero, Zero, Nibble::new_checked(SIGHALT).unwrap()),
            ],
        )
        .unwrap();
        assert_reg!(vm, A, a % b);
    }
}

#[test]
fn test_sub() {
    for (a, b) in CASES {
        let mut vm = make_test_vm(1024 * 4).unwrap();
        run(
            &mut vm,
            &[
                Imm(B, Literal12Bit::new_checked(a).unwrap()),
                Imm(C, Literal12Bit::new_checked(b).unwrap()),
                Sub(A, B, C),
                System(Zero, Zero, Nibble::new_checked(SIGHALT).unwrap()),
            ],
        )
        .unwrap();
        assert_reg!(vm, A, a.wrapping_sub(b));
    }

    {
        let mut vm = make_test_vm(1024 * 4).unwrap();
        run(
            &mut vm,
            &[
                Imm(B, Literal12Bit::new_checked(10).unwrap()),
                Imm(C, Literal12Bit::new_checked(52).unwrap()),
                Sub(A, B, C),
                System(Zero, Zero, Nibble::new_checked(SIGHALT).unwrap()),
            ],
        )
        .unwrap();
        assert_reg!(vm, A, u16::MAX - 41);
    }
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
            ShiftLeft(B, C, Nibble::new_checked(4)?),
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
            ShiftRightLogical(C, A, Nibble::new_checked(3)?),
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
            ShiftRightArithmetic(C, A, Nibble::new_checked(2)?),
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_reg!(vm, C, 0xffdc);
    Ok(())
}
