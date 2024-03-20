use simplevm::Instruction::*;
use simplevm::Register::*;
use simplevm::*;

mod common;
use common::*;

#[test]
fn test_loop() -> Result<(), String> {
    let mut vm = Machine::new(4 * 1024);
    run(
        &mut vm,
        &[
            Imm(A, 5),
            // LABEL: start=2
            Test(A, Zero, TestOp::Neq), // if A != 0
            AddIf(PC, PC, Nibble::new(2)),  //
            Imm(PC, 14),                // GOTO: end
            AddImmSigned(A, Literal7Bit::from_signed(-1)),
            AddImm(B, Literal7Bit::new(1)),
            Imm(PC, 2),
            // LABEL: end=14
            System(Zero, Zero, Nibble::new(SIGHALT)),
        ],
    )?;
    assert_reg!(vm, B, 5);
    Ok(())
}
