use simplevm::Instruction::*;
use simplevm::Register::*;
use simplevm::*;

mod common;
use common::*;

#[test]
fn test_loop() -> Result<(), String> {
    let mut vm = make_test_vm(4 * 1024)?;
    run(
        &mut vm,
        &[
            Imm(A, Literal12Bit::new_checked(5)?),
            // LABEL: start=2
            Test(A, Zero, TestOp::Neq),              // if A != 0
            AddIf(PC, PC, Nibble::new_checked(2)?),  //
            Imm(PC, Literal12Bit::new_checked(14)?), // GOTO: end
            AddImmSigned(A, Literal7Bit::from_signed(-1)?),
            AddImm(B, Literal7Bit::new_checked(1)?),
            Imm(PC, Literal12Bit::new_checked(2)?),
            // LABEL: end=14
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_reg!(vm, B, 5);
    Ok(())
}
