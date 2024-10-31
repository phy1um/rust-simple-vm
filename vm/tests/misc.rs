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
            BranchIf(Literal10Bit::new_checked(4)?), //
            Jump(Literal10Bit::new_checked(1)?),     // GOTO: end
            AddImmSigned(A, Literal7Bit::from_signed(-1)?),
            AddImm(B, Literal7Bit::new_checked(1)?),
            Branch(Literal10Bit::from_signed(-10)?),
            // LABEL: end=16
            Invalid,
            System(Zero, Zero, Nibble::new_checked(SIGHALT)?),
        ],
    )?;
    assert_reg!(vm, B, 5);
    Ok(())
}
