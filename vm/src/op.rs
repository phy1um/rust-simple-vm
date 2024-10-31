use macros::VmInstruction;
use std::fmt;
use std::str::FromStr;

use crate::op_fields::*;
use crate::register::Register;

/**
 * TYPE A
 * 1RRR LLLL LLLL LLLL
 * TYPE B
 * 0RRR SSSA AAAA DDDD
 * Add (A = 0)
 * [ assume register A = 1, B = 2 ]
 * Reg[D] = Reg[R] + Reg[S]
 * 1001 0100 0000 0001
 * Reg[0] is always = 0
 * Reg[A] = Reg[B] + Reg[Zero]
 * 1000 0100 0000 0001
 *
 * Stack (A = 0xf)
 * Push (D = 0)
 * 1001 1010 1111 0000
 * Pop (D = 1)
 * 1010 1010 1111 0001
 *
 * 1000 0000 0000 0000
 */

#[derive(Debug)]
pub enum InstructionParseError {
    NoContent,
    Fail(String),
}

pub trait InstructionPart {
    fn as_mask(&self) -> u16;
    fn from_instruction(ins: u16) -> Self;
}

#[derive(Debug, VmInstruction, PartialEq, Eq, Clone)]
pub enum Instruction {
    #[opcode(0xff)]
    Imm(Register, Literal12Bit), // Imm has unique instruction format, it doesn't use an opcode.
    #[opcode(0x0)]
    Invalid,
    // Binary Operators
    #[opcode(0x1)]
    Add(Register, Register, Register),
    #[opcode(0x2)]
    Sub(Register, Register, Register),
    #[opcode(0x3)]
    Mul(Register, Register, Register),
    #[opcode(0x4)]
    And(Register, Register, Register),
    #[opcode(0x5)]
    Or(Register, Register, Register),
    #[opcode(0x6)]
    Xor(Register, Register, Register),
    #[opcode(0x1e)]
    Mod(Register, Register, Register),
    // Register+Imm
    #[opcode(0x7)]
    AddImm(Register, Literal7Bit),
    #[opcode(0x8)]
    AddImmSigned(Register, Literal7Bit),
    // Shifts
    #[opcode(0x9)]
    ShiftLeft(Register, Register, Nibble),
    #[opcode(0xa)]
    ShiftRightLogical(Register, Register, Nibble),
    #[opcode(0xb)]
    ShiftRightArithmetic(Register, Register, Nibble),
    // Load and Store
    #[opcode(0xc)]
    LoadWord(Register, Register, Register), // R0 = RAM[R1 | (R2<<16)]
    #[opcode(0xd)]
    StoreWord(Register, Register, Register), // RAM[R1 | (R2<<16)] = R0
    #[opcode(0xe)]
    LoadByte(Register, Register, Register),
    #[opcode(0xf)]
    StoreByte(Register, Register, Register),
    // Compound operations
    #[opcode(0x10)]
    SetAndSave(Register, Register, Register), // R2 = R0, R0 = R1
    #[opcode(0x11)]
    AddAndSave(Register, Register, Register), // R2 = R0, R0 = R0+R1
    // Control flow
    #[opcode(0x12)]
    Test(Register, Register, TestOp),
    #[opcode(0x13)]
    AddIf(Register, Register, Nibble),
    // Stack
    #[opcode(0x14)]
    Stack(Register, Register, StackOp),
    #[opcode(0x15)]
    LoadStackOffset(Register, Register, Nibble),
    #[opcode(0x16)]
    Jump(Literal10Bit),
    #[opcode(0x17)]
    JumpRegister(Register, Register),
    #[opcode(0x18)]
    BranchIf(Literal10Bit),
    #[opcode(0x19)]
    Branch(Literal10Bit),
    #[opcode(0x1a)]
    BranchRegisterIf(Register, Literal7Bit),
    // Syscalls
    #[opcode(0x1f)]
    System(Register, Register, Nibble),
}

#[cfg(test)]
mod test {
    use super::Instruction::*;
    use super::*;
    use crate::register::Register::*;

    #[test]
    fn test_encodings() -> Result<(), String> {
        let ops = vec![
            Imm(M, Literal12Bit::new_checked(0x30)?),
            AddImm(C, Literal7Bit::new_checked(0x20)?),
            AddImmSigned(A, Literal7Bit::new_checked(0x7)?),
            Add(C, B, A),
            Sub(D, BP, SP),
            Mul(D, BP, SP),
            And(D, BP, SP),
            Or(D, BP, SP),
            Xor(D, BP, SP),
            Mod(C, A, Zero),
            ShiftLeft(M, BP, Nibble::new_checked(0xe)?),
            ShiftRightLogical(M, BP, Nibble::new_checked(0xe)?),
            ShiftRightArithmetic(M, BP, Nibble::new_checked(0xe)?),
            LoadWord(A, C, M),
            LoadByte(A, C, M),
            StoreWord(C, A, M),
            StoreByte(C, A, M),
            SetAndSave(A, B, C),
            AddAndSave(D, B, C),
            Test(BP, A, TestOp::Gte),
            AddIf(D, A, Nibble::new_checked(0x0)?),
            Stack(B, SP, StackOp::Dup),
            LoadStackOffset(A, BP, Nibble::new_checked(0x3)?),
            System(A, B, Nibble::new_checked(0x3)?),
        ];
        let encoded: Vec<_> = ops.iter().map(|x| x.encode_u16()).collect();
        for (l, r) in ops.iter().zip(encoded.iter()) {
            assert_eq!(*l, Instruction::try_from(*r)?);
        }
        Ok(())
    }
}
