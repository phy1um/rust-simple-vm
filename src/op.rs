use std::fmt;
use std::str::FromStr;

use crate::register::Register;
use macros::VmInstruction;

/**
 * instruction = [ 0 0 0 0 0 0 0 0 | 0 0 0 0 0 0 0 0 ]
 *                 OPERATOR        | ARG(s)
 *                                 | 8 bit literal
 *                                 | REG1  | REG2
*/

pub enum InstructionParseError {
    NoContent,
    Fail(String),
}

impl From<String> for InstructionParseError {
    fn from(value: String) -> Self {
        Self::Fail(value)
    }
}

#[derive(Debug, VmInstruction)]
pub enum Instruction {
    #[opcode(0x0)]
    Nop,
    #[opcode(0x1)]
    Push(u8),
    #[opcode(0x2)]
    PopRegister(Register),
    #[opcode(0x3)]
    PushRegister(Register),
    #[opcode(0x4)]
    SetRegister(Register, Register),
    #[opcode(0x6)]
    LoadARegister(Register),
    #[opcode(0x7)]
    LoadAImm(u8),
    #[opcode(0x8)]
    LoadBRegister(Register),
    #[opcode(0x9)]
    LoadBImm(u8),
    #[opcode(0xA)]
    LoadCRegister(Register),
    #[opcode(0xB)]
    LoadCImm(u8),
    #[opcode(0x20)]
    AddStack,
    #[opcode(0x21)]
    AddRegister(Register, Register),
    #[opcode(0x22)]
    SubStack,
    #[opcode(0x23)]
    SubRegister(Register, Register),

    #[opcode(0x40)]
    BranchImm(i8), // branch relative (PC += x if FLAGS[c])
    #[opcode(0x41)]
    BranchRegister(Register), // branch absolute (PC = Registers[r] if FLAGS[c])

    #[opcode(0x50)]
    IfZero(Register),

    #[opcode(0xF0)]
    Signal(u8),
}

#[cfg(test)]
mod test {
    use super::Instruction::*;
    use super::*;

    #[test]
    fn test_encodings() {
        assert_eq!(SubStack.encode_u16(), 0x22 as u16);
        assert_eq!(Push(0x5).encode_u16(), 0x0501 as u16);
        assert_eq!(
            AddRegister(Register::B, Register::BP).encode_u16(),
            0x6121 as u16
        );
    }
}
