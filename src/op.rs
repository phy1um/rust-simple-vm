use std::str::FromStr;

use macros::{VmInstruction};
use crate::register::Register;

/**
 * instruction = [ 0 0 0 0 0 0 0 0 | 0 0 0 0 0 0 0 0 ]
 *                 OPERATOR        | ARG(s)
 *                                 | 8 bit literal
 *                                 | REG1  | REG2
*/

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
    #[opcode(0x20)]
    AddStack,
    #[opcode(0x21)]
    AddRegister(Register, Register), 
    #[opcode(0xF0)]
    Signal(u8),
}

fn parse_instruction_arg(ins: u16) -> u8 {
    ((ins & 0xff00) >> 8) as u8
}

impl TryFrom<u16> for Instruction {
    type Error = String;
    fn try_from(ins: u16) -> Result<Self, Self::Error> {
        let op = (ins & 0xff) as u8; 
        match OpCode::try_from(op)? {
            OpCode::Nop => Ok(Instruction::Nop),
            OpCode::Push => {
                let arg = parse_instruction_arg(ins);
                Ok(Instruction::Push(arg))
            },
            OpCode::PopRegister => {
                let reg = (ins&0xf00) >> 8;
                Register::from_u8(reg as u8)
                    .ok_or(format!("unknown register 0x{:X}", reg))
                    .map(|r| Instruction::PopRegister(r))
            },
            OpCode::PushRegister => {
                let reg = (ins&0xf00) >> 8;
                Register::from_u8(reg as u8)
                    .ok_or(format!("unknown register 0x{:X}", reg))
                    .map(|r| Instruction::PushRegister(r))
            }
            OpCode::AddStack => {
                Ok(Instruction::AddStack)
            },
            OpCode::AddRegister => {
                let reg1_raw = (ins&0xf00)>>8;
                let reg2_raw = (ins&0xf000)>>12;
                let reg1 = Register::from_u8(reg1_raw as u8)
                    .ok_or(format!("unknown register 0x{:X}", reg1_raw))?;
                let reg2 = Register::from_u8(reg2_raw as u8)
                    .ok_or(format!("unknown register 0x{:X}", reg2_raw))?;
                Ok(Instruction::AddRegister(reg1, reg2))
            }
            OpCode::Signal => {
                let arg = parse_instruction_arg(ins);
                Ok(Instruction::Signal(arg))
            },
        }
    }
}


