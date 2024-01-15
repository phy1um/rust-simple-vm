use crate::register::Register;
use std::str::FromStr;

/**
 * instruction = [ 0 0 0 0 0 0 0 0 | 0 0 0 0 0 0 0 0 ]
 *                 OPERATOR        | ARG(s)
 *                                 | 8 bit literal
 *                                 | REG1  | REG2
*/


#[derive(Debug)]
pub enum Instruction {
    Nop,
    Push(u8),
    PopRegister(Register),
    AddStack,
    AddRegister(Register, Register), 
    PushRegister(Register),
    Signal(u8),
}

impl Instruction {
    fn encode_r1(r: Register) -> u16 {
        (r as u16)&0xf << 8
    }

    fn encode_r2(r: Register) -> u16 {
        (r as u16)&0xf << 12
    }

    fn encode_num(u: u8) -> u16 {
        (u as u16) << 8
    }

    fn encode_rs(r1: Register, r2: Register) -> u16 {
        Self::encode_r1(r1) | Self::encode_r2(r2)
    }

    pub fn encode_u16(&self) -> u16 {
        match self {
            Self::Nop => OpCode::Nop as u16,
            Self::Push(x) => OpCode::Push as u16 | Self::encode_num(*x),
            Self::PopRegister(r) => OpCode::PopRegister as u16 | Self::encode_r1(*r),
            Self::PushRegister(r) => OpCode::PushRegister as u16 | Self::encode_r1(*r),
            Self::AddStack => OpCode::AddStack as u16,
            Self::AddRegister(r1, r2) => OpCode::AddRegister as u16 | Self::encode_rs(*r1,*r2),
            Self::Signal(x) => OpCode::Signal as u16 | Self::encode_num(*x),
        }
    }
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

#[repr(u8)]
#[derive(Debug)]
pub enum OpCode {
    Nop = 0x0,
    Push = 0x1,
    PopRegister = 0x2,
    PushRegister = 0x3,
    Signal = 0x0f,
    AddStack = 0x10,
    AddRegister = 0x11,
}

impl FromStr for OpCode {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Nop" => Ok(Self::Nop),
            "Push" => Ok(Self::Push),
            "Signal" => Ok(Self::Signal),
            "PopRegister" => Ok(Self::PopRegister),
            "PushRegister" => Ok(Self::PushRegister),
            "AddStack" => Ok(Self::AddStack),
            "AddRegister" => Ok(Self::AddRegister),
            _ => Err(format!("unknown opcode {}", s)),
        }
    }
}

impl TryFrom<u8> for OpCode {
    type Error = String;
    fn try_from(b: u8) -> Result<Self, Self::Error> {
        match b {
            x if x == Self::Nop as u8 => Ok(Self::Nop),
            x if x == Self::Push as u8 => Ok(Self::Push),
            x if x == Self::PopRegister as u8 => Ok(Self::PopRegister),
            x if x == Self::PushRegister as u8 => Ok(Self::PushRegister),
            x if x == Self::Signal as u8 => Ok(Self::Signal),
            x if x == Self::AddStack as u8 => Ok(Self::AddStack),
            x if x == Self::AddRegister as u8 => Ok(Self::AddRegister),
            _ => Err(format!("unknown opcode {:X}", b)),
        }
    }

}

