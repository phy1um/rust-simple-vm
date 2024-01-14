use crate::register::Register;

#[derive(Debug)]
pub enum Instruction {
    Nop,
    Push(u8),
    PopRegister(Register),
    AddStack,
    AddRegister(Register, Register),
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
            Self::AddStack => OpCode::AddStack as u16,
            Self::AddRegister(r1, r2) => OpCode::AddRegister as u16 | Self::encode_rs(*r1,*r2),
            Self::Signal(x) => OpCode::Signal as u16 | Self::encode_num(*x),
        }
    }
}

#[repr(u8)]
#[derive(Debug)]
pub enum OpCode {
    Nop = 0x0,
    Push = 0x1,
    PopRegister = 0x2,
    Signal = 0x0f,
    AddStack = 0x10,
    AddRegister = 0x11,
}

impl OpCode {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "Nop" => Some(Self::Nop),
            "Push" => Some(Self::Push),
            "Signal" => Some(Self::Signal),
            "PopRegister" => Some(Self::PopRegister),
            "AddStack" => Some(Self::AddStack),
            "AddRegister" => Some(Self::AddRegister),
            _ => None,
        }
    }

    pub fn from_u8(b: u8) -> Option<Self> {
        match b {
            x if x == Self::Nop as u8 => Some(Self::Nop),
            x if x == Self::Push as u8 => Some(Self::Push),
            x if x == Self::PopRegister as u8 => Some(Self::PopRegister),
            x if x == Self::Signal as u8 => Some(Self::Signal),
            x if x == Self::AddStack as u8 => Some(Self::AddStack),
            x if x == Self::AddRegister as u8 => Some(Self::AddRegister),
            _ => None,
        }
    }
}


