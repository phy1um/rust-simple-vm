use std::fmt;

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum Register {
    A, B, C, M, SP, PC, BP, FLAGS, 
}

impl Register {
    pub fn from_u8(v: u8) -> Option<Self> {
        match v {
            x if x == Register::A as u8 => Some(Register::A),
            x if x == Register::B as u8 => Some(Register::B),
            x if x == Register::C as u8 => Some(Register::C),
            x if x == Register::M as u8 => Some(Register::M),
            x if x == Register::SP as u8 => Some(Register::SP),
            x if x == Register::PC as u8 => Some(Register::PC),
            x if x == Register::BP as u8 => Some(Register::BP),
            x if x == Register::FLAGS as u8 => Some(Register::FLAGS),
            _ => None,
        }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::A => write!(f, "A"),
            Self::B => write!(f, "B"),
            Self::C => write!(f, "C"),
            Self::M => write!(f, "M"),
            Self::SP => write!(f, "SP"),
            Self::BP => write!(f, "BP"),
            Self::PC => write!(f, "PC"),
            Self::FLAGS => write!(f, "FLAGS"),
        }
    }
}

