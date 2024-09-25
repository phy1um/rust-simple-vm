use std::fmt;
use std::str::FromStr;

#[cfg(target_family = "wasm")]
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum Flag {
    Compare = 0b1,
    DidJump = 0b10,
    Overflow = 0b100,
}

#[cfg(not(target_family = "wasm"))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum Register {
    Zero,
    A,
    B,
    C,
    M,
    SP,
    D,
    BP,
}

#[cfg(target_family = "wasm")]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[repr(u8)]
pub enum Register {
    Zero,
    A,
    B,
    C,
    M,
    SP,
    D,
    BP,
}

impl Register {
    pub fn from_u8(v: u8) -> Option<Self> {
        match v {
            x if x == Register::A as u8 => Some(Register::A),
            x if x == Register::B as u8 => Some(Register::B),
            x if x == Register::C as u8 => Some(Register::C),
            x if x == Register::M as u8 => Some(Register::M),
            x if x == Register::SP as u8 => Some(Register::SP),
            x if x == Register::D as u8 => Some(Register::D),
            x if x == Register::BP as u8 => Some(Register::BP),
            x if x == Register::Zero as u8 => Some(Register::Zero),
            _ => None,
        }
    }

    pub fn as_mask_first(&self) -> u16 {
        ((*self as u16) & 0x7) << 12
    }

    pub fn from_instruction_first(ins: u16) -> Option<Self> {
        Self::from_u8(((ins & 0x7000) >> 12) as u8)
    }

    pub fn as_mask_second(&self) -> u16 {
        ((*self as u16) & 0x7) << 9
    }

    pub fn from_instruction_second(ins: u16) -> Option<Self> {
        Self::from_u8(((ins & 0xe00) >> 9) as u8)
    }

    pub fn as_mask_third(&self) -> u16 {
        (*self as u16) & 0x7
    }

    pub fn from_instruction_third(ins: u16) -> Option<Self> {
        Self::from_u8((ins & 0x7) as u8)
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
            Self::D => write!(f, "D"),
            Self::Zero => write!(f, "Zero"),
        }
    }
}

impl FromStr for Register {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "A" => Ok(Register::A),
            "B" => Ok(Register::B),
            "C" => Ok(Register::C),
            "M" => Ok(Register::M),
            "SP" => Ok(Register::SP),
            "BP" => Ok(Register::BP),
            "D" => Ok(Register::D),
            "Zero" => Ok(Register::Zero),
            _ => Err(format!("unknown register {}", s)),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_encoding() {
        assert_eq!(Register::Zero as u16, 0);
        assert_eq!(Register::A as u16, 1);
        assert_eq!(Register::B as u16, 2);
        assert_eq!(Register::C as u16, 3);
        assert_eq!(Register::M as u16, 4);
        assert_eq!(Register::SP as u16, 5);
        assert_eq!(Register::D as u16, 6);
        assert_eq!(Register::BP as u16, 7);
    }
}
