use simplevm::{Instruction, Register, Literal12Bit, Literal7Bit, Literal10Bit};
use std::fmt;

use crate::compile::context::Context;
use crate::compile::error::CompilerError;

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol(pub String);

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Symbol {
    pub fn new(s: &str) -> Self {
        Self(s.to_owned())
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum UnresolvedInstruction {
    Instruction(Instruction),
    Imm(Register, Symbol),
    AddImm(Register, Symbol),
    AddImmSigned(Register, Symbol),
    JumpOffset(Symbol),
    Label(Symbol),
}

impl fmt::Display for UnresolvedInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Instruction(i) => write!(f, "{i}"),
            Self::Imm(r, s) => write!(f, "Imm {r} !{s}"),
            Self::AddImm(r, s) => write!(f, "AddImm {r} !{s}"),
            Self::AddImmSigned(r, s) => write!(f, "AddImmSigned {r} !{s}"),
            Self::JumpOffset(s) => write!(f, "JumpOffset !{s}"),
            Self::Label(s) => write!(f, ":{s}"),
        }
    }
}

impl UnresolvedInstruction {
    pub fn resolve(&self, ctx: &Context) -> Result<Option<Instruction>, CompilerError> {
        match self {
            Self::Instruction(i) => Ok(Some(i.clone())),
            Self::Imm(reg, sym) => ctx.get(sym).and_then(|v| Literal12Bit::new_checked(v as u16).map_err(|_| CompilerError::LiteralOutOfBounds(v, 0, 0xfff)).map(|x| Some(Instruction::Imm(reg.clone(), x)))),
            Self::AddImm(reg, sym) => ctx.get(sym).and_then(|v| Literal7Bit::new_checked(v as u8).map_err(|_| CompilerError::LiteralOutOfBounds(v, 0, 0x7f)).map(|x| Some(Instruction::AddImm(reg.clone(), x)))),
            Self::AddImmSigned(reg, sym) => ctx.get(sym).and_then(|v| Literal7Bit::new_checked(v as u8).map_err(|_| CompilerError::LiteralOutOfBounds(v, 0, 0x7f)).map(|x| Some(Instruction::AddImmSigned(reg.clone(), x)))),
            Self::JumpOffset(sym) => ctx.get(sym).and_then(|v| Literal10Bit::new_checked(v as u16).map_err(|_| CompilerError::LiteralOutOfBounds(v, 0, 0x3ff)).map(|x| Some(Instruction::JumpOffset(x)))),
            Self::Label(_) => Ok(None),
        }
    }

    pub fn size(&self) -> u32 {
        match self {
            Self::Label(_) => 0,
            _ => 2,
        }
    }

}


