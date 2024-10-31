use crate::{Instruction, Literal10Bit, Literal12Bit, Literal7Bit, Register};
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone)]
pub enum ResolveError {
    LiteralOutOfBounds { value: u32, min: u32, max: u32 },
    UnalignedJump(String, u32),
    UnknownSymbol(String),
}

#[derive(Debug, Clone)]
pub enum UnresolvedInstruction {
    Instruction(Instruction),
    Imm(Register, String),
    AddImm(Register, String),
    AddImmSigned(Register, String),
    Jump(String),
    Branch(String),
    Label(String),
}

impl fmt::Display for UnresolvedInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Instruction(i) => write!(f, "{i}"),
            Self::Imm(r, s) => write!(f, "Imm {r} !{s}"),
            Self::AddImm(r, s) => write!(f, "AddImm {r} !{s}"),
            Self::AddImmSigned(r, s) => write!(f, "AddImmSigned {r} !{s}"),
            Self::Jump(s) => write!(f, "Jump !{s}"),
            Self::Branch(s) => write!(f, "Branch !{s}"),
            Self::Label(s) => write!(f, ":{s}"),
        }
    }
}

impl UnresolvedInstruction {
    pub fn resolve(
        &self,
        compiled_offset: u32,
        symbols: &HashMap<String, u32>,
    ) -> Result<Option<Instruction>, ResolveError> {
        match self {
            Self::Instruction(i) => Ok(Some(i.clone())),
            Self::Imm(reg, sym) => symbols
                .get(sym)
                .ok_or(ResolveError::UnknownSymbol(sym.to_string()))
                .and_then(|v| {
                    Literal12Bit::new_checked(*v as u16)
                        .map_err(|_| ResolveError::LiteralOutOfBounds {
                            value: *v,
                            min: 0,
                            max: 0xfff,
                        })
                        .map(|x| Some(Instruction::Imm(*reg, x)))
                }),
            Self::AddImm(reg, sym) => symbols
                .get(sym)
                .ok_or(ResolveError::UnknownSymbol(sym.to_string()))
                .and_then(|v| {
                    Literal7Bit::new_checked(*v as u8)
                        .map_err(|_| ResolveError::LiteralOutOfBounds {
                            value: *v,
                            min: 0,
                            max: 0x7f,
                        })
                        .map(|x| Some(Instruction::AddImm(*reg, x)))
                }),
            Self::AddImmSigned(reg, sym) => symbols
                .get(sym)
                .ok_or(ResolveError::UnknownSymbol(sym.to_string()))
                .and_then(|v| {
                    Literal7Bit::new_checked(*v as u8)
                        .map_err(|_| ResolveError::LiteralOutOfBounds {
                            value: *v,
                            min: 0,
                            max: 0x7f,
                        })
                        .map(|x| Some(Instruction::AddImmSigned(*reg, x)))
                }),
            Self::Jump(sym) => symbols
                .get(sym)
                .ok_or(ResolveError::UnknownSymbol(sym.to_string()))
                .and_then(|v| {
                    if *v % 16 != 0 {
                        Err(ResolveError::UnalignedJump(sym.clone(), *v))
                    } else {
                        let shifted = *v >> 4;
                        Literal10Bit::new_checked(shifted as u16)
                            .map_err(|_| ResolveError::LiteralOutOfBounds {
                                value: shifted,
                                min: 0,
                                max: 0x3ff,
                            })
                            .map(|x| Some(Instruction::Jump(x)))
                    }
                }),
            Self::Branch(sym) => symbols
                .get(sym)
                .ok_or(ResolveError::UnknownSymbol(sym.to_string()))
                .and_then(|v| {
                    let offset = (*v as i32) - (compiled_offset as i32);
                    Literal10Bit::from_signed(offset as i16)
                        .map_err(|_| ResolveError::LiteralOutOfBounds {
                            value: *v,
                            min: 0,
                            max: 0x3ff,
                        })
                        .map(|x| Some(Instruction::Branch(x)))
                }),
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
