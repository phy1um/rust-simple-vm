use crate::register::Register;
use macros::{StringyEnum, VmInstruction};
use std::fmt;
use std::str::FromStr;

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

pub enum InstructionParseError {
    NoContent,
    Fail(String),
}

pub trait InstructionPart {
    fn as_mask(&self) -> u16;
    fn from_instruction(ins: u16) -> Self;
}

#[derive(Debug, PartialEq, Eq)]
pub struct Literal7Bit {
    pub value: u8,
}

impl Literal7Bit {
    pub fn new(value: u8) -> Self {
        Self { value }
    }

    pub fn from_signed(value: i8) -> Self {
        if value >= 0 {
            Self::new(value.unsigned_abs())
        } else {
            let v: u8 = value.unsigned_abs();
            let inv = !(v & 0x7f);
            Self::new((inv + 1) & 0x7f)
        }
    }

    pub fn as_signed(&self) -> i8 {
        let sgn = (self.value & 0x40) >> 6;
        if sgn == 0 {
            (self.value & 0x7f) as i8
        } else {
            unsafe { std::mem::transmute(self.value | 0x80) }
        }
    }
}

impl fmt::Display for Literal7Bit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Literal10Bit {
    pub value: u16,
}

impl Literal10Bit {
    pub fn new(value: u16) -> Self {
        Self { value }
    }
}

impl fmt::Display for Literal10Bit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Clone, Copy, StringyEnum)]
pub enum TestOp {
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    BothZero,
    EitherNonZero,
    BothNonZero,
}

impl TryFrom<u16> for TestOp {
    type Error = String;
    fn try_from(value: u16) -> Result<Self, Self::Error> {
        match value {
            x if x == TestOp::Eq as u16 => Ok(TestOp::Eq),
            x if x == TestOp::Neq as u16 => Ok(TestOp::Neq),
            x if x == TestOp::Lt as u16 => Ok(TestOp::Lt),
            x if x == TestOp::Lte as u16 => Ok(TestOp::Lte),
            x if x == TestOp::Gt as u16 => Ok(TestOp::Gt),
            x if x == TestOp::Gte as u16 => Ok(TestOp::Gte),
            x if x == TestOp::BothZero as u16 => Ok(TestOp::BothZero),
            x if x == TestOp::BothNonZero as u16 => Ok(TestOp::BothNonZero),
            x if x == TestOp::EitherNonZero as u16 => Ok(TestOp::EitherNonZero),
            _ => Err(format!("unknown test op value {}", value)),
        }
    }
}

#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Clone, Copy, StringyEnum)]
pub enum StackOp {
    Pop,
    Push,
    Peek,
    Swap,
    Dup,
    Rotate,
    Add,
    Sub,
}

impl TryFrom<u16> for StackOp {
    type Error = String;
    fn try_from(value: u16) -> Result<Self, Self::Error> {
        match value {
            x if x == StackOp::Pop as u16 => Ok(StackOp::Pop),
            x if x == StackOp::Push as u16 => Ok(StackOp::Push),
            x if x == StackOp::Peek as u16 => Ok(StackOp::Peek),
            x if x == StackOp::Swap as u16 => Ok(StackOp::Swap),
            x if x == StackOp::Dup as u16 => Ok(StackOp::Dup),
            x if x == StackOp::Rotate as u16 => Ok(StackOp::Rotate),
            x if x == StackOp::Add as u16 => Ok(StackOp::Add),
            x if x == StackOp::Sub as u16 => Ok(StackOp::Sub),
            _ => Err(format!("unknown stack op value {}", value)),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Nibble {
    pub value: u8,
}

impl Nibble {
    pub fn new(value: u8) -> Self {
        Self { value }
    }
}

impl fmt::Display for Nibble {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, VmInstruction, PartialEq, Eq)]
pub enum Instruction {
    #[opcode(0xff)]
    // TODO: make u16 more restrictive
    Imm(Register, u16), // Imm has unique instruction format, it doesn't use an opcode.
    #[opcode(0x0)]
    Invalid(u16),
    #[opcode(0x1)]
    Add(Register, Register, Register),
    #[opcode(0x2)]
    Sub(Register, Register, Register),
    #[opcode(0x3)]
    AddImm(Register, Literal7Bit),
    #[opcode(0x4)]
    AddImmSigned(Register, Literal7Bit),
    #[opcode(0x5)]
    ShiftLeft(Register, Register, Nibble),
    #[opcode(0x6)]
    ShiftRightLogical(Register, Register, Nibble),
    #[opcode(0x7)]
    ShiftRightArithmetic(Register, Register, Nibble),
    // TODO: and, or, xor, not
    #[opcode(0x8)]
    Load(Register, Register, Register), // R0 = RAM[R1 | (R2<<16)]
    #[opcode(0x9)]
    Store(Register, Register, Register), // RAM[R1 | (R2<<16)] = R0
    #[opcode(0xa)]
    JumpOffset(Literal10Bit),
    #[opcode(0x10)]
    SetAndSave(Register, Register, Register), // R2 = R0, R0 = R1
    #[opcode(0x11)]
    AddAndSave(Register, Register, Register), // R2 = R0, R0 = R0+R1
    #[opcode(0xb)]
    Test(Register, Register, TestOp),
    #[opcode(0xc)]
    AddIf(Register, Nibble),
    #[opcode(0xd)]
    Stack(Register, Register, StackOp),
    #[opcode(0xe)]
    LoadStackOffset(Register, Register, Nibble),
    #[opcode(0xf)]
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
            Imm(M, 0x30),
            AddImm(C, Literal7Bit::new(0x20)),
            AddImmSigned(A, Literal7Bit::new(0x7)),
            Add(C, B, A),
            Sub(PC, BP, SP),
            ShiftLeft(M, BP, Nibble::new(0xe)),
            ShiftRightLogical(M, BP, Nibble::new(0xe)),
            ShiftRightArithmetic(M, BP, Nibble::new(0xe)),
            Load(A, C, M),
            Store(C, A, M),
            JumpOffset(Literal10Bit::new(1000)),
            SetAndSave(A, B, C),
            AddAndSave(PC, B, C),
            Test(BP, A, TestOp::Gte),
            AddIf(PC, Nibble::new(0x0)),
            Stack(B, SP, StackOp::Dup),
            LoadStackOffset(A, BP, Nibble::new(0x3)),
            System(A, B, Nibble::new(0x3)),
        ];
        let encoded: Vec<_> = ops.iter().map(|x| x.encode_u16()).collect();
        for (l, r) in ops.iter().zip(encoded.iter()) {
            assert_eq!(*l, Instruction::try_from(*r)?);
        }
        Ok(())
    }

    #[test]
    fn test_literal_7b() {
        assert!(Literal7Bit::from_signed(-1).value == 0b111_1111);
        assert!(Literal7Bit::from_signed(-5).value == 0b111_1011);
        assert!(Literal7Bit::from_signed(-30).value == 0b110_0010);
        assert!(Literal7Bit::from_signed(-10).as_signed() == -10);
        assert!(Literal7Bit::from_signed(-30).as_signed() == -30);
        assert!(Literal7Bit::from_signed(-29).as_signed() == -29);
    }
}
