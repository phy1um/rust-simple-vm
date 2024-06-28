use macros::StringyEnum;
use std::fmt;
use std::str::FromStr;

enum CombinedResult<A, B> {
    Left(A),
    Right(B),
}

impl<A, B> CombinedResult<A, B> {
    fn from<E>(left: Result<A, E>, right: Result<B, E>) -> Result<CombinedResult<A, B>, E> {
        match left {
            Ok(a) => Ok(CombinedResult::Left(a)),
            Err(_) => match right {
                Ok(b) => Ok(CombinedResult::Right(b)),
                Err(e) => Err(e),
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Literal7Bit {
    pub value: u8,
}

impl Literal7Bit {
    pub fn new_checked(value: u8) -> Result<Self, String> {
        if value >= 0x80 {
            Err(format!("out of range [0x0, 0x7F]: {:X}", value))
        } else {
            Ok(Self { value })
        }
    }

    pub fn from_str_radix(s: &str, radix: u32) -> Result<Self, String> {
        let res = CombinedResult::from(u8::from_str_radix(s, radix), i8::from_str_radix(s, radix))
            .map_err(|_| format!("invalid number {}", s))?;
        match res {
            CombinedResult::Left(a) => Self::new_checked(a),
            CombinedResult::Right(b) => Self::from_signed(b),
        }
    }

    pub fn from_signed(value: i8) -> Result<Self, String> {
        if value >= 0 {
            Self::new_checked(value.unsigned_abs())
        } else {
            let v: u8 = value.unsigned_abs();
            let inv = !(v & 0x7f);
            Self::new_checked((inv + 1) & 0x7f)
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Literal10Bit {
    pub value: u16,
}

impl Literal10Bit {
    pub fn new_checked(value: u16) -> Result<Self, String> {
        if value > 0x3ff {
            Err(format!("out of range [0x0, 0x3ff]: {:X}", value))
        } else {
            Ok(Self { value })
        }
    }

    pub fn from_str_radix(s: &str, radix: u32) -> Result<Self, String> {
        let res =
            CombinedResult::from(u16::from_str_radix(s, radix), i16::from_str_radix(s, radix))
                .map_err(|_| format!("invalid number {}", s))?;
        match res {
            CombinedResult::Left(a) => Self::new_checked(a),
            CombinedResult::Right(b) => Self::from_signed(b),
        }
    }

    pub fn from_signed(value: i16) -> Result<Self, String> {
        if value >= 0 {
            Self::new_checked(value.unsigned_abs())
        } else {
            let v: u16 = value.unsigned_abs();
            let inv = !(v & 0x3ff);
            Self::new_checked((inv + 1) & 0x3ff)
        }
    }

    pub fn as_signed(&self) -> i16 {
        let sgn = (self.value & 0x200) >> 9;
        if sgn == 0 {
            (self.value & 0x3ff) as i16
        } else {
            unsafe { std::mem::transmute(self.value | 0xfc00) }
        }
    }
}

impl fmt::Display for Literal10Bit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Literal12Bit {
    pub value: u16,
}

impl Literal12Bit {
    pub fn new_checked(value: u16) -> Result<Self, String> {
        if value > 0xfff {
            Err(format!("out of range [0x0, 0xfff]: {:X}", value))
        } else {
            Ok(Self { value })
        }
    }

    pub fn from_str_radix(s: &str, radix: u32) -> Result<Self, String> {
        let res =
            CombinedResult::from(u16::from_str_radix(s, radix), i16::from_str_radix(s, radix))
                .map_err(|_| format!("invalid number {}", s))?;
        match res {
            CombinedResult::Left(a) => Self::new_checked(a),
            CombinedResult::Right(b) => Self::from_signed(b),
        }
    }

    pub fn from_signed(value: i16) -> Result<Self, String> {
        if value >= 0 {
            Self::new_checked(value.unsigned_abs())
        } else {
            let v: u16 = value.unsigned_abs();
            let inv = !(v & 0xfff);
            Self::new_checked((inv + 1) & 0xfff)
        }
    }

    pub fn as_signed(&self) -> i16 {
        let sgn = (self.value & 0x800) >> 11;
        if sgn == 0 {
            (self.value & 0xfff) as i16
        } else {
            unsafe { std::mem::transmute(self.value | 0xf000) }
        }
    }
}

impl fmt::Display for Literal12Bit {
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

#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct Nibble {
    pub value: u8,
}

impl Nibble {
    pub fn new_checked(value: u8) -> Result<Self, String> {
        if value > 0xf {
            Err(format!("out of range [0x0, 0xF]: {:X}", value))
        } else {
            Ok(Self { value })
        }
    }

    pub fn from_str_radix(s: &str, radix: u32) -> Result<Self, String> {
        let res = CombinedResult::from(u8::from_str_radix(s, radix), i8::from_str_radix(s, radix))
            .map_err(|_| format!("invalid number {}", s))?;
        match res {
            CombinedResult::Left(a) => Self::new_checked(a),
            CombinedResult::Right(b) => Self::from_signed(b),
        }
    }

    pub fn from_signed(value: i8) -> Result<Self, String> {
        if value >= 0 {
            Self::new_checked(value.unsigned_abs())
        } else {
            let v: u8 = value.unsigned_abs();
            let inv = !(v & 0xf);
            Self::new_checked((inv + 1) & 0xf)
        }
    }

    pub fn as_signed(&self) -> i8 {
        let sgn = (self.value & 0x8) >> 3;
        if sgn == 0 {
            (self.value & 0xf) as i8
        } else {
            unsafe { std::mem::transmute(self.value | 0xf0) }
        }
    }
}

impl fmt::Display for Nibble {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_literal_7b() -> Result<(), String> {
        assert!(Literal7Bit::from_signed(-1)?.value == 0b111_1111);
        assert!(Literal7Bit::from_signed(-5)?.value == 0b111_1011);
        assert!(Literal7Bit::from_signed(-30)?.value == 0b110_0010);
        assert!(Literal7Bit::from_signed(-10)?.as_signed() == -10);
        assert!(Literal7Bit::from_signed(-30)?.as_signed() == -30);
        assert!(Literal7Bit::from_signed(-29)?.as_signed() == -29);
        Ok(())
    }
}
