use std::fmt;
use std::str::FromStr;

#[derive(Debug, PartialEq)]
pub struct Identifier(pub String);

impl Identifier {
    pub fn new(s: &str) -> Self {
        Self(s.to_owned())
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
    Char,
    Void,
}

impl FromStr for Type {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "int" => Ok(Self::Int),
            "char" => Ok(Self::Char),
            "void" => Ok(Self::Void),
            x => Err(format!("unknown type {x}"))
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::Char => write!(f, "char"),
            Self::Void => write!(f, "void"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Declare(Identifier, Type, Option<Box<Expression>>),
    Assign(Identifier, Box<Expression>),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Declare(i, t, Some(expr)) => write!(f, "let {t} {i} := {expr}"),
            Self::Declare(i, t, None) => write!(f, "let {t} {i}"),
            Self::Assign(i, expr) => write!(f, "{i} := {expr}"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    LiteralInt(i32),
    LiteralChar(char),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::LiteralInt(i) => write!(f, "{i}"),
            Self::LiteralChar(c) => write!(f, "{c}"),
        }
    }
}
