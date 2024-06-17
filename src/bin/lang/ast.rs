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

#[derive(Debug, PartialEq, Clone)]
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
    Return(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Declare(i, t, Some(expr)) => write!(f, "let {t} {i} := {expr}"),
            Self::Declare(i, t, None) => write!(f, "let {t} {i}"),
            Self::Assign(i, expr) => write!(f, "{i} := {expr}"),
            Self::Return(expr) => write!(f, "return {expr}"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    LiteralInt(i32),
    LiteralChar(char),
    Variable(String),
    FunctionCall(Identifier, Vec<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::LiteralInt(i) => write!(f, "{i}"),
            Self::LiteralChar(c) => write!(f, "'{c}'"),
            Self::Variable(v) => write!(f, "{v}"),
            Self::FunctionCall(name, args) => write!(f, "{name}({})", args.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ")),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TopLevel {
    FunctionDefinition{name: Identifier, return_type: Type, args: Vec<(Identifier, Type)>, body: Vec<Statement>},
}

impl fmt::Display for TopLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FunctionDefinition{name, return_type, args, body} => {
                let arglist = args.iter().map(|(id, arg_type)| format!("{arg_type} {id}")).collect::<Vec<String>>().join(", ");
                let body = body.iter().map(|s| format!("{s};")).collect::<Vec<String>>().join("\n");
                write!(f, "{return_type} {name}({}) {{\n{}\n}}\n", arglist, body)
            }
        }
    }
}

