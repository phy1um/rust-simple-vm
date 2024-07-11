use std::fmt;
use std::str::FromStr;

#[derive(Debug, PartialEq, Clone)]
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
    Pointer(Box<Type>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::Char => write!(f, "char"),
            Self::Void => write!(f, "void"),
            Self::Pointer(t) => write!(f, "*{t}"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Declare(Identifier, Type, Option<Box<Expression>>),
    Assign(Identifier, Box<Expression>),
    AssignDeref {
        lhs: Expression,
        rhs: Expression,
    },
    Return(Expression),
    Break,
    Continue,
    If {
        cond: Expression,
        body: Vec<Statement>,
        else_body: Option<Vec<Statement>>,
    },
    While {
        cond: Expression,
        body: Vec<Statement>,
    },
    Expression(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Declare(i, t, Some(expr)) => write!(f, "let {t} {i} := {expr};"),
            Self::Declare(i, t, None) => write!(f, "let {t} {i};"),
            Self::Assign(i, expr) => write!(f, "{i} := {expr};"),
            Self::AssignDeref { lhs, rhs } => write!(f, "*{lhs} := {rhs};"),
            Self::Return(expr) => write!(f, "return {expr};"),
            Self::If {
                cond,
                body,
                else_body,
            } => {
                if let Some(e) = else_body {
                    write!(
                        f,
                        "if ({cond}) {{\n{}\n}} else {{\n{}\n}}\n",
                        body.iter()
                            .map(|x| format!("{x}"))
                            .collect::<Vec<_>>()
                            .join("\n"),
                        e.iter()
                            .map(|x| format!("{x}"))
                            .collect::<Vec<_>>()
                            .join("\n")
                    )
                } else {
                    write!(
                        f,
                        "if ({cond}) {{\n{}\n}}\n",
                        body.iter()
                            .map(|x| format!("{x}"))
                            .collect::<Vec<_>>()
                            .join("\n")
                    )
                }
            }
            Self::While { cond, body } => write!(
                f,
                "while ({cond}) {{\n{}\n}}\n",
                body.iter()
                    .map(|x| format!("{x}"))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            Self::Break => write!(f, "break;"),
            Self::Continue => write!(f, "continue;"),
            Self::Expression(e) => write!(f, "{e};"),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Mod,
    Equal,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    NotEqual,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Subtract => write!(f, "-"),
            Self::Multiply => write!(f, "*"),
            Self::Mod => write!(f, "%"),
            Self::Equal => write!(f, "=="),
            Self::GreaterThan => write!(f, ">"),
            Self::GreaterThanEqual => write!(f, ">="),
            Self::LessThan => write!(f, "<"),
            Self::LessThanEqual => write!(f, "<="),
            Self::NotEqual => write!(f, "!="),
        }
    }
}

impl FromStr for BinOp {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(Self::Add),
            "-" => Ok(Self::Subtract),
            "*" => Ok(Self::Multiply),
            "%" => Ok(Self::Mod),
            "==" => Ok(Self::Equal),
            ">" => Ok(Self::GreaterThan),
            ">=" => Ok(Self::GreaterThanEqual),
            "<" => Ok(Self::LessThan),
            "<=" => Ok(Self::LessThanEqual),
            "!=" => Ok(Self::NotEqual),
            _ => Err(format!("unknown binop {s}")),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    LiteralInt(i32),
    LiteralChar(char),
    Variable(String),
    AddressOf(Identifier),
    Deref(Box<Expression>),
    BinOp(Box<Expression>, Box<Expression>, BinOp),
    FunctionCall(Identifier, Vec<Expression>),
    Bracketed(Box<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::LiteralInt(i) => write!(f, "{i}"),
            Self::LiteralChar(c) => write!(f, "'{c}'"),
            Self::Variable(v) => write!(f, "{v}"),
            Self::AddressOf(i) => write!(f, "&{i}"),
            Self::Deref(i) => write!(f, "*{i}"),
            Self::BinOp(e0, e1, op) => write!(f, "{e0} {op} {e1}"),
            Self::Bracketed(e) => write!(f, "({e})"),
            Self::FunctionCall(name, args) => write!(
                f,
                "{name}({})",
                args.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TopLevel {
    FunctionDefinition {
        name: Identifier,
        return_type: Type,
        args: Vec<(Identifier, Type)>,
        body: Vec<Statement>,
    },
    InlineAsm {
        name: Identifier,
        args: Vec<(Identifier, Type)>,
        body: String,
    },
    GlobalVariable {
        name: Identifier,
        var_type: Type,
    },
}

impl fmt::Display for TopLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FunctionDefinition {
                name,
                return_type,
                args,
                body,
            } => {
                let arglist = args
                    .iter()
                    .map(|(id, arg_type)| format!("{arg_type} {id}"))
                    .collect::<Vec<String>>()
                    .join(", ");
                let body = body
                    .iter()
                    .map(|s| format!("{s}"))
                    .collect::<Vec<String>>()
                    .join("\n");
                writeln!(f, "{return_type} {name}({}) {{\n{}\n}}", arglist, body)
            }
            Self::InlineAsm { name, args, body } => {
                let arglist = args
                    .iter()
                    .map(|(id, arg_type)| format!("{arg_type} {id}"))
                    .collect::<Vec<String>>()
                    .join(", ");
                writeln!(f, "asm! {name}({}) {{{body}}}", arglist)
            }
            Self::GlobalVariable { name, var_type } => writeln!(f, "global {var_type} {name};"),
        }
    }
}
