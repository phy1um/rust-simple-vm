use std::collections::HashMap;
use std::fmt;

use crate::ast;
use crate::compile::block::{BlockScope, BlockVariable};
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

type StructField = (Type, usize);

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Char,
    Void,
    Pointer(Box<Type>),
    Struct(HashMap<String, StructField>),
    UncheckedInt,
}

impl Type {
    fn max(&self, other: &Self) -> Self {
        // bias LHS
        if self.size_bytes() >= other.size_bytes() {
            self.clone()
        } else {
            other.clone()
        }
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, Self::Pointer(_))
    }

    pub fn is_numeric(&self) -> bool {
        matches!(self, Self::Int | Self::Char | Self::UncheckedInt)
    }

    pub fn is_struct(&self) -> bool {
        matches!(self, Self::Struct(_))
    }

    pub fn size_bytes(&self) -> usize {
        match self {
            Self::Int => 2,
            Self::Char => 1,
            Self::Void => 0,
            // TODO: long pointer?
            Self::Pointer(_) => 2,
            Self::UncheckedInt => 2,
            Self::Struct(fields) => fields.values().map(|(ty, _offset)| ty.size_bytes()).sum(),
        }
    }

    pub fn can_assign_from(&self, other: &Self) -> bool {
        // TODO: struct value assignment
        !self.is_struct()
            && !other.is_struct()
            && !matches!(other, Self::Void)
            && self.size_bytes() >= other.size_bytes()
    }

    pub fn from_ast(ctx: &Context, value: &ast::Type) -> Result<Self, CompilerError> {
        match value {
            ast::Type::Int => Ok(Self::Int),
            ast::Type::Char => Ok(Self::Char),
            ast::Type::Void => Ok(Self::Void),
            ast::Type::Pointer(t) => Ok(Self::Pointer(Box::new(Self::from_ast(ctx, t)?))),
            ast::Type::Struct(fields) => Ok(Self::Struct({
                let mut offset = 0;
                let mut out = HashMap::new();
                for (name, field_type) in fields.iter() {
                    let tt = Self::from_ast(ctx, field_type)?;
                    let no = offset;
                    offset += tt.size_bytes();
                    out.insert(name.to_string(), (tt, no));
                    if offset % 2 != 0 {
                        offset += 1;
                    }
                }
                out
            })),
            ast::Type::User(s) => ctx
                .get_user_type(s)
                .cloned()
                .ok_or(CompilerError::UnknownType(s.to_owned())),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::Char => write!(f, "char"),
            Self::Void => write!(f, "void"),
            Self::Pointer(t) => write!(f, "*{t}"),
            Self::UncheckedInt => write!(f, "int"),
            Self::Struct(fields) => write!(
                f,
                "struct {{\n{}\n}};",
                fields
                    .iter()
                    .map(|(name, (ty, _offset))| format!("{ty} {name}"))
                    .collect::<Vec<_>>()
                    .join(",\n")
            ),
        }
    }
}

// TODO: maybe this should return an error
pub fn type_of(ctx: &Context, scope: &BlockScope, expr: &ast::Expression) -> Type {
    match expr {
        ast::Expression::LiteralInt(_) => Type::Int,
        ast::Expression::LiteralChar(_) => Type::Char,
        ast::Expression::LiteralString(_) => Type::Pointer(Box::new(Type::Void)),
        ast::Expression::BuiltinSizeof(_) => Type::Int,
        ast::Expression::AddressOf(fields) => {
            Type::Pointer(Box::new(get_fields_type(ctx, scope, fields)))
        }
        ast::Expression::ArrayDeref { lhs, index: _ } => {
            if let Type::Pointer(t) = type_of(ctx, scope, lhs.as_ref()) {
                *t.clone()
            } else {
                Type::Void
            }
        }
        ast::Expression::Deref(expr) => {
            let inner_type = type_of(ctx, scope, expr);
            if let Type::Pointer(t) = inner_type {
                *t.clone()
            } else {
                Type::Void
            }
        }
        ast::Expression::Bracketed(expr) => type_of(ctx, scope, expr),
        ast::Expression::FunctionCall(name, _) => {
            if let Some(def) = ctx.function_defs.get(&name.0) {
                def.return_type.clone()
            } else {
                Type::Void
            }
        }
        ast::Expression::BinOp(a, b, op) => {
            let type_a = type_of(ctx, scope, a);
            let type_b = type_of(ctx, scope, b);
            match op {
                ast::BinOp::Add | ast::BinOp::Subtract | ast::BinOp::Multiply => {
                    if let Type::Pointer(_) = type_a {
                        type_a
                    } else {
                        type_a.max(&type_b)
                    }
                }
                ast::BinOp::Mod => type_a,
                ast::BinOp::Equal
                | ast::BinOp::NotEqual
                | ast::BinOp::GreaterThan
                | ast::BinOp::GreaterThanEqual
                | ast::BinOp::LessThan
                | ast::BinOp::LessThanEqual => Type::Int,
            }
        }
        ast::Expression::Variable(fields) => get_fields_type(ctx, scope, fields),
    }
}

fn variable_type(ctx: &Context, scope: &BlockScope, name: &str) -> Type {
    if let Some(bv) = scope.get(ctx, name) {
        match bv {
            BlockVariable::Local(_, t) => t,
            BlockVariable::Arg(_, t) => t,
            BlockVariable::Const(_) => Type::Int,
            BlockVariable::Global(_, t) => t,
        }
    } else if ctx.symbols.contains_key(name) {
        Type::Int
    } else {
        // undefined variables become void to maximize error info?
        // alternate: cast to unchecked ints which cast to anything
        Type::Void
    }
}

fn get_fields_type(ctx: &Context, scope: &BlockScope, fields: &[ast::Identifier]) -> Type {
    let mut head_type = variable_type(ctx, scope, &fields.first().unwrap().0);
    for field in &fields[1..] {
        let struct_fields = {
            if let Type::Struct(fs) = head_type {
                fs
            } else if let Type::Pointer(t) = head_type {
                if let Type::Struct(fs) = *t {
                    fs
                } else {
                    HashMap::new()
                }
            } else {
                HashMap::new()
            }
        };
        head_type = struct_fields
            .get(&field.0)
            .map(|(t, _)| t.clone())
            .unwrap_or(Type::Void);
    }
    head_type
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn can_assign() {
        assert!(Type::Int.can_assign_from(&Type::Int));
    }
}
