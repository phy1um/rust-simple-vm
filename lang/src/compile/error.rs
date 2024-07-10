use crate::compile::resolve::{Symbol, Type};

#[derive(Debug)]
pub enum CompilerError {
    LiteralOutOfBounds(u32, u32, u32),
    UnknownSymbol(Symbol),
    VariableAlreadyDefined(String),
    VariableUndefined(String),
    BreakNotInLoop,
    ContinueNotInLoop,
    InlineAsm(String),
    TypeAssign { from: Type, to: Type },
    DerefInvalidType(Type),
}
