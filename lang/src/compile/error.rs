use crate::compile::resolve::Symbol;

#[derive(Debug)]
pub enum CompilerError {
    LiteralOutOfBounds(u32, u32, u32),
    UnknownSymbol(Symbol),
    VariableAlreadyDefined(String),
    VariableUndefined(String),
    BreakNotInLoop,
    ContinueNotInLoop,
    InlineAsm(String),
}


