use std::collections::HashMap;
use crate::ast;
use crate::compile::resolve::{Symbol, UnresolvedInstruction};
use crate::compile::error::CompilerError;
use crate::compile::block::Block;

use simplevm::Instruction;

#[allow(dead_code)]
#[derive(Debug)]
pub struct FunctionDefinition {
    pub args: Vec<(String, ast::Type)>,
    pub return_type: ast::Type,
}

#[derive(Debug, Default)]
pub struct Context {
    pub symbols: HashMap<String, u32>,
    pub functions: Vec<Block>,
    pub function_defs: HashMap<String, FunctionDefinition>,
    pub init: Vec<UnresolvedInstruction>,
}

impl Context {
    pub fn get(&self, s: &Symbol) -> Result<u32, CompilerError> {
        self.symbols.get(&s.0).ok_or(CompilerError::UnknownSymbol(s.clone())).copied()
    }

    pub fn define(&mut self, s: &Symbol, v: u32) {
        self.symbols.insert(s.0.to_owned(), v);
    }

    pub fn load_init(&mut self, prog: Vec<UnresolvedInstruction>) {
        self.init = prog;
    }

    pub fn get_lines_unresolved(&self) -> Result<Vec<String>, CompilerError> {
        let mut out = Vec::new();
        for ins in &self.init {
            out.push(ins.to_string());
        }
        for func in &self.functions {
            for ins in &func.instructions {
                out.push(ins.to_string());
            }
        }
        Ok(out)
    }

    pub fn get_instructions(&self) -> Result<Vec<Instruction>, CompilerError> {
        let mut out = Vec::new();
        for ins in &self.init {
            if let Some(c) = ins.resolve(self)? {
                out.push(c);
            }
        }
        for func in &self.functions {
            for ins in &func.instructions {
                 if let Some(c) = ins.resolve(self)? {
                    out.push(c);
                }
            }
        }
        Ok(out)
    }
}


