use crate::compile::block::Block;
use crate::compile::error::CompilerError;
use crate::compile::resolve::{Symbol, Type, UnresolvedInstruction};
use std::collections::HashMap;

use simplevm::Instruction;

use std::fmt;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub args: Vec<(String, Type)>,
    pub return_type: Type,
}

#[derive(Debug, Clone)]
pub struct Global {
    pub address: usize,
    pub var_type: Type,
}

#[derive(Debug, Default, Clone)]
pub struct Context {
    pub symbols: HashMap<String, u32>,
    pub functions: Vec<Block>,
    pub function_defs: HashMap<String, FunctionDefinition>,
    pub globals: HashMap<String, Global>,
    pub user_types: HashMap<String, Type>,
    pub init: Vec<UnresolvedInstruction>,
    global_head: usize,
    pub global_base: usize,
    pub program_start_offset: u32,
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "context:")?;
        writeln!(
            f,
            " symbols: [{}]",
            self.symbols
                .iter()
                .map(|(k, v)| format!("{k}={v}"))
                .collect::<Vec<_>>()
                .join(", ")
        )?;
        writeln!(f, " function defs: [")?;
        for (func, def) in &self.function_defs {
            writeln!(
                f,
                "  {} {func}({})",
                def.return_type,
                def.args
                    .iter()
                    .map(|(x, _)| x.to_owned())
                    .collect::<Vec<_>>()
                    .join(", ")
            )?;
        }
        writeln!(f, " ]")?;
        writeln!(
            f,
            "globals: [{}]",
            self.globals
                .iter()
                .map(|(k, g)| format!("{k}={:?}", g))
                .collect::<Vec<_>>()
                .join(", ")
        )?;
        writeln!(f, "globals start @ {}", self.global_base)?;
        writeln!(f, "program start @ {}", self.program_start_offset)
    }
}

impl Context {
    pub fn new(global_base: usize) -> Self {
        Self {
            global_base,
            ..Self::default()
        }
    }

    pub fn get(&self, s: &Symbol) -> Result<u32, CompilerError> {
        self.symbols
            .get(&s.0)
            .ok_or(CompilerError::UnknownSymbol(s.clone()))
            .copied()
    }

    pub fn define(&mut self, s: &Symbol, v: u32) {
        self.symbols.insert(s.0.to_owned(), v);
    }

    pub fn define_global(&mut self, s: &str, t: Type) {
        let offset = self.global_head;
        self.global_head += t.size_bytes();
        self.globals.insert(
            s.to_string(),
            Global {
                address: self.global_base + offset,
                var_type: t,
            },
        );
    }

    pub fn define_user_type(&mut self, s: &str, t: Type) {
        self.user_types.insert(s.to_string(), t);
    }

    pub fn get_user_type(&self, s: &str) -> Option<&Type> {
        self.user_types.get(s)
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
