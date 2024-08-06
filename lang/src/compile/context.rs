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
    // (name of function, block)
    pub functions: Vec<(String, Block)>,
    pub function_defs: HashMap<String, FunctionDefinition>,
    pub globals: HashMap<String, Global>,
    pub static_data: Vec<(usize, Vec<u8>)>,
    pub user_types: HashMap<String, Type>,
    pub init: Vec<UnresolvedInstruction>,
    static_head: usize,
    pub global_base: usize,
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
        writeln!(
            f,
            "program section start @ {}",
            self.get_code_section_start()
        )
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
        let offset = self.static_head;
        self.static_head += t.size_bytes();
        self.globals.insert(
            s.to_string(),
            Global {
                address: self.global_base + offset,
                var_type: t,
            },
        );
    }

    pub fn push_static_data(&mut self, data: Vec<u8>) -> u32 {
        let offset = self.static_head;
        self.static_head += data.len();
        if self.static_head % 2 == 1 {
            self.static_head += 1;
        }
        self.static_data.push((offset, data));
        offset as u32
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
        for (_, func) in &self.functions {
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
        for (_, func) in &self.functions {
            for ins in &func.instructions {
                if let Some(c) = ins.resolve(self)? {
                    out.push(c);
                }
            }
        }
        Ok(out)
    }

    pub fn get_static(&self) -> Result<(usize, Vec<u8>), CompilerError> {
        let mut out = vec![0; self.static_head];
        for (offset, data) in &self.static_data {
            for (i, d) in data.iter().enumerate() {
                out[offset + i] = *d;
            }
        }
        Ok((0, out))
    }

    pub fn get_code_section_start(&self) -> u32 {
        self.get_static_section_size()
    }

    fn get_static_section_size(&self) -> u32 {
        let mut max: u32 = 0;
        for (offset, data) in &self.static_data {
            if (offset + data.len()) as u32 > max {
                max = (offset + data.len()) as u32;
            }
        }
        max + (max % 2)
    }

    pub fn get_static_section_start(&self) -> u32 {
        0
    }
}
