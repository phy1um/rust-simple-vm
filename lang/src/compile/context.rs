use crate::compile::block::Block;
use crate::compile::error::CompilerError;
use crate::compile::resolve::{Symbol, Type};
use std::collections::HashMap;

use simplevm::{
    binfmt::{BinaryFile, Section, SectionMode},
    resolve::UnresolvedInstruction,
    Instruction,
};

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
        self.static_data.push((offset, vec![0, 0]));
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
        let block = Block {
            instructions: prog,
            local_count: 0,
            local_offset: 0,
            args: Vec::new(),
            offset: 1,
        };
        self.functions.push(("_init".to_string(), block));
    }

    pub fn get_lines_unresolved(&self) -> Result<Vec<String>, CompilerError> {
        let mut out = Vec::new();
        for (_, func) in &self.functions {
            for ins in &func.instructions {
                out.push(ins.to_string());
            }
        }
        Ok(out)
    }

    pub fn get_instructions(&self) -> Result<Vec<(u32, Vec<Instruction>)>, CompilerError> {
        let mut out = Vec::new();
        for (_, func) in &self.functions {
            let mut function_instructions = Vec::new();
            let mut ins_offset = 0;
            for ins in func.instructions.iter() {
                if let Some(c) = ins
                    .resolve(ins_offset + func.offset, &self.symbols)
                    .map_err(|e| CompilerError::InstructionResolve(format!("{e:?}")))?
                {
                    function_instructions.push(c);
                    ins_offset += 2;
                }
            }
            out.push((func.offset, function_instructions));
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

    pub fn get_entrypoint(&self) -> Option<u32> {
        self.symbols.get("_init").copied()
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

    pub fn to_binary(&self) -> Result<BinaryFile, String> {
        let (static_offset, static_data) = self.get_static().unwrap();
        let functions_and_offsets = self
            .get_instructions()
            .map_err(|x| format!("resolving: {x:?}"))?;
        let mut max_offset_func_size = 0;
        let mut max_offset: Option<u32> = None;
        for (offset, instructions) in &functions_and_offsets {
            if let Some(o) = max_offset {
                if *offset > o {
                    max_offset = Some(*offset);
                    max_offset_func_size = instructions.len() * 2;
                    // TODO: replace with logging
                    // println!("new max {max_offset:?}");
                }
            } else {
                max_offset = Some(*offset);
                max_offset_func_size = instructions.len() * 2;
            }
        }
        let code_size = max_offset.unwrap() as usize + max_offset_func_size;
        // println!("pre-allocating code space for {code_size} bytes");
        let mut code_bytes: Vec<u8> = vec![0; code_size];
        for (offset, instructions) in functions_and_offsets {
            // println!("writing function @ {offset}");
            let local_offset = (offset - self.get_code_section_start()) as usize;
            for (index, ins) in instructions.iter().enumerate() {
                // println!(" - {index} ({}) = {ins}", local_offset + index*2);
                let raw_instruction = ins.encode_u16().to_le_bytes();
                code_bytes[local_offset + (index * 2)] = raw_instruction[0];
                code_bytes[local_offset + (index * 2) + 1] = raw_instruction[1];
            }
        }
        let mut bin = BinaryFile::default();
        let entrypoint = self
            .get_entrypoint()
            .ok_or("no function _init".to_string())? as u16;
        bin.entrypoint = entrypoint;
        bin.version = 1;
        bin.sections.push(Section {
            size: code_bytes.len() as u16,
            mode: SectionMode::RO,
            address: self.get_code_section_start(),
            file_offset: 1,
        });
        bin.sections.push(Section {
            size: 0x8000,
            mode: SectionMode::Heap,
            address: self.get_code_section_start() + code_bytes.len() as u32,
            ..Section::default()
        });
        if !static_data.is_empty() {
            bin.sections.push(Section {
                size: static_data.len() as u16,
                mode: SectionMode::RW,
                address: static_offset as u32,
                file_offset: 1,
            });
        }
        let header_size = bin.get_header_size();
        bin.sections.get_mut(0).unwrap().file_offset = header_size as u32;
        let static_data_start = header_size + code_bytes.len();
        // println!("bin data extend: code_bytes @ {}", bin.data.len());
        bin.data.extend(code_bytes);
        if !static_data.is_empty() {
            bin.sections.get_mut(2).unwrap().file_offset = static_data_start as u32;
            bin.data.extend(static_data);
        }
        Ok(bin)
    }
}
