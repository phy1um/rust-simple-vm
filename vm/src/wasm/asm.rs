use crate::binfmt::{BinaryFile, SectionMode};
use crate::*;
use wasm_bindgen::prelude::*;

use std::str::FromStr;

#[wasm_bindgen]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SymbolKind {
    Variable,
    Label,
}

#[wasm_bindgen]
#[derive(Debug, Clone)]
pub struct ResolvedSymbol {
    name: String,
    kind: SymbolKind,
    value: String,
}

#[wasm_bindgen]
impl ResolvedSymbol {
    #[wasm_bindgen(js_name = getName)]
    pub fn get_name(&self) -> String {
        self.name.to_owned()
    }

    #[wasm_bindgen(js_name = getValue)]
    pub fn get_value(&self) -> String {
        self.value.to_owned()
    }

    #[wasm_bindgen(js_name = isLabel)]
    pub fn is_label(&self) -> bool {
        self.kind == SymbolKind::Label
    }
}

#[wasm_bindgen]
#[derive(Debug, Default)]
struct PreProcessor {
    pp: pp::PreProcessor,
    resolved_lines: Option<Vec<String>>,
    resolved_symbols: Option<Vec<ResolvedSymbol>>,
}

#[wasm_bindgen(js_class= PreProcessor)]
impl PreProcessor {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        let mut pp = pp::PreProcessor::default();
        pp::macros::setup_std_macros(&mut pp);
        Self {
            pp,
            ..Self::default()
        }
    }

    #[wasm_bindgen]
    pub fn compile(&mut self, input: &str) -> Result<Vec<u8>, String> {
        let mut out = Vec::new();
        self.pp
            .handle(input)
            .map_err(|e| format!("failed to handle: {e}"))?;
        let bin: BinaryFile = self
            .pp
            .clone()
            .try_into()
            .map_err(|e| format!("build binary: {e}"))?;
        bin.to_bytes(&mut out);
        Ok(out)
    }

    #[wasm_bindgen]
    pub fn clear(&mut self) {
        let mut pp = pp::PreProcessor::default();
        pp::macros::setup_std_macros(&mut pp);
        self.pp = pp;
        self.resolved_lines = None;
        self.resolved_symbols = None;
    }

    #[wasm_bindgen]
    pub fn get_lines(&self) -> Result<Vec<String>, String> {
        self.resolved_lines
            .clone()
            .ok_or("no resolved program".to_string())
    }

    #[wasm_bindgen]
    pub fn get_symbols(&self) -> Result<Vec<ResolvedSymbol>, String> {
        Ok(self
            .pp
            .labels
            .iter()
            .map(|(name, value)| ResolvedSymbol {
                name: name.to_string(),
                value: format!("{value}"),
                kind: SymbolKind::Label,
            })
            .collect::<Vec<_>>())
    }

    #[wasm_bindgen(js_name = createSection)]
    pub fn create_section(&mut self, name: &str, offset: u32, kind: &str) -> Result<(), String> {
        let mode = SectionMode::from_str(kind)?;
        self.pp.create_section(name, offset, mode);
        Ok(())
    }

    #[wasm_bindgen(js_name = setActiveSection)]
    pub fn set_active_section(&mut self, name: &str) {
        self.pp.set_active_section(name);
    }

    #[wasm_bindgen(js_name = createHeap)]
    pub fn create_heap(&mut self, offset: u32, size: u32) {
        self.pp.create_heap(offset, size);
    }
}

#[wasm_bindgen]
pub fn assemble_line(line: &str) -> Result<u16, String> {
    let ins = Instruction::from_str(line).map_err(|x| format!("failed to parse: {:?}", x))?;
    Ok(ins.encode_u16())
}

#[wasm_bindgen]
pub fn dissasemble_instruction(i: u16) -> Result<String, String> {
    let ins = Instruction::try_from(i)?;
    Ok(format!("{}", ins))
}
