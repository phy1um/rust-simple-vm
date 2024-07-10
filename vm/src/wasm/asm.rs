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
    pub fn resolve(&mut self, input: &str) -> Result<(), String> {
        let lines = self
            .pp
            .resolve(input)
            .map_err(|_| "failed to resolve".to_string())?;
        let mut out: Vec<String> = Vec::new();
        for l in lines {
            out.push(
                self.pp
                    .resolve_pass2(&l)
                    .map_err(|x| format!("error @ line {}: {x}", l.get_line_number()))?,
            );
        }
        self.resolved_lines = Some(out);
        let mut syms = Vec::new();
        for (k, v) in self.pp.variables.iter() {
            syms.push(ResolvedSymbol {
                name: k.to_owned(),
                kind: match v {
                    pp::Variable::Label(_) => SymbolKind::Label,
                    pp::Variable::User(_) => SymbolKind::Variable,
                },
                value: match v {
                    pp::Variable::Label(s) => s.to_owned(),
                    pp::Variable::User(s) => s.to_owned(),
                },
            });
        }
        self.resolved_symbols = Some(syms);
        Ok(())
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
        self.resolved_symbols
            .clone()
            .ok_or("no resolved program".to_string())
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
