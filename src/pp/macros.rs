use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

use crate::pp::{PreProcessor};

pub fn setup_std_macros(pp: &mut PreProcessor) {
    pp.define_macro("defvar", defvar);
    pp.define_macro("include", include);
    pp.define_macro("defmacro", defmacro);
}

pub fn defvar(pp: &mut PreProcessor, input: Vec<&str>) -> Result<Vec<String>, String> {
    if input.len() != 2 {
        return Err("format <name> <value>".to_string());
    }
    let name = input.get(0).unwrap();
    let value = input.get(1).unwrap();
    pp.define_variable(name, value);
    Ok(Vec::new())
}

pub fn include(_pp: &mut PreProcessor, input: Vec<&str>) -> Result<Vec<String>, String> {
    if input.len() != 1 {
        return Err(format!("requires exactly 1 argument, got {}", input.len()));
    }
    let file_name = input.get(0).unwrap();
    let file = File::open(Path::new(&file_name))
        .map_err(|x| format!("failed to open \"{}\": {}", file_name, x))?;
    let mut out: Vec<String> = Vec::new();
    for line in BufReader::new(file).lines() {
        let line_inner = line.map_err(|x| format!("{}", x))?;
        out.push(line_inner);
    }
    Ok(out)
}

pub fn defmacro(pp: &mut PreProcessor, input: Vec<&str>) -> Result<Vec<String>,String> {
    if input.is_empty() {
        return Err(format!("no arguments"));
    }
    let macro_name = input.get(0).unwrap();
    let mut lines: Vec<String> = Vec::new();
    let mut current_line: Vec<String> = Vec::new();
    for &token in &input[1..] {
        if token != "/" {
            current_line.push(token.to_string());  
        } else {
            lines.push(current_line.join(" "));
            current_line.clear();
        }
    }
    if !current_line.is_empty() {
        lines.push(current_line.join(" "));
    }
    pp.define_subst_macro(macro_name, lines);
    Ok(Vec::new())
}

