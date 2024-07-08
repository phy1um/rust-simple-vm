use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

use crate::pp::PreProcessor;
use crate::Instruction;

pub fn setup_std_macros(pp: &mut PreProcessor) {
    pp.define_macro("defvar", defvar);
    pp.define_macro("include", include);
    pp.define_macro("defmacro", defmacro);
    pp.define_macro("offsetPC", set_pc_offset);
}

pub fn defvar(pp: &mut PreProcessor, input: Vec<&str>) -> Result<Vec<String>, String> {
    if input.len() != 2 {
        return Err("format <name> <value>".to_string());
    }
    let name = input.first().unwrap();
    let value = input.get(1).unwrap();
    pp.define_user_variable(name, value);
    Ok(Vec::new())
}

pub fn include(_pp: &mut PreProcessor, input: Vec<&str>) -> Result<Vec<String>, String> {
    if input.len() != 1 {
        return Err(format!("requires exactly 1 argument, got {}", input.len()));
    }
    let file_name = input.first().unwrap();
    let file = File::open(Path::new(&file_name))
        .map_err(|x| format!("failed to open \"{}\": {}", file_name, x))?;
    let mut out: Vec<String> = Vec::new();
    for line in BufReader::new(file).lines() {
        let line_inner = line.map_err(|x| format!("{}", x))?;
        out.push(line_inner);
    }
    Ok(out)
}

pub fn defmacro(pp: &mut PreProcessor, input: Vec<&str>) -> Result<Vec<String>, String> {
    if input.is_empty() {
        return Err("no arguments".to_string());
    }
    let macro_name = input.first().unwrap();
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

pub fn set_pc_offset(pp: &mut PreProcessor, input: Vec<&str>) -> Result<Vec<String>, String> {
    if input.len() != 1 {
        return Err(format!("requires exactly 1 argument, got {}", input.len()));
    }
    let (num, base) = Instruction::pre_handle_number(input.first().unwrap())?;
    let offset = u32::from_str_radix(num, base).map_err(|_| format!("invalid number: {}", num))?;
    pp.instruction_count = offset;
    Ok(Vec::new())
}
