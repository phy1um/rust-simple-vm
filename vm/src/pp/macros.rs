use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

use std::str::FromStr;

use crate::binfmt::SectionMode;
use crate::pp::PreProcessor;

pub fn setup_std_macros(pp: &mut PreProcessor) {
    pp.define_macro("defvar", defvar);
    pp.define_macro("include", include);
    pp.define_macro("defmacro", defmacro);
    pp.define_macro("section", section);
    pp.define_macro("heap", add_heap);
    pp.define_macro("string", data_string);
    pp.define_macro("bytes", data_bytes);
    pp.define_macro("entrypoint", set_entrypoint);
}

pub fn set_entrypoint(pp: &mut PreProcessor, input: Vec<&str>) -> Result<Vec<String>, String> {
    if input.len() != 1 {
        return Err("format <value>".to_string());
    }
    let value: u16 = input
        .first()
        .unwrap()
        .parse()
        .map_err(|e| format!("parse entrypoint: {e}"))?;
    pp.set_entrypoint(value);
    Ok(Vec::new())
}

pub fn section(pp: &mut PreProcessor, input: Vec<&str>) -> Result<Vec<String>, String> {
    if input.len() == 1 {
        let name = input.first().unwrap();
        pp.set_active_section(name);
        Ok(Vec::new())
    } else if input.len() != 3 {
        Err("format <name> <offset> <kind>".to_string())
    } else {
        let name = input.first().unwrap();
        let (num, base) = input.get(1).map(|s| get_base(s)).unwrap();
        let offset = u32::from_str_radix(num, base).map_err(|e| format!("parse offset: {e}"))?;
        let kind = SectionMode::from_str(input.get(2).unwrap())?;
        if kind == SectionMode::Heap {
            return Err("cannot create heap using this macro, use `.heap` instead".to_string());
        };
        pp.create_section(name, offset, kind);
        pp.set_active_section(name);
        Ok(Vec::new())
    }
}

pub fn add_heap(pp: &mut PreProcessor, input: Vec<&str>) -> Result<Vec<String>, String> {
    if input.len() != 2 {
        Err("format <offset> <size>".to_string())
    } else {
        let (offset_num, offset_base) = input.first().map(|s| get_base(s)).unwrap();
        let offset = u32::from_str_radix(offset_num, offset_base)
            .map_err(|e| format!("parse offset: {e}"))?;
        let (size_num, size_base) = input.get(1).map(|s| get_base(s)).unwrap();
        let size =
            u32::from_str_radix(size_num, size_base).map_err(|e| format!("parse size: {e}"))?;
        pp.create_heap(offset, size);
        Ok(Vec::new())
    }
}

pub fn data_string(pp: &mut PreProcessor, input: Vec<&str>) -> Result<Vec<String>, String> {
    let data = input.join(" ");
    pp.write_section_raw(data.as_bytes());
    Ok(Vec::new())
}

pub fn data_bytes(pp: &mut PreProcessor, input: Vec<&str>) -> Result<Vec<String>, String> {
    let mut data = Vec::new();
    for i in input.iter() {
        let value = u8::from_str_radix(i, 16).map_err(|e| format!("invalid byte {i}: {e}"))?;
        data.push(value);
    }
    pp.write_section_raw(&data);
    Ok(Vec::new())
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

fn get_base(s: &str) -> (&str, u32) {
    match s.chars().next() {
        Some('$') => (&s[1..s.len() - 1], 16),
        _ => (s, 10),
    }
}
