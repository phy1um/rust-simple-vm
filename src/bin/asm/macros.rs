
use crate::pp::{PreProcessor};

pub fn macro_defvar(pp: &mut PreProcessor, input: Vec<&str>) -> Result<Vec<String>, String> {
    if input.len() != 2 {
        return Err("format <name> <value>".to_string());
    }
    let name = input.get(0).unwrap();
    let value = input.get(1).unwrap();
    pp.define_variable(name, value);
    Ok(Vec::new())
}
