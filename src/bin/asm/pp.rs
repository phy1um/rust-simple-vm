
use std::collections::HashMap;
use std::fmt;

pub enum Error {
    UnknownToken(String),
    MacroEval(String, String),
    Other(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::UnknownToken(s) => write!(f, "unknown token: {}", s),
            Error::MacroEval(name, err) => write!(f, "eval macro {}: {}", name, err),
            Error::Other(s) => write!(f, "{}", s),
        }
    }
}

type MacroFunc = fn(&mut PreProcessor, input: Vec<&str>) -> Result<Vec<String>, String>;

pub struct PreProcessor {
    variables: HashMap<String, String>,
    macros: HashMap<String, MacroFunc>,
    instruction_count: u32,
}

impl PreProcessor {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            macros: HashMap::new(),
            instruction_count: 0,
        }
    }

    pub fn resolve(&mut self, input: &str) -> Result<String, Error> {
        let parts: Vec<_> = input.split(' ').collect();
        if parts.len() == 0 {
            return Ok(String::new());
        }
        if let Some(head) = parts.get(0) {
            match head.chars().nth(0) {
                Some(';') => return Ok(input.to_string()),
                Some('.') => {
                    let name = &head[1..];
                    let func = self.get_macro(name).ok_or(
                            Error::Other(format!("macro {}", head)))?;
                    let res = func(self, parts[1..].to_vec()).map_err(|x| Error::MacroEval(name.to_string(), x))?;
                    let resolved: Result<Vec<String>, Error> = res.into_iter().map(|line| {
                        self.resolve(&line)
                    }).collect();
                    return Ok(resolved?.join("\n"));
                }
                Some(':') => {
                    let name = &head[1..];
                    let offset = format!("{}", self.instruction_count*2);
                    self.define_variable(name, &offset);
                    return Ok(String::new());
                }
                _ => (),
            }
        }
        // resolved :: Vec<Result<String, Error>>
        let resolved = parts.into_iter().map(|p| {
            match p.chars().nth(0) {
                Some('!') => {
                    self.get_variable(&p[1..]).ok_or(
                        Error::UnknownToken(p[1..].to_string()))
                }
                _ => Ok(p.to_string())
            }
        });
        let st: Result<Vec<String>, Error> = resolved.into_iter().collect();
        self.instruction_count += 1;
        Ok(st?.join(" "))
    }

    fn get_variable(&self, name: &str) -> Option<String> {
        self.variables.get(name).cloned()
    }

    pub fn define_variable(&mut self, name: &str, value: &str) {
        self.variables.insert(name.to_string(), value.to_string());
    }

    fn get_macro(&mut self, name: &str) -> Option<&MacroFunc> {
        self.macros.get(name)
    }

    pub fn define_macro(&mut self, name: &str, value: MacroFunc) {
        self.macros.insert(name.to_string(), value);
    }
}
