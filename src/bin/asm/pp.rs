
use std::collections::HashMap;
use std::fmt;

pub enum Error {
    Unknown(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Unknown(s) => write!(f, "unknown: {}", s)
        }
    }
}

pub struct PreProcessor {
    variables: HashMap<String, String>,
}

impl PreProcessor {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    pub fn resolve(&mut self, input: &str) -> Result<String, Error> {
        let parts: Vec<_> = input.split(' ').collect();
        // resolved :: Vec<Result<String, Error>>
        let resolved = parts.into_iter().map(|p| {
            match p.chars().nth(0) {
                Some('!') => {
                    self.get_variable(&p[1..]).ok_or(
                        Error::Unknown(format!("token {}", p)))
                }
                _ => Ok(p.to_string())
            }
        });
        let st: Result<Vec<String>, Error> = resolved.into_iter().collect();
        Ok(st?.join(" "))
    }

    fn get_variable(&self, name: &str) -> Option<String> {
        self.variables.get(name).cloned()
    }

    pub fn define_variable(&mut self, name: &str, value: &str) {
        self.variables.insert(name.to_string(), value.to_string());
    }
}
