use std::collections::HashMap;
use std::fmt;

pub mod macros;

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

#[derive(Debug)]
pub enum Macro {
    Func(MacroFunc),
    Subst(Vec<String>),
}

#[derive(Debug, Clone)]
pub enum Variable {
    Label(String),
    User(String),
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Label(s) => write!(f, "{s}"),
            Self::User(s) => write!(f, "{s}"),
        }
    }
}

#[derive(Debug, Default)]
pub struct PreProcessor {
    pub variables: HashMap<String, Variable>,
    macros: HashMap<String, Macro>,
    instruction_count: u32,
}

#[derive(Debug)]
pub struct ProcessedLine {
    source_line_number: usize,
    line: ProcessedLinePart,
}

#[derive(Debug)]
enum ProcessedLinePart {
    Line(String),
    Unresolved(String, Box<ProcessedLinePart>, Box<ProcessedLinePart>),
}

impl ProcessedLine {
    pub fn from_str(s: &str, source_line_number: usize) -> Self {
        Self {
            source_line_number,
            line: ProcessedLinePart::Line(s.to_string()),
        }
    }

    pub fn get_line_number(&self) -> usize {
        self.source_line_number
    }
}

impl PreProcessor {
    pub fn resolve_pass2(&self, p: &ProcessedLine) -> Result<String, Error> {
        self.reprocess_line(&p.line)
    }

    fn reprocess_line(&self, p: &ProcessedLinePart) -> Result<String, Error> {
        match p {
            ProcessedLinePart::Line(s) => Ok(s.to_string()),
            ProcessedLinePart::Unresolved(varname, pre, post) => {
                let value = self
                    .get_variable(varname)
                    .map(|x| x.to_string())
                    .ok_or(Error::UnknownToken(varname.to_string()))?;
                Ok(format!(
                    "{} {} {}",
                    self.reprocess_line(pre)?,
                    value,
                    self.reprocess_line(post)?
                ))
            }
        }
    }

    pub fn resolve(&mut self, input: &str) -> Result<Vec<ProcessedLine>, Error> {
        let mut res: Vec<ProcessedLine> = Vec::new();
        for line in input.lines() {
            let parts: Vec<_> = line.split_whitespace().collect();
            if parts.is_empty() {
                continue;
            }
            if let Some(head) = parts.first() {
                match head.chars().nth(0) {
                    Some(';') => {
                        res.push(ProcessedLine::from_str(
                            line,
                            self.instruction_count as usize,
                        ));
                        continue;
                    }
                    Some('.') => {
                        let name = &head[1..];
                        let func = self
                            .get_macro(name)
                            .ok_or(Error::UnknownToken(head[1..].to_string()))?;
                        let macro_res = match func {
                            Macro::Func(f) => f(self, parts[1..].to_vec())
                                .map_err(|x| Error::MacroEval(name.to_string(), x))?,
                            Macro::Subst(lines) => {
                                lines
                                    .iter()
                                    .map(|line| {
                                        let mp: Result<Vec<String>, String> = line
                                            .split(' ')
                                            .map(|p| {
                                                match p.chars().nth(0) {
                                                    Some('!') => {
                                                        match p[1..].parse::<u32>() {
                                                            Ok(n) => parts
                                                                .get((n + 1) as usize)
                                                                .ok_or(format!(
                                                                    "subst {}: out of bounds",
                                                                    p
                                                                ))
                                                                .map(|x| x.to_string()),
                                                            Err(_) => {
                                                                Ok(p.to_string())
                                                                // Err(format!("parse {}: {}", p, e))
                                                            }
                                                        }
                                                    }
                                                    _ => Ok(p.to_string()),
                                                }
                                            })
                                            .collect();
                                        // TODO: handle error here
                                        match mp {
                                            Ok(s) => s.join(" "),
                                            Err(e) => format!("err: {}", e),
                                        }
                                    })
                                    .collect()
                            }
                        };
                        let b = macro_res.join("\n");
                        let mut resolved = self.resolve(&b)?;
                        res.append(&mut resolved);
                        continue;
                    }
                    Some(':') => {
                        let name = &head[1..];
                        let offset = format!("{}", self.instruction_count * 2);
                        self.define_label(name, &offset);
                        continue;
                    }
                    _ => (),
                }
            }

            res.push(ProcessedLine {
                source_line_number: self.instruction_count as usize,
                line: self.build_parts(parts),
            });
            self.instruction_count += 1;
        }
        Ok(res)
    }

    fn build_parts(&self, parts: Vec<&str>) -> ProcessedLinePart {
        let mut line: Vec<String> = Vec::new();
        for i in 0..parts.len() {
            if let Some('!') = parts[i].chars().nth(0) {
                let varname = &parts[i][1..].to_string();
                match self.get_variable(varname).map(|x| x.to_string()) {
                    Some(x) => line.push(x),
                    None => {
                        return ProcessedLinePart::Unresolved(
                            varname.to_string(),
                            Box::new(ProcessedLinePart::Line(line.join(" "))),
                            Box::new(self.build_parts(parts[i + 1..].to_vec())),
                        )
                    }
                }
            } else {
                line.push(parts[i].to_string());
            }
        }
        ProcessedLinePart::Line(line.join(" "))
    }

    fn get_variable(&self, name: &str) -> Option<Variable> {
        self.variables.get(name).cloned()
    }

    pub fn define_label(&mut self, name: &str, value: &str) {
        self.variables
            .insert(name.to_string(), Variable::Label(value.to_string()));
    }

    pub fn define_user_variable(&mut self, name: &str, value: &str) {
        self.variables
            .insert(name.to_string(), Variable::User(value.to_string()));
    }

    fn get_macro(&mut self, name: &str) -> Option<&Macro> {
        self.macros.get(name)
    }

    pub fn define_macro(&mut self, name: &str, value: MacroFunc) {
        self.macros.insert(name.to_string(), Macro::Func(value));
    }

    pub fn define_subst_macro(&mut self, name: &str, value: Vec<String>) {
        self.macros.insert(name.to_string(), Macro::Subst(value));
    }
}
