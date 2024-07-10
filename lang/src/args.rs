use std::fmt;

pub enum ArgsError {
    UnknownFlag(String),
}

#[derive(Debug, Default, PartialEq)]
pub enum OutputFormat {
    #[default]
    Binary,
    AnnotatedAsm,
}

impl fmt::Display for ArgsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArgsError::UnknownFlag(s) => write!(f, "unknown flag: {}", s),
        }
    }
}

#[derive(Debug, Default)]
pub struct Args {
    pub bin_name: String,
    pub target_files: Vec<String>,
    pub output_format: OutputFormat,
    pub binary_offset: usize,
    show_help: bool,
}

impl Args {
    pub fn validate(&self) -> bool {
        if self.show_help {
            return false;
        };
        !self.target_files.is_empty()
    }

    pub fn usage(&self) -> String {
        format!(
            "usage: {} [OPTIONS] <input files...>

options:
    -h, --help\tShow this message.
    -a, --readable-asm\tOutput is annotated assembly.
",
            self.bin_name
        )
    }
}

pub fn process_cli(args: &[String]) -> Result<Args, ArgsError> {
    let mut out = Args {
        bin_name: args[0].to_string(),
        ..Args::default()
    };
    for a in &args[1..] {
        if let Some(flag) = a.strip_prefix("--") {
            match flag {
                "--readable-asm" => out.output_format = OutputFormat::AnnotatedAsm,
                "help" => out.show_help = true,
                x => return Err(ArgsError::UnknownFlag(x.to_string())),
            }
        } else if let Some(flag) = a.strip_prefix('-') {
            match flag {
                "a" => out.output_format = OutputFormat::AnnotatedAsm,
                "h" => out.show_help = true,
                x => return Err(ArgsError::UnknownFlag(x.to_string())),
            }
        } else {
            out.target_files.push(a.to_string());
        }
    }
    Ok(out)
}
