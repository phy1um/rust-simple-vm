use std::fmt;

pub enum ArgsError {
    ExtraInput,
    UnknownFlag(String),
}

impl fmt::Display for ArgsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArgsError::ExtraInput => write!(f, "extra input file(s)"),
            ArgsError::UnknownFlag(s) => write!(f, "unknown flag: {}", s),
        }
    }
}

pub struct Args {
    pub bin_name: String,
    pub input_file: Option<String>,
    pub preprocess_only: bool,
    pub map_binary_at: usize,
    show_help: bool,
}

impl Args {
    pub fn validate(&self) -> bool {
        if self.show_help {
            return false;
        };
        self.input_file.is_some()
    }

    pub fn usage(&self) -> String {
        format!(
            "usage: {} [OPTIONS] <input file>

options:
    -h, --help\tShow this message.
    -p, --preprocess-only\tStop after running the preprocessor and print the instructions.
    -x, --program-offset\tAddress to load program at initialzie PC register.

",
            self.bin_name
        )
    }
}

impl Default for Args {
    fn default() -> Self {
        Self {
            bin_name: ".".to_string(),
            input_file: None,
            preprocess_only: false,
            show_help: false,
            map_binary_at: 0x0,
        }
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
                "preprocess-only" => out.preprocess_only = true,
                "help" => out.show_help = true,
                x => return Err(ArgsError::UnknownFlag(x.to_string())),
            }
        } else if let Some(flag) = a.strip_prefix('-') {
            match flag {
                "p" => out.preprocess_only = true,
                "h" => out.show_help = true,
                x => return Err(ArgsError::UnknownFlag(x.to_string())),
            }
        } else {
            if out.input_file.is_some() {
                return Err(ArgsError::ExtraInput);
            };
            out.input_file = Some(a.to_string());
        }
    }
    Ok(out)
}
