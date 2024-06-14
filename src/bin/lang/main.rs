pub mod parse;
pub mod combinator;
pub mod character;
pub mod ast;
mod language;

use crate::parse::run_parser;
use crate::language::*;

fn main() -> Result<(), String> {
    let program = run_parser(parse_ast, "int foo() {\nlet int a := 7;\na := 99;\n}\n")?;
    for p in program {
        println!("{p}\n");
    }
    Ok(())
}

