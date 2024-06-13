pub mod parse;
pub mod combinator;
pub mod character;
pub mod ast;
mod language;

use crate::parse::run_parser;
use crate::language::*;

fn main() -> Result<(), String> {
    let c = run_parser(statement, "x := 5")?; 
    println!("got: {c}");
    Ok(())
}

