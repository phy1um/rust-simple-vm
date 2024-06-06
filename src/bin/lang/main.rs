pub mod parse;
pub mod combinator;
pub mod character;

use crate::parse::Parser;
use crate::combinator::*;
use crate::character::*;

fn run_parser<S, T, E>(parser: impl Parser<S, T, E>, input: S) -> Result<T, E> 
{
    let (_, res) = parser.run(input)?;
    Ok(res)
}

fn main() -> Result<(), String> {
    // let cmb = repeat(1, "expected at least 1".to_string(), alpha);
    let seq = Sequence::new(vec![token("<<>>"), token("aoo "), token("bar")]);
    let c = run_parser(seq, "<<>>aoo bar")?; 
    println!("got: {c:?}");
    Ok(())
}

