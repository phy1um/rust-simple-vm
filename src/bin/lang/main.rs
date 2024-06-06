pub mod parse;
pub mod combinator;

use crate::parse::Parser;
use crate::combinator::*;

fn run_parser<S, T, E>(parser: impl Parser<S, T, E>, input: S) -> Result<T, E> 
{
    let (_, res) = parser.run(input)?;
    Ok(res)
}

fn is_char(c: char) -> impl Fn(&str) -> Result<(&str, char), String> {
    move |input| {
        if let Some(x) = input.chars().next() {
            if x == c {
                Ok((&input[1..], c))
            } else {
                Err(format!("expected '{c}' got '{x}'"))
            }
        } else {
            Err("empty".to_string())
        }
    }
}

fn main() -> Result<(), String> {
    let cmb = Sequence::new(vec![is_char('a'), is_char('o'), is_char('o')]);
    let c = run_parser(map(cmb, |cs| cs.iter().collect::<String>()), "aoo bar")?; 
    println!("got: {c}");
    Ok(())
}

