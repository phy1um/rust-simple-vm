use std::convert::Infallible;

use crate::character::{char_predicate, is_char, not_char, whitespace};
use crate::combinator::*;
use crate::error::{ParseError, ParseErrorKind};
use crate::parse::Parser;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Token {
    pub content: String,
    pub line_number: usize,
    pub position_in_line: usize,
}

impl Token {
    fn new(s: &str) -> Self {
        Token {
            content: s.to_string(),
            line_number: 0,
            position_in_line: 0,
        }
    }
}

type TResult<'a, T, E> = Result<(&'a str, T), E>;

fn lit_token<'a>(t: &'static str) -> impl Fn(&'a str) -> TResult<'a, Token, ParseError> {
    move |input| {
        map(crate::character::token(t), |s| Token::new(s))(input)
            .map_err(|_| ParseError::new(input, ParseErrorKind::ExpectedToken(t.to_string())))
    }
}

fn literal_string<'a>(input: &'a str) -> TResult<'a, Token, ParseError> {
    let (sn, res) = wrapped(is_char('"'), repeat0(not_char("\"")), is_char('"'))(input)
        .map_err(|_| ParseError::new("", ParseErrorKind::ExpectedString))?;
    let quoted = format!("\"{}\"", res.iter().collect::<String>());
    Ok((sn, Token::new(&quoted)))
}

// TODO: this function is disgusting because of opaque types
fn next_token(input: &str) -> Result<(&str, Token), ParseError> {
    if input.is_empty() {
        Err(ParseError::new("", ParseErrorKind::EndOfInput))
    } else if let Ok((sn, res)) = map(repeat1(not_char("{}[]()+-*%<>=:\" \n\t")), |cs| {
        Token::new(&cs.iter().collect::<String>())
    })(input)
    {
        Ok((sn, res))
    } else if let Ok((sn, res)) = literal_string(input) {
        Ok((sn, res))
    } else if let Ok((sn, Some(res))) = Any::new(vec![
        lit_token(":="),
        lit_token("=="),
        lit_token(">="),
        lit_token("<="),
    ])
    .run(input)
    {
        Ok((sn, res))
    } else {
        map(
            char_predicate(|c| "{}[]()+-*%<>=:".contains(c), "invalid".to_string()),
            |cs| Token::new(&cs.to_string()),
        )(input)
        .map_err(|_| ParseError::new("", ParseErrorKind::EndOfInput))
    }
}

pub(crate) fn tokens(input: &str) -> Result<(&str, Vec<Token>), Infallible> {
    match repeat0(skip_whitespace(next_token))(input) {
        Err(_) => panic!("bad"),
        Ok((sn, res)) => Ok((sn, res)),
    }
}

fn skip_whitespace<'a, T, F>(f: F) -> impl Fn(&'a str) -> Result<(&'a str, T), ParseError>
where
    F: Parser<&'a str, T, ParseError>,
{
    move |input| {
        let (s0, _) = discard(repeat0(whitespace))(input)
            .map_err(|_| ParseError::new("", ParseErrorKind::ExpectedInt))?;
        f.run(s0)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn t(s: &str) -> Token {
        Token {
            content: s.to_string(),
            line_number: 0,
            position_in_line: 0,
        }
    }

    #[test]
    fn tokenize() {
        let (_, res) = tokens("foo bar := baz[52] *8+7-5").unwrap();
        assert_eq!(
            vec![
                t("foo"),
                t("bar"),
                t(":="),
                t("baz"),
                t("["),
                t("52"),
                t("]"),
                t("*"),
                t("8"),
                t("+"),
                t("7"),
                t("-"),
                t("5")
            ],
            res
        );
    }

    #[test]
    fn tokenize_with_string_literal() {
        let (_, res) = tokens("foo := \"hello world\"").unwrap();
        assert_eq!(vec![t("foo"), t(":="), t("\"hello world\"")], res);
    }
}
