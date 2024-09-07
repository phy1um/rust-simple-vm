use crate::character::*;
use crate::combinator::*;
use crate::language::tokenize::Token;

use std::fmt;

#[derive(Debug, Clone)]
pub enum LexError {
    NoMatch,
    Incomplete,
    InvalidHexNumber(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Base {
    Base16,
    Base10,
}

impl Base {
    fn format(&self, i: i32) -> String {
        match self {
            Self::Base16 => format!("{i:X}"),
            Self::Base10 => format!("{i}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LexedTokenKind {
    // a string literal, as represented by "quotation marks" in the source.
    LiteralString(String),
    // an integer literal, and the base it was parsed from (eg 0x100).
    LiteralInt(i32, Base),
    // a character literal eg 'a'.
    LiteralChar(char),
    // a series of alphanumeric characters that can be a valid identifier.
    Identifier(String),
    // anything else, eg the characters [, =, >, +,...
    Symbol(String),
}

impl fmt::Display for LexedTokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::LiteralString(s) => write!(f, "\"{s}\""),
            Self::LiteralInt(i, b) => write!(f, "{}", b.format(*i)),
            Self::LiteralChar(c) => write!(f, "'{c}'"),
            Self::Identifier(name) => write!(f, "{name}"),
            Self::Symbol(s) => write!(f, "{s}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LexedToken {
    pub value: LexedTokenKind,
    pub line_number: usize,
    pub position_in_line: usize,
}

impl fmt::Display for LexedToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl LexedToken {
    fn new_from(value: LexedTokenKind, other: Token) -> Self {
        Self {
            value,
            line_number: other.line_number,
            position_in_line: other.position_in_line,
        }
    }
}

impl TryFrom<Token> for LexedToken {
    type Error = LexError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        for lexer in [
            lex_string,
            lex_char,
            lex_int_base16,
            lex_int_base10,
            lex_identifier,
        ] {
            match lexer(value.clone()) {
                Err(LexError::NoMatch) => (),
                Err(LexError::Incomplete) => (),
                Ok(token) => return Ok(token),
                Err(e) => return Err(e),
            }
        }
        Ok(LexedToken::new_from(
            LexedTokenKind::Symbol(value.content.clone()),
            value,
        ))
    }
}

fn lex_string(input: Token) -> Result<LexedToken, LexError> {
    if let Ok((rest, body)) =
        wrapped(is_char('"'), repeat0(not_char("\"")), is_char('"'))(StrState::new(&input.content))
    {
        if !rest.is_empty() {
            Err(LexError::Incomplete)
        } else {
            Ok(LexedToken::new_from(
                LexedTokenKind::LiteralString(body.iter().collect::<String>()),
                input.clone(),
            ))
        }
    } else {
        Err(LexError::NoMatch)
    }
}

fn lex_int_base10(input: Token) -> Result<LexedToken, LexError> {
    let (rest, res) = map(repeat1(numeric), |x| {
        LexedToken::new_from(
            LexedTokenKind::LiteralInt(
                x.iter().collect::<String>().parse::<i32>().unwrap(),
                Base::Base10,
            ),
            input.clone(),
        )
    })(StrState::new(&input.content))
    .map_err(|_| LexError::NoMatch)?;
    if !rest.is_empty() {
        Err(LexError::Incomplete)
    } else {
        Ok(res)
    }
}

fn lex_int_base16(input: Token) -> Result<LexedToken, LexError> {
    let s0 = input.content.strip_prefix("0x").ok_or(LexError::NoMatch)?;
    let (rest, res) = map(repeat1(alphanumeric), |x| {
        let num_str = x.iter().collect::<String>();
        match i32::from_str_radix(&num_str, 16) {
            Ok(num) => Ok(LexedToken::new_from(
                LexedTokenKind::LiteralInt(num, Base::Base16),
                input.clone(),
            )),
            Err(_e) => Err(LexError::InvalidHexNumber(s0.to_string())),
        }
    })(StrState::new(s0))
    .map_err(|_| LexError::NoMatch)?;
    if !rest.is_empty() {
        Err(LexError::Incomplete)
    } else {
        res
    }
}

fn lex_char(input: Token) -> Result<LexedToken, LexError> {
    map(wrapped(token("'"), not_char("'"), token("'")), |c| {
        LexedToken::new_from(LexedTokenKind::LiteralChar(c), input.clone())
    })(StrState::new(&input.content))
    .map_err(|_| LexError::NoMatch)
    .map(|(_s, t)| t)
}

fn lex_identifier(input: Token) -> Result<LexedToken, LexError> {
    let (s0, fst) = char_predicate_or(alpha, is_char('_'))(StrState::new(&input.content))
        .map_err(|_| LexError::NoMatch)?;
    let (state_tail, rest) = repeat0(char_predicate_or(alphanumeric, is_char('_')))(s0)
        .map_err(|_| LexError::NoMatch)?;
    let id_str = fst.to_string() + &rest.iter().collect::<String>();
    if !state_tail.is_empty() {
        Err(LexError::Incomplete)
    } else {
        Ok(LexedToken::new_from(
            LexedTokenKind::Identifier(id_str),
            input.clone(),
        ))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::error::{ParseError, ParseErrorKind};
    use crate::language::tokenize::tokens;

    #[test]
    fn lex_identifier() {
        let input = "foo bar baz";
        let tokens = tokens(input);
        let lexed: Vec<LexedToken> = tokens
            .iter()
            .map(|x| {
                x.clone()
                    .try_into()
                    .map_err(|e| ParseError::from_token_prelex(&x, ParseErrorKind::LexError(e)))
            })
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(
            LexedToken {
                line_number: 1,
                position_in_line: 1,
                value: LexedTokenKind::Identifier("foo".to_string()),
            },
            lexed.first().unwrap().clone()
        );
    }
}
