use crate::character::{char_predicate, is_char, not_char, whitespace, StrState};
use crate::combinator::*;
use crate::error::{ParseError, ParseErrorKind};
use crate::parse::Parser;

const DELIMITERS: &str = "{}[]()+-*%<>=:;^&|.,!@#&*'\"";
const DELIMITERS_AND_SPACES: &str = "{}[]()+-*%<>=:;^&|.,!@#&*'\" \n\t";

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Token {
    pub content: String,
    pub line_number: usize,
    pub position_in_line: usize,
}

impl Token {
    fn new(s: &str, line_number: usize, position_in_line: usize) -> Self {
        Token {
            content: s.to_string(),
            line_number,
            position_in_line,
        }
    }
}

type TResult<'a, T, E> = Result<(StrState<'a>, T), E>;

fn lit_token<'a>(t: &'static str) -> impl Fn(StrState<'a>) -> TResult<'a, Token, ParseError> {
    move |input| {
        map(crate::character::token(t), |s| {
            Token::new(s, input.line_number, input.position_in_line)
        })(input)
        .map_err(|_| {
            ParseError::from_str_state(&input, ParseErrorKind::ExpectedToken(t.to_string()))
        })
    }
}

fn literal_string(input: StrState<'_>) -> TResult<'_, Token, ParseError> {
    let (sn, res) = wrapped(is_char('"'), repeat0(not_char("\"")), is_char('"'))(input)
        .map_err(|_| ParseError::from_str_state(&input, ParseErrorKind::ExpectedString))?;
    let quoted = format!("\"{}\"", res.iter().collect::<String>());
    Ok((
        sn,
        Token::new(&quoted, input.line_number, input.position_in_line),
    ))
}

fn literal_char(input: StrState<'_>) -> TResult<'_, Token, ParseError> {
    let (sn, res) = wrapped(is_char('\''), not_char("'"), is_char('\''))(input)
        .map_err(|_| ParseError::from_str_state(&input, ParseErrorKind::ExpectedChar))?;
    Ok((
        sn,
        Token::new(
            &format!("'{res}'"),
            input.line_number,
            input.position_in_line,
        ),
    ))
}

fn next_token(input: StrState<'_>) -> Result<(StrState<'_>, Token), ParseError> {
    if input.is_empty() {
        Err(ParseError::end_of_input())
    // try to get a series of characters split on delimiters, or string, or char literal
    } else if let Ok((sn, Some(res))) =
        Any::new(vec![token_delimited, literal_string, literal_char]).run(input)
    {
        Ok((sn, res))
    // try to get multi-delimiter sequences eg ">=" into one token. these are in an else-if because of
    // opaque types :(
    } else if let Ok((sn, Some(res))) = Any::new(vec![
        lit_token(":="),
        lit_token("=="),
        lit_token(">="),
        lit_token("<="),
    ])
    .run(input)
    {
        Ok((sn, res))
    // otherwise put a single delimited character in its own token. this should cover all possible
    // characters!!!
    } else {
        map(
            char_predicate(|c| DELIMITERS.contains(c), "invalid".to_string()),
            |cs| Token::new(&cs.to_string(), input.line_number, input.position_in_line),
        )(input)
        .map_err(|_| ParseError::from_str_state(&input, ParseErrorKind::EndOfInput))
    }
}

fn token_delimited(input: StrState<'_>) -> Result<(StrState<'_>, Token), ParseError> {
    map(repeat1(not_char(DELIMITERS_AND_SPACES)), |cs| {
        Token::new(
            &cs.iter().collect::<String>(),
            input.line_number,
            input.position_in_line,
        )
    })(input)
    .map_err(|_| ParseError::from_str_state(&input, ParseErrorKind::ExpectedIdentifier))
}

pub(crate) fn tokens(input: &str) -> Vec<Token> {
    match repeat0(skip_whitespace(next_token))(StrState::new(input)) {
        Err(_) => panic!("bad"),
        Ok((_sn, res)) => res,
    }
}

fn skip_whitespace<'a, T, F>(f: F) -> impl Fn(StrState<'a>) -> Result<(StrState<'a>, T), ParseError>
where
    F: Parser<StrState<'a>, T, ParseError>,
{
    move |input| {
        let (s0, _) = discard(repeat0(whitespace))(input)
            .map_err(|_| ParseError::from_str_state(&input, ParseErrorKind::ExpectedInt))?;
        f.run(s0)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn t(s: &str, line_number: usize, position_in_line: usize) -> Token {
        Token {
            content: s.to_string(),
            line_number,
            position_in_line,
        }
    }

    #[test]
    fn tokenize() {
        let res = tokens("foo bar := baz[52] *8+7-5");
        assert_eq!(
            vec![
                t("foo", 1, 1),
                t("bar", 1, 5),
                t(":=", 1, 9),
                t("baz", 1, 12),
                t("[", 1, 15),
                t("52", 1, 16),
                t("]", 1, 18),
                t("*", 1, 20),
                t("8", 1, 21),
                t("+", 1, 22),
                t("7", 1, 23),
                t("-", 1, 24),
                t("5", 1, 25)
            ],
            res
        );
    }

    #[test]
    fn tokenize_with_string_literal() {
        let res = tokens("foo := \"hello world\"");
        assert_eq!(
            vec![t("foo", 1, 1), t(":=", 1, 5), t("\"hello world\"", 1, 8)],
            res
        );
    }
}
