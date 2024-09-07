use crate::character::{char_predicate, is_char, not_char, whitespace, StrState};
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

// TODO: this function is disgusting because of opaque types
fn next_token(input: StrState<'_>) -> Result<(StrState<'_>, Token), ParseError> {
    if input.is_empty() {
        Err(ParseError::from_str_state(
            &input,
            ParseErrorKind::EndOfInput,
        ))
    } else if let Ok((sn, res)) = map(
        repeat1(not_char("{}[]()+-*%<>=:;^&|.,!@#&*'\" \n\t")),
        |cs| {
            Token::new(
                &cs.iter().collect::<String>(),
                input.line_number,
                input.position_in_line,
            )
        },
    )(input)
    {
        Ok((sn, res))
    } else if let Ok((sn, res)) = literal_string(input) {
        Ok((sn, res))
    } else if let Ok((sn, res)) = literal_char(input) {
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
            char_predicate(
                |c| "{}[]()+-*%<>=:;^&|.,!@#&*".contains(c),
                "invalid".to_string(),
            ),
            |cs| Token::new(&cs.to_string(), input.line_number, input.position_in_line),
        )(input)
        .map_err(|_| ParseError::from_str_state(&input, ParseErrorKind::EndOfInput))
    }
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
                t("foo", 0, 0),
                t("bar", 0, 4),
                t(":=", 0, 8),
                t("baz", 0, 11),
                t("[", 0, 14),
                t("52", 0, 15),
                t("]", 0, 17),
                t("*", 0, 19),
                t("8", 0, 20),
                t("+", 0, 21),
                t("7", 0, 22),
                t("-", 0, 23),
                t("5", 0, 24)
            ],
            res
        );
    }

    #[test]
    fn tokenize_with_string_literal() {
        let res = tokens("foo := \"hello world\"");
        assert_eq!(
            vec![t("foo", 0, 0), t(":=", 0, 4), t("\"hello world\"", 0, 7)],
            res
        );
    }
}
