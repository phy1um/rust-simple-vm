#[derive(Debug, Clone)]
pub enum CharError {
    ExpectedChar { expected: char, got: char },
    UnexpectedChar { expected: String, got: char },
    CharFailedPredicate { got: char, name: String },
    ExpectedToken(String),
    EndOfInput,
}

#[derive(Debug, Clone, Copy)]
pub struct StrState<'a> {
    pub input: &'a str,
    pub line_number: usize,
    pub position_in_line: usize,
}

impl<'a> StrState<'a> {
    pub fn new(s: &'a str) -> Self {
        Self {
            input: s,
            line_number: 1,
            position_in_line: 1,
        }
    }

    pub fn next(&self) -> Option<char> {
        self.input.chars().next()
    }

    pub fn is_empty(&self) -> bool {
        self.input.is_empty()
    }

    pub fn advance(self, by: usize) -> Self {
        Self {
            input: &self.input[by..],
            line_number: self.line_number,
            position_in_line: self.position_in_line + by,
        }
    }

    pub fn succ(self) -> Self {
        self.advance(1)
    }

    fn line_feed(self) -> Self {
        Self {
            input: &self.input[1..],
            line_number: self.line_number + 1,
            position_in_line: 1,
        }
    }
}

type CharResult<'a> = Result<(StrState<'a>, char), CharError>;

pub fn is_char<'a>(c: char) -> impl Fn(StrState<'a>) -> CharResult<'a> {
    move |input| {
        if let Some(x) = input.next() {
            if x == c {
                Ok((input.succ(), c))
            } else {
                Err(CharError::ExpectedChar {
                    expected: c,
                    got: x,
                })
            }
        } else {
            Err(CharError::EndOfInput)
        }
    }
}

pub fn not_char<'a, 'b>(
    s: &'a str,
) -> impl Fn(StrState<'b>) -> Result<(StrState<'b>, char), CharError> + 'a {
    move |input| {
        if let Some(c) = input.next() {
            if !s.contains(c) {
                Ok((input.succ(), c))
            } else {
                Err(CharError::UnexpectedChar {
                    expected: s.to_string(),
                    got: c,
                })
            }
        } else {
            Err(CharError::EndOfInput)
        }
    }
}

pub fn token<'a, 'b>(
    s: &'a str,
) -> impl Fn(StrState<'b>) -> Result<(StrState<'b>, &'a str), CharError> {
    move |state| {
        if state.input.strip_prefix(s).is_some() {
            Ok((state.advance(s.len()), s))
        } else {
            Err(CharError::ExpectedToken(s.to_string()))
        }
    }
}

pub fn alpha(input: StrState<'_>) -> CharResult<'_> {
    char_predicate(char::is_alphabetic, "alphabetic".to_string())(input)
}

pub fn numeric(input: StrState<'_>) -> CharResult<'_> {
    char_predicate(char::is_numeric, "numeric".to_string())(input)
}

pub fn alphanumeric(input: StrState<'_>) -> CharResult<'_> {
    char_predicate(char::is_alphanumeric, "alphanumeric".to_string())(input)
}

pub fn whitespace(input: StrState<'_>) -> CharResult<'_> {
    if let Some(c) = input.next() {
        if c == '\n' {
            Ok((input.line_feed(), c))
        } else if c.is_whitespace() {
            Ok((input.succ(), c))
        } else {
            Err(CharError::CharFailedPredicate {
                got: c,
                name: "whitespace".to_string(),
            })
        }
    } else {
        Err(CharError::EndOfInput)
    }
}

pub fn char_predicate_or<'a>(
    a: impl Fn(StrState<'a>) -> CharResult<'a>,
    b: impl Fn(StrState<'a>) -> CharResult<'a>,
) -> impl Fn(StrState<'a>) -> CharResult<'a> {
    move |input| {
        if let Ok(x) = a(input) {
            Ok(x)
        } else {
            b(input)
        }
    }
}

pub fn char_predicate<'a>(
    f: fn(char) -> bool,
    name: String,
) -> impl Fn(StrState<'a>) -> CharResult<'a> {
    move |state| {
        if let Some(c) = state.next() {
            if f(c) {
                Ok((state.succ(), c))
            } else {
                Err(CharError::CharFailedPredicate {
                    got: c,
                    name: name.to_string(),
                })
            }
        } else {
            Err(CharError::EndOfInput)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::combinator::*;

    #[test]
    fn test_numeric() {
        {
            let (s, n) = numeric(StrState::new("1234")).unwrap();
            assert_eq!("234", s.input);
            assert_eq!(n, '1');
        }
        {
            let (s, n) =
                map(repeat1(numeric), |x| x.iter().collect::<String>())(StrState::new("1234"))
                    .unwrap();
            assert_eq!("", s.input);
            assert_eq!("1234", n);
        }
        {
            let (s, n) =
                map(repeat1(numeric), |x| x.iter().collect::<String>())(StrState::new("1234abcd"))
                    .unwrap();
            assert_eq!("abcd", s.input);
            assert_eq!("1234", n);
        }
    }

    #[test]
    fn test_alpha() {
        {
            let (s, n) = alpha(StrState::new("xxxx")).unwrap();
            assert_eq!("xxx", s.input);
            assert_eq!(n, 'x');
        }
        {
            let (s, n) =
                map(repeat1(alpha), |x| x.iter().collect::<String>())(StrState::new("abcdef"))
                    .unwrap();
            assert_eq!("", s.input);
            assert_eq!("abcdef", n);
        }
        {
            let (s, n) =
                map(repeat1(alpha), |x| x.iter().collect::<String>())(StrState::new("xyz:123"))
                    .unwrap();
            assert_eq!(":123", s.input);
            assert_eq!("xyz", n);
        }
    }

    #[test]
    fn test_alphanumeric() {
        {
            let (s, n) = alphanumeric(StrState::new("a7")).unwrap();
            assert_eq!("7", s.input);
            assert_eq!(n, 'a');
        }
        {
            let (s, n) = alphanumeric(StrState::new("9e")).unwrap();
            assert_eq!("e", s.input);
            assert_eq!(n, '9');
        }
        {
            let (s, n) = map(repeat1(alphanumeric), |x| x.iter().collect::<String>())(
                StrState::new("abc123zxy987"),
            )
            .unwrap();
            assert_eq!("", s.input);
            assert_eq!("abc123zxy987", n);
        }
        {
            let (s, n) = map(repeat1(alphanumeric), |x| x.iter().collect::<String>())(
                StrState::new("a1b2c3::9z"),
            )
            .unwrap();
            assert_eq!("::9z", s.input);
            assert_eq!("a1b2c3", n);
        }
    }

    #[test]
    fn test_whitespace() {
        for c in " \n\r\t".chars() {
            let cstr = c.to_string();
            let (s, n) = whitespace(StrState::new(&cstr)).unwrap();
            assert_eq!("", s.input);
            assert_eq!(c, n);
        }
        {
            let (s, n) = map(repeat1(whitespace), |x| x.iter().collect::<String>())(StrState::new(
                "\n\n  \t\t",
            ))
            .unwrap();
            assert_eq!("", s.input);
            assert_eq!("\n\n  \t\t", n);
        }
        {
            let (s, n) = map(repeat1(whitespace), |x| x.iter().collect::<String>())(StrState::new(
                "   \nabc\n\n",
            ))
            .unwrap();
            assert_eq!("abc\n\n", s.input);
            assert_eq!("   \n", n);
        }
    }

    #[test]
    fn test_not_char() {
        let (s, n) =
            map(repeat1(not_char("x")), |x| x.iter().collect::<String>())(StrState::new("foox"))
                .unwrap();
        assert_eq!("x", s.input);
        assert_eq!("foo", n);
    }
}
