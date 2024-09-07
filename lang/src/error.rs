use crate::character::StrState;
use crate::language::lex::{LexError, LexedToken};
use crate::language::tokenize::Token;
use crate::language::State;
use std::fmt;

#[derive(Debug, Clone)]
pub struct ParseError {
    line_number: usize,
    position_in_line: usize,
    tag: Option<String>,
    kind: ParseErrorKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Confidence {
    Low,
    Medium,
    High,
}

#[derive(Debug, Clone)]
pub struct ConfidenceError<T: Clone> {
    t: T,
    confidence: Confidence,
}

#[allow(dead_code)]
impl<T: Clone> ConfidenceError<T> {
    pub fn from(t: T, confidence: Confidence) -> Self {
        Self { t, confidence }
    }

    pub fn low(t: T) -> Self {
        Self::from(t, Confidence::Low)
    }

    pub fn medium(t: T) -> Self {
        Self::from(t, Confidence::Medium)
    }

    pub fn high(t: T) -> Self {
        Self::from(t, Confidence::High)
    }

    pub fn into_low(self) -> Self {
        Self {
            t: self.t,
            confidence: Confidence::Low,
        }
    }

    pub fn into_medium(self) -> Self {
        Self {
            t: self.t,
            confidence: Confidence::Medium,
        }
    }

    pub fn into_high(self) -> Self {
        Self {
            t: self.t,
            confidence: Confidence::High,
        }
    }

    pub fn take(self) -> T {
        self.t
    }

    pub fn elevate(self) -> Self {
        match self.confidence {
            Confidence::Low => Self {
                confidence: Confidence::Medium,
                t: self.t,
            },
            Confidence::Medium => Self {
                confidence: Confidence::High,
                t: self.t,
            },
            Confidence::High => Self {
                confidence: Confidence::High,
                t: self.t,
            },
        }
    }

    pub fn select(options: &[Self]) -> Self {
        options
            .iter()
            .fold(options.first().unwrap(), |a, b| {
                if a.confidence > b.confidence {
                    a
                } else {
                    b
                }
            })
            .clone()
    }
}

impl ParseError {
    pub fn from_str_state(s: &StrState<'_>, kind: ParseErrorKind) -> Self {
        Self {
            line_number: s.line_number,
            position_in_line: s.position_in_line,
            tag: None,
            kind,
        }
    }

    pub(crate) fn from_state(s: &State<'_>, kind: ParseErrorKind) -> Self {
        if let Some(head) = s.first() {
            Self {
                line_number: head.line_number,
                position_in_line: head.position_in_line,
                tag: None,
                kind,
            }
        } else {
            Self::end_of_input()
        }
    }

    pub fn from_token(t: &LexedToken, kind: ParseErrorKind) -> Self {
        Self {
            line_number: t.line_number,
            position_in_line: t.position_in_line,
            tag: None,
            kind,
        }
    }

    pub(crate) fn from_token_prelex(t: &Token, kind: ParseErrorKind) -> Self {
        Self {
            line_number: t.line_number,
            position_in_line: t.position_in_line,
            tag: None,
            kind,
        }
    }

    pub fn end_of_input() -> Self {
        Self {
            line_number: 0,
            position_in_line: 0,
            tag: Some("EOF".to_string()),
            kind: ParseErrorKind::EndOfInput,
        }
    }

    pub fn tag(mut self, s: &str) -> Self {
        self.tag = Some(s.to_owned());
        self
    }

    pub fn from_errs(errs: Vec<ParseError>) -> Self {
        if let Some(err) = errs.first() {
            Self {
                line_number: err.line_number,
                position_in_line: err.position_in_line,
                kind: ParseErrorKind::Errors(errs),
                tag: None,
            }
        } else {
            Self {
                line_number: 0,
                position_in_line: 0,
                kind: ParseErrorKind::Errors(errs),
                tag: None,
            }
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(tag) = &self.tag {
            write!(
                f,
                "@{}:{} {}:!!{}!!",
                self.line_number, self.position_in_line, self.kind, tag
            )
        } else {
            write!(
                f,
                "@{}:{} {}",
                self.line_number, self.position_in_line, self.kind
            )
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    ExpectedToken(String),
    ExpectedType,
    ExpectedIdentifier,
    ExpectedBinop,
    ExpectedExpression,
    ExpectedExpressionLHS,
    ExpectedStatement,
    ExpectedTopLevel,
    ExpectedArrayDeref,
    EndOfInput,
    ExpectedInt,
    ExpectedChar,
    ExpectedString,
    LexError(LexError),
    InputTail,
    Errors(Vec<ParseError>),
    Numeric(std::num::ParseIntError),
}

impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ExpectedChar => write!(f, "expected char"),
            Self::ExpectedToken(s) => write!(f, "expected token \"{s}\""),
            Self::ExpectedType => write!(f, "expected type (eg int, void)"),
            Self::ExpectedBinop => write!(f, "expected binary operator (eg +, *, >=)"),
            Self::ExpectedExpression => write!(f, "expected expression"),
            Self::ExpectedExpressionLHS => write!(f, "expected expression (lhs)"),
            Self::ExpectedStatement => write!(f, "expected statement"),
            Self::ExpectedTopLevel => write!(f, "expected function definition etc."),
            Self::ExpectedArrayDeref => write!(f, "expected array deref"),
            Self::ExpectedIdentifier => write!(f, "expected identifier"),
            Self::ExpectedInt => write!(f, "expected integer"),
            Self::ExpectedString => write!(f, "expected string"),
            Self::EndOfInput => write!(f, "end of input"),
            Self::LexError(e) => write!(f, "lex error: {e:?}"),
            Self::InputTail => write!(f, "unexpected tail at end of input"),
            Self::Numeric(e) => write!(f, "invalid number: {e:?}"),
            Self::Errors(v) => write!(
                f,
                "[\n## {}\n]",
                v.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" \n## ")
            ),
        }
    }
}
