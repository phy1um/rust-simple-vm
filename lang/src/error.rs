use std::fmt;

#[derive(Debug, Clone)]
pub struct ParseError {
    context: String,
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
    pub fn new(ctx: &str, kind: ParseErrorKind) -> Self {
        Self {
            context: ctx.to_owned(),
            kind,
            tag: None,
        }
    }

    pub fn tag(mut self, s: &str) -> Self {
        self.tag = Some(s.to_owned());
        self
    }

    pub fn from_errs(ctx: &str, errs: Vec<ParseError>) -> Self {
        Self {
            context: ctx.to_owned(),
            kind: ParseErrorKind::Errors(errs),
            tag: None,
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(tag) = &self.tag {
            write!(f, "{}:!!{}!!, @ {}", self.kind, tag, self.context)
        } else {
            write!(f, "{}, @ {}", self.kind, self.context)
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    ExpectedChar { expected: char, got: char },
    UnexpectedChar(String, char),
    ExpectedToken(String),
    CharFailedPredicate(char, String),
    ExpectedType,
    ExpectedBinop,
    ExpectedExpression,
    ExpectedExpressionLHS,
    ExpectedStatement,
    ExpectedTopLevel,
    EndOfInput,
    Errors(Vec<ParseError>),
    Numeric(std::num::ParseIntError),
}

impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ExpectedChar { expected, got } => write!(f, "expected '{expected}', got '{got}'"),
            Self::UnexpectedChar(s, c) => write!(f, "char '{c}' not in \"{s}\""),
            Self::ExpectedToken(s) => write!(f, "expected token \"{s}\""),
            Self::CharFailedPredicate(c, name) => {
                write!(f, "char '{c}' failed predicate \"{name}\"")
            }
            Self::ExpectedType => write!(f, "expected type (eg int, void)"),
            Self::ExpectedBinop => write!(f, "expected binary operator (eg +, *, >=)"),
            Self::ExpectedExpression => write!(f, "expected expression"),
            Self::ExpectedExpressionLHS => write!(f, "expected expression (lhs)"),
            Self::ExpectedStatement => write!(f, "expected statement"),
            Self::ExpectedTopLevel => write!(f, "expected function definition etc."),
            Self::EndOfInput => write!(f, "end of input"),
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
