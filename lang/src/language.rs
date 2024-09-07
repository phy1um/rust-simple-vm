use crate::ast;
use crate::combinator::*;
use crate::error::{Confidence, ConfidenceError, ParseError, ParseErrorKind};
use crate::parse::Parser;

use std::fmt;

mod expression;
pub mod lex;
mod statement;
pub mod tokenize;

use lex::{LexedToken, LexedTokenKind};
use statement::*;
use tokenize::tokens;

pub(crate) type CResult<S, T> = Result<(S, T), ConfidenceError<ParseError>>;

#[derive(Copy, Clone, Debug)]
pub(crate) struct State<'a> {
    input: &'a [LexedToken],
    expr_precedence: u32,
}

impl<'a> fmt::Display for State<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.input)
    }
}

impl<'a> State<'a> {
    fn new(input: &'a [LexedToken]) -> Self {
        Self {
            input,
            expr_precedence: 0,
        }
    }

    pub(crate) fn succ(self) -> Self {
        Self {
            input: &self.input[1..self.input.len()],
            expr_precedence: self.expr_precedence,
        }
    }

    pub(crate) fn first(&self) -> Option<&LexedToken> {
        self.input.first()
    }
}

pub(crate) fn symbol<'a>(t: &'a str) -> impl Fn(State<'a>) -> CResult<State<'a>, &'a str> {
    move |state| {
        if let Some(head) = state.first() {
            if let LexedTokenKind::Symbol(s) = &head.value {
                if s == t {
                    return Ok((state.succ(), t));
                };
            };
            Err(ConfidenceError::low(ParseError::from_token(
                head,
                ParseErrorKind::ExpectedToken(t.to_string()),
            )))
        } else {
            Err(ConfidenceError::low(ParseError::end_of_input()))
        }
    }
}

pub(crate) fn name<'a>(t: &'a str) -> impl Fn(State) -> CResult<State, &'a str> {
    move |state| {
        if let Some(head) = state.first() {
            if let LexedTokenKind::Identifier(s) = &head.value {
                if s == t {
                    return Ok((state.succ(), t));
                }
            };
            Err(ConfidenceError::low(ParseError::from_token(
                head,
                ParseErrorKind::ExpectedToken(t.to_string()),
            )))
        } else {
            Err(ConfidenceError::low(ParseError::end_of_input()))
        }
    }
}

pub(crate) fn identifier(state: State) -> CResult<State, ast::Identifier> {
    if let Some(head) = state.input.first() {
        if let LexedTokenKind::Identifier(s) = &head.value {
            Ok((state.succ(), ast::Identifier(s.to_string())))
        } else {
            Err(ConfidenceError::low(ParseError::from_token(
                head,
                ParseErrorKind::ExpectedIdentifier,
            )))
        }
    } else {
        Err(ConfidenceError::low(ParseError::end_of_input()))
    }
}

fn parse_type_raw(input: State) -> CResult<State, ast::Type> {
    let (s0, res) = identifier(input)?;
    if res.0 == "struct" {
        let (s1, _) = symbol("{")(s0)?;
        let (s2, fields) = allow_empty(delimited(named_arg, symbol(",")))(s1)?;
        let (s3, _) = symbol("}")(s2)?;
        Ok((s3, ast::Type::Struct(fields)))
    } else {
        Ok((
            s0,
            match res.0.as_ref() {
                "void" => ast::Type::Void,
                "int" => ast::Type::Int,
                "char" => ast::Type::Char,
                s => ast::Type::User(s.to_string()),
            },
        ))
    }
}

pub(crate) fn parse_type(input: State) -> CResult<State, ast::Type> {
    match symbol("*")(input) {
        Ok((s, _)) => {
            let (sn, inner) = parse_type(s)?;
            Ok((sn, ast::Type::Pointer(Box::new(inner))))
        }
        Err(_) => parse_type_raw(input),
    }
}

pub(crate) fn dotted_field(input: State) -> CResult<State, ast::Identifier> {
    let (s0, _) = symbol(".")(input)?;
    identifier(s0)
}

fn named_arg(input: State) -> CResult<State, (ast::Identifier, ast::Type)> {
    let (s0, ty) = parse_type(input)?;
    let (s1, name) = identifier(s0)?;
    Ok((s1, (name, ty)))
}

fn function_body(input: State) -> CResult<State, Vec<ast::Statement>> {
    let (mut head, _) = symbol("{")(input)?;
    let mut out = Vec::new();
    loop {
        match symbol("}")(head) {
            Ok((sx, _)) => return Ok((sx, out)),
            Err(_) => match statement(head) {
                Ok((sx, st)) => {
                    out.push(st);
                    head = sx;
                }
                Err(e) => return Err(e),
            },
        }
    }
}

fn type_definition(input: State) -> CResult<State, ast::TopLevel> {
    let (s0, _) = name("type")(input)?;
    let (s1, name) = identifier(s0)?;
    let (s2, _) = symbol(":=")(s1)?;
    let (s3, alias) = parse_type(s2)?;
    let (s4, _) = symbol(";")(s3)?;
    Ok((s4, ast::TopLevel::TypeDefinition { name, alias }))
}

fn function_definition(input: State) -> CResult<State, ast::TopLevel> {
    let (s0, return_type) = parse_type(input)?;
    let (s1, name) = identifier(s0)?;
    let (s2, _) = symbol("(")(s1)?;
    let (s3, args) = allow_empty(delimited(named_arg, symbol(",")))(s2)?;
    let (s4, _) = symbol(")")(s3)?;
    let (s5, body) = function_body(s4)?;
    Ok((
        s5,
        ast::TopLevel::FunctionDefinition {
            name,
            return_type,
            body,
            args,
        },
    ))
}

fn un_tokenize(tokens: &[LexedToken]) -> Vec<String> {
    let mut out = Vec::new();
    let (mut current_line, mut line_offset) = if let Some(token) = tokens.first() {
        (token.line_number, 1)
    } else {
        (0, 0)
    };
    let mut line_parts: Vec<String> = Vec::new();
    for token in tokens {
        if token.line_number != current_line {
            out.push(line_parts.join(""));
            line_parts = Vec::new();
            current_line = token.line_number;
            let token_str = token.to_string();
            line_offset = token.position_in_line + token_str.len();
            line_parts.push(token.to_string());
        } else {
            let space_offset = token.position_in_line - line_offset;
            line_parts.push((0..space_offset).map(|_| " ").collect::<String>());
            let token_str = token.to_string();
            line_offset = token.position_in_line + token_str.len();
            line_parts.push(token_str);
        }
    }
    if !line_parts.is_empty() {
        out.push(line_parts.join(""));
    };
    out
}

fn inline_asm(input: State) -> CResult<State, ast::TopLevel> {
    let (s0, _) = name("asm")(input)?;
    let (s01, _) = symbol("!")(s0)?;
    let (s1, name) = identifier(s01)?;
    let (s2, _) = symbol("(")(s1)?;
    let (s3, args) = allow_empty(delimited(named_arg, symbol(",")))(s2)?;
    let (s4, _) = symbol(")")(s3)?;
    let (s5, _) = symbol("{")(s4)?;
    let mut body = Vec::new();
    let mut sn = s5;
    loop {
        if let Some(token) = sn.first() {
            if let LexedTokenKind::Symbol(s) = &token.value {
                if s == "}" {
                    return Ok((
                        sn.succ(),
                        ast::TopLevel::InlineAsm {
                            name,
                            body: un_tokenize(&body).join("\n"),
                            args,
                        },
                    ));
                }
            };
            body.push(token.clone());
            sn = sn.succ();
        } else {
            return Err(ConfidenceError::high(ParseError::end_of_input()));
        }
    }
}

fn global_variable(input: State) -> CResult<State, ast::TopLevel> {
    let (s0, _) = name("global")(input)?;
    let (s1, var_type) = parse_type(s0)?;
    let (s2, name) = identifier(s1)?;
    let (s3, _) = symbol(";")(s2)?;
    Ok((s3, ast::TopLevel::GlobalVariable { name, var_type }))
}

pub fn parse_ast(input: &str) -> Result<Vec<ast::TopLevel>, ParseError> {
    let tokens = tokens(input);
    let lexed: Vec<LexedToken> = tokens
        .iter()
        .map(|x| {
            x.clone()
                .try_into()
                .map_err(|e| ParseError::from_token_prelex(x, ParseErrorKind::LexError(e)))
        })
        .collect::<Result<Vec<_>, _>>()?;

    let mut out = Vec::new();
    let mut current_state = State::new(&lexed);
    loop {
        if current_state.input.is_empty() {
            return Ok(out);
        } else {
            let (snn, res) = AnyCollectErr::new(vec![
                inline_asm,
                global_variable,
                function_definition,
                type_definition,
            ])
            .run(current_state)
            .map_err(|es| ConfidenceError::select(&es).take())?;
            out.push(res);
            current_state = snn;
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! run_parser {
        ($p:expr, $s:expr) => {
            crate::parse::run_parser(
                $p,
                State::new(&{
                    let tokens = tokens($s);
                    let lexed: Vec<LexedToken> = tokens
                        .iter()
                        .map(|x| {
                            x.clone().try_into().map_err(|e| {
                                ParseError::from_token_prelex(&x, ParseErrorKind::LexError(e))
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()
                        .unwrap();
                    println!("{lexed:?}");
                    lexed
                }),
            )
        };
    }

    #[test]
    fn test_function() {
        let expected = "int foo() {\nlet int a := 7;\na := 99;\n}\n";
        assert_eq!(
            expected,
            run_parser!(function_definition, expected)
                .unwrap()
                .to_string()
        );
    }

    #[test]
    fn test_inline_asm() {
        {
            let expected = "asm! foobar(int a, int b) {\nAdd A B A\nSub A B A\n}\n";
            assert_eq!(
                expected,
                run_parser!(inline_asm, expected).unwrap().to_string()
            );
        }
    }

    #[test]
    fn test_global() {
        {
            let expected = "global char foo;\n";
            assert_eq!(
                expected,
                run_parser!(global_variable, expected).unwrap().to_string()
            );
        }
    }

    #[test]
    fn test_struct() {
        {
            let expected = "type Foo := struct {
int x,
int y,
};
";
            assert_eq!(
                expected,
                run_parser!(type_definition, expected).unwrap().to_string()
            );
        }
    }

    #[test]
    fn test_identifier() {
        {
            let expected = "__foo_bar";
            assert_eq!(
                expected,
                run_parser!(identifier, expected).unwrap().to_string(),
            );
        }
        {
            let expected = "Aa123___foo";
            assert_eq!(
                expected,
                run_parser!(identifier, expected).unwrap().to_string(),
            );
        }
        {
            let res = run_parser!(identifier, "1abcde");
            assert!(matches!(res, Err(_)));
        }
    }
}
