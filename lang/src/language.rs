use crate::ast;
use crate::character::{
    alpha, alphanumeric, char_predicate_or, is_char, not_char, numeric, whitespace,
};
use crate::combinator::*;
use crate::error::{Confidence, ConfidenceError, ParseError};
use crate::parse::Parser;

use std::fmt;

mod expression;
mod statement;

use statement::*;

pub(crate) type CResult<S, T> = Result<(S, T), ConfidenceError<ParseError>>;
pub(crate) type PResult<S, T> = Result<(S, T), ParseError>;

#[derive(Copy, Clone, Debug)]
pub(crate) struct State<'a> {
    input: &'a str,
    expr_precedence: u32,
}

impl<'a> fmt::Display for State<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.input)
    }
}

impl<'a> State<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            expr_precedence: 0,
        }
    }

    fn run<T, E>(self, p: impl Parser<&'a str, T, E>) -> Result<(Self, T), E> {
        match p.run(self.input) {
            Ok((st, res)) => Ok((
                Self {
                    input: st,
                    expr_precedence: self.expr_precedence,
                },
                res,
            )),
            Err(e) => Err(e),
        }
    }
}

pub(crate) fn lift_to_state<'a, T, E, F>(p: F) -> impl Fn(State<'a>) -> Result<(State<'a>, T), E>
where
    F: Parser<&'a str, T, E>,
{
    move |state| match p.run(state.input) {
        Ok((st, res)) => Ok((
            State {
                input: st,
                expr_precedence: state.expr_precedence,
            },
            res,
        )),
        Err(e) => Err(e),
    }
}

pub(crate) fn token<'a>(t: &'a str) -> impl Fn(State) -> CResult<State, &'a str> {
    move |state| state.run(with_confidence(crate::character::token(t), Confidence::Low))
}

pub(crate) fn with_confidence<S, T, E: Clone>(
    p: impl Parser<S, T, E>,
    conf: Confidence,
) -> impl Fn(S) -> Result<(S, T), ConfidenceError<E>> {
    move |input| p.run(input).map_err(|e| ConfidenceError::from(e, conf))
}

pub(crate) fn parse_type_raw(input: State) -> CResult<State, ast::Type> {
    let (s0, res) = input.run(with_confidence(
        map(repeat1(alpha), |x| x.iter().collect::<String>()),
        Confidence::Low,
    ))?;
    if res == "struct" {
        let (s1, _) = skip_whitespace(token("{"))(s0)?;
        let (s2, fields) = allow_empty(delimited(named_arg, skip_whitespace(token(","))))(s1)?;
        let (s3, _) = skip_whitespace(token("}"))(s2)?;
        Ok((s3, ast::Type::Struct(fields)))
    } else {
        Ok((
            s0,
            match res.as_ref() {
                "void" => ast::Type::Void,
                "int" => ast::Type::Int,
                "char" => ast::Type::Char,
                s => ast::Type::User(s.to_string()),
            },
        ))
    }
}

pub(crate) fn parse_type(input: State) -> CResult<State, ast::Type> {
    match skip_whitespace(token("*"))(input) {
        Ok((s, _)) => {
            let (sn, inner) = parse_type(s)?;
            Ok((sn, ast::Type::Pointer(Box::new(inner))))
        }
        Err(_) => parse_type_raw(input),
    }
}

pub(crate) fn identifier(input: State) -> Result<(State, ast::Identifier), ParseError> {
    let (s0, fst) = input.run(char_predicate_or(alpha, is_char('_')))?;
    let (s1, rest) = s0.run(repeat0(char_predicate_or(alphanumeric, is_char('_'))))?;
    let id_str = fst.to_string() + &rest.iter().collect::<String>();
    Ok((s1, ast::Identifier::new(&id_str)))
}

pub(crate) fn dotted_field(input: State) -> CResult<State, ast::Identifier> {
    let (s0, _) = token(".")(input)?;
    with_confidence(identifier, Confidence::Low)(s0)
}

fn skip_whitespace<'a, T, F>(f: F) -> impl Fn(State<'a>) -> CResult<State<'a>, T>
where
    F: Parser<State<'a>, T, ConfidenceError<ParseError>>,
{
    move |input| {
        let (s0, _) = input
            .run(discard(repeat0(whitespace)))
            .map_err(ConfidenceError::low)?;
        f.run(s0)
    }
}

fn named_arg(input: State) -> CResult<State, (ast::Identifier, ast::Type)> {
    let (s0, ty) = skip_whitespace(parse_type)(input)?;
    let (s1, name) = skip_whitespace(with_confidence(identifier, Confidence::Low))(s0)?;
    Ok((s1, (name, ty)))
}

fn function_body(input: State) -> CResult<State, Vec<ast::Statement>> {
    let (mut head, _) = skip_whitespace(token("{"))(input)?;
    let mut out = Vec::new();
    loop {
        match skip_whitespace(token("}"))(head) {
            Ok((sx, _)) => return Ok((sx, out)),
            Err(_) => match skip_whitespace(statement)(head) {
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
    let (s0, _) = skip_whitespace(token("type"))(input)?;
    let (s1, name) = skip_whitespace(with_confidence(identifier, Confidence::Medium))(s0)?;
    let (s2, _) = skip_whitespace(token(":="))(s1)?;
    let (s3, alias) = skip_whitespace(parse_type)(s2)?;
    let (s4, _) = skip_whitespace(token(";"))(s3)?;
    Ok((s4, ast::TopLevel::TypeDefinition { name, alias }))
}

fn function_definition(input: State) -> CResult<State, ast::TopLevel> {
    let (s0, return_type) = skip_whitespace(parse_type)(input)?;
    let (s1, name) = skip_whitespace(with_confidence(identifier, Confidence::Low))(s0)?;
    let (s2, _) = skip_whitespace(token("("))(s1)?;
    let (s3, args) = allow_empty(delimited(named_arg, skip_whitespace(token(","))))(s2)?;
    let (s4, _) = skip_whitespace(token(")"))(s3)?;
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

fn inline_asm(input: State) -> CResult<State, ast::TopLevel> {
    let (s0, _) = skip_whitespace(token("asm!"))(input)?;
    let (s1, name) = skip_whitespace(with_confidence(identifier, Confidence::Low))(s0)?;
    let (s2, _) = skip_whitespace(token("("))(s1)?;
    let (s3, args) = allow_empty(delimited(named_arg, skip_whitespace(token(","))))(s2)?;
    let (s4, _) = skip_whitespace(token(")"))(s3)?;
    let (s5, body) = wrapped(
        skip_whitespace(token("{")),
        lift_to_state(with_confidence(repeat1(not_char("{}")), Confidence::Medium)),
        skip_whitespace(token("}")),
    )(s4)?;
    Ok((
        s5,
        ast::TopLevel::InlineAsm {
            name,
            body: body.iter().collect::<String>(),
            args,
        },
    ))
}

fn global_variable(input: State) -> CResult<State, ast::TopLevel> {
    let (s0, _) = skip_whitespace(token("global"))(input)?;
    let (s1, var_type) = skip_whitespace(parse_type)(s0)?;
    let (s2, name) = skip_whitespace(with_confidence(identifier, Confidence::High))(s1)?;
    let (s3, _) = skip_whitespace(token(";"))(s2)?;
    Ok((s3, ast::TopLevel::GlobalVariable { name, var_type }))
}

pub fn parse_ast(input: &str) -> PResult<&str, Vec<ast::TopLevel>> {
    let mut out = Vec::new();
    let mut current_state = State::new(input);
    loop {
        let (state_next, _) = current_state.run(discard(repeat0(whitespace)))?;
        if state_next.input.is_empty() {
            return Ok((state_next.input, out));
        } else {
            let (snn, res) = AnyCollectErr::new(vec![
                inline_asm,
                global_variable,
                function_definition,
                type_definition,
            ])
            .run(state_next)
            .map_err(|es| ConfidenceError::select(&es).take())?;
            out.push(res);
            current_state = snn;
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn run_parser<'a, T, E>(p: impl Parser<State<'a>, T, E>, s: &'a str) -> Result<T, E> {
        crate::parse::run_parser(p, State::new(s))
    }

    #[test]
    fn test_function() {
        let expected = "int foo() {\nlet int a := 7;\na := 99;\n}\n";
        assert_eq!(
            expected,
            run_parser(function_definition, expected)
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
                run_parser(inline_asm, expected).unwrap().to_string()
            );
        }
    }

    #[test]
    fn test_global() {
        {
            let expected = "global char foo;\n";
            assert_eq!(
                expected,
                run_parser(global_variable, expected).unwrap().to_string()
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
                run_parser(type_definition, expected).unwrap().to_string()
            );
        }
    }

    #[test]
    fn test_identifier() {
        {
            let expected = "__foo_bar";
            assert_eq!(
                expected,
                run_parser(identifier, expected).unwrap().to_string(),
            );
        }
        {
            let expected = "Aa123___foo";
            assert_eq!(
                expected,
                run_parser(identifier, expected).unwrap().to_string(),
            );
        }
        {
            let res = run_parser(identifier, "1abcde");
            assert!(matches!(res, Err(_)));
        }
    }
}
