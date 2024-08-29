use crate::ast;
use crate::character::{
    alpha, alphanumeric, char_predicate_or, is_char, not_char, numeric, whitespace,
};
use crate::combinator::*;
use crate::error::{Confidence, ConfidenceError, ParseError, ParseErrorKind};
use crate::parse::Parser;

use std::fmt;
use std::str::FromStr;

type CResult<S, T> = Result<(S, T), ConfidenceError<ParseError>>;
type PResult<S, T> = Result<(S, T), ParseError>;

#[derive(Copy, Clone, Debug)]
struct State<'a> {
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

fn lift_to_state<'a, T, E, F>(p: F) -> impl Fn(State<'a>) -> Result<(State<'a>, T), E>
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

fn token<'a>(t: &'a str) -> impl Fn(State) -> CResult<State, &'a str> {
    move |state| state.run(with_confidence(crate::character::token(t), Confidence::Low))
}

fn with_confidence<S, T, E: Clone>(
    p: impl Parser<S, T, E>,
    conf: Confidence,
) -> impl Fn(S) -> Result<(S, T), ConfidenceError<E>> {
    move |input| p.run(input).map_err(|e| ConfidenceError::from(e, conf))
}

fn parse_type_raw(input: State) -> CResult<State, ast::Type> {
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

fn parse_type(input: State) -> CResult<State, ast::Type> {
    match skip_whitespace(token("*"))(input) {
        Ok((s, _)) => {
            let (sn, inner) = parse_type(s)?;
            Ok((sn, ast::Type::Pointer(Box::new(inner))))
        }
        Err(_) => parse_type_raw(input),
    }
}

fn identifier(input: State) -> Result<(State, ast::Identifier), ParseError> {
    let (s0, fst) = input.run(char_predicate_or(alpha, is_char('_')))?;
    let (s1, rest) = s0.run(repeat0(char_predicate_or(alphanumeric, is_char('_'))))?;
    let id_str = fst.to_string() + &rest.iter().collect::<String>();
    Ok((s1, ast::Identifier::new(&id_str)))
}

fn expression_literal_int_base10(input: State) -> CResult<State, ast::Expression> {
    input
        .run(map(repeat1(numeric), |x| {
            ast::Expression::LiteralInt(x.iter().collect::<String>().parse::<i32>().unwrap())
        }))
        .map_err(|v| ConfidenceError::from(v, Confidence::Low))
}

fn expression_literal_int_base16(input: State) -> CResult<State, ast::Expression> {
    let (s0, _) = token("0x")(input)?;
    s0.run(map_err(repeat1(alphanumeric), |x| {
        let num_str = x.iter().collect::<String>();
        match i32::from_str_radix(&num_str, 16) {
            Ok(num) => Ok(ast::Expression::LiteralInt(num)),
            Err(e) => Err(ParseError::new(input.input, ParseErrorKind::Numeric(e))),
        }
    }))
    .map_err(|v| ConfidenceError::from(v, Confidence::Low))
}

fn expression_literal_char(input: State) -> CResult<State, ast::Expression> {
    map(
        wrapped(
            token("'"),
            lift_to_state(with_confidence(not_char("'"), Confidence::Low)),
            token("'"),
        ),
        ast::Expression::LiteralChar,
    )(input)
}

fn expression_literal_string(input: State) -> CResult<State, ast::Expression> {
    map(
        wrapped(
            token("\""),
            lift_to_state(with_confidence(repeat0(not_char("\"")), Confidence::Medium)),
            token("\""),
        ),
        |x| ast::Expression::LiteralString(x.into_iter().collect()),
    )(input)
}

fn expression_variable(input: State) -> CResult<State, ast::Expression> {
    let (s0, id) = skip_whitespace(with_confidence(identifier, Confidence::Low))(input)?;
    Ok((s0, ast::Expression::Variable(vec![id])))
}

fn expression_address_of(input: State) -> CResult<State, ast::Expression> {
    let (s0, _) = skip_whitespace(token("&"))(input)?;
    let (s1, name) = identifier(s0).map_err(|v| ConfidenceError::from(v, Confidence::Medium))?;
    let (s2, mut fields) = repeat0(dotted_field)(s1)?;
    fields.insert(0, name);
    Ok((s2, ast::Expression::AddressOf(fields)))
}

fn reset_prec<'a, T, E>(
    p: impl Parser<State<'a>, T, E>,
) -> impl Fn(State<'a>) -> Result<(State<'a>, T), E> {
    move |mut state| {
        state.expr_precedence = 0;
        p.run(state)
    }
}

fn expression_call(input: State) -> CResult<State, ast::Expression> {
    let (s0, id) = skip_whitespace(with_confidence(identifier, Confidence::Low))(input)?;
    let (s1, args) = wrapped(
        token("("),
        allow_empty(delimited(
            reset_prec(skip_whitespace(expression)),
            skip_whitespace(token(",")),
        )),
        token(")"),
    )(s0)?;
    Ok((s1, ast::Expression::FunctionCall(id, args)))
}

fn expression_deref(input: State) -> CResult<State, ast::Expression> {
    let (s0, _) = skip_whitespace(token("*"))(input)?;
    let (sn, expr) = expression(s0)?;
    Ok((sn, ast::Expression::Deref(Box::new(expr))))
}

fn expression_bracketed(input: State) -> CResult<State, ast::Expression> {
    let (mut s, res) = map(
        skip_whitespace(wrapped(token("("), expression, token(")"))),
        |x| ast::Expression::Bracketed(Box::new(x)),
    )(input)?;
    s.expr_precedence = 0;
    Ok((s, res))
}

fn expression_builtin_sizeof(input: State) -> CResult<State, ast::Expression> {
    let (s0, _) = skip_whitespace(token("sizeof"))(input)?;
    let (s1, tt) = skip_whitespace(wrapped(token("("), parse_type, token(")")))(s0)?;
    Ok((s1, ast::Expression::BuiltinSizeof(tt)))
}

fn binop(input: State) -> CResult<State, ast::BinOp> {
    map(
        require(
            Any::new(vec![
                token("+"),
                token("-"),
                token("*"),
                token("%"),
                token("=="),
                token(">="),
                token(">"),
                token("<="),
                token("<"),
                token("!="),
            ]),
            ConfidenceError::low(ParseError::new(input.input, ParseErrorKind::ExpectedBinop)),
        ),
        |x| ast::BinOp::from_str(x).unwrap(),
    )
    .run(input)
}

fn expression_struct_fields(input: State) -> CResult<State, ast::Expression> {
    let (s0, id) = skip_whitespace(with_confidence(identifier, Confidence::Low))(input)?;
    let (s1, mut fields) = repeat1(dotted_field)(s0)?;
    fields.insert(0, id);
    Ok((s1, ast::Expression::Variable(fields)))
}

fn expression_part_array_index(input: State) -> CResult<State, ast::Expression> {
    let (s0, _) = skip_whitespace(token("["))(input)?;
    let (s1, index) = skip_whitespace(expression)(s0)?;
    let (s2, _) = token("]")(s1)?;
    Ok((s2, index))
}

fn precedence_climb_recursive(
    res: ast::Expression,
    input: State,
) -> CResult<State, ast::Expression> {
    let (s0, op) = skip_whitespace(binop)(input)?;
    println!("found op: {op}, prec: {}", s0.expr_precedence);
    if op.get_precedence() >= s0.expr_precedence {
        let (mut s1, rhs) = skip_whitespace(expression)(s0).map_err(ConfidenceError::elevate)?;
        s1.expr_precedence = op.get_precedence() + if op.is_left_associative() { 1 } else { 0 };
        Ok((s1, ast::Expression::BinOp(Box::new(res), Box::new(rhs), op)))
    } else {
        Ok((s0, res))
    }
}

fn expression(input: State) -> CResult<State, ast::Expression> {
    let (mut rest, mut expr) = AnyCollectErr::new(vec![
        expression_literal_int_base16,
        expression_literal_int_base10,
        expression_literal_char,
        expression_literal_string,
        expression_struct_fields,
        expression_builtin_sizeof,
        expression_call,
        expression_address_of,
        expression_variable,
        expression_bracketed,
        expression_deref,
    ])
    .run(input)
    .map_err(|v| ConfidenceError::select(&v))?;
    if let Ok((tail, e)) = expression_part_array_index(rest) {
        expr = ast::Expression::ArrayDeref {
            lhs: Box::new(expr),
            index: Box::new(e),
        };
        rest = tail;
    };
    if let Ok((tail, expr)) = precedence_climb_recursive(expr.clone(), rest) {
        return Ok((tail, expr));
    };
    Ok((rest, expr))
}

fn statement_variable_assign(input: State) -> CResult<State, ast::Statement> {
    let (s0, id) = identifier(input).map_err(ConfidenceError::low)?;
    let (s1, _) = skip_whitespace(token(":="))(s0)?;
    let (s2, expr) = skip_whitespace(expression)(s1).map_err(ConfidenceError::elevate)?;
    let (s3, _) = skip_whitespace(token(";"))(s2).map_err(ConfidenceError::elevate)?;
    Ok((s3, ast::Statement::Assign(id, Box::new(expr))))
}

fn statement_variable_assign_struct_fields(input: State) -> CResult<State, ast::Statement> {
    let (s0, id) = identifier(input).map_err(ConfidenceError::low)?;
    let (s1, mut fields) = repeat1(dotted_field)(s0)?;
    fields.insert(0, id);
    let (s2, _) = skip_whitespace(token(":="))(s1)?;
    let (s3, rhs) = skip_whitespace(expression)(s2).map_err(ConfidenceError::elevate)?;
    let (s4, _) = skip_whitespace(token(";"))(s3).map_err(ConfidenceError::elevate)?;
    Ok((s4, ast::Statement::AssignStructField { fields, rhs }))
}

fn dotted_field(input: State) -> CResult<State, ast::Identifier> {
    let (s0, _) = token(".")(input)?;
    with_confidence(identifier, Confidence::Low)(s0)
}

fn statement_variable_assign_deref(input: State) -> CResult<State, ast::Statement> {
    let (s0, _) = skip_whitespace(token("*"))(input)?;
    let (s1, lhs) = skip_whitespace(expression)(s0)?;
    let (mut s2, _) = skip_whitespace(token(":="))(s1)?;
    s2.expr_precedence = 0;
    let (s3, rhs) = skip_whitespace(expression)(s2).map_err(ConfidenceError::elevate)?;
    let (s4, _) = skip_whitespace(token(";"))(s3).map_err(ConfidenceError::elevate)?;
    Ok((s4, ast::Statement::AssignDeref { lhs, rhs }))
}

fn statement_array_index(input: State) -> CResult<State, ast::Statement> {
    let (s0, lhs) = skip_whitespace(expression)(input)?;
    if let ast::Expression::ArrayDeref { lhs, index } = lhs {
        let (mut s1, _) = skip_whitespace(token(":="))(s0).map_err(ConfidenceError::into_high)?;
        s1.expr_precedence = 0;
        let (s2, rhs) = skip_whitespace(expression)(s1).map_err(ConfidenceError::into_high)?;
        let (s3, _) = skip_whitespace(token(";"))(s2).map_err(ConfidenceError::into_high)?;
        Ok((
            s3,
            ast::Statement::AssignArray {
                lhs: *lhs,
                index: *index,
                rhs,
            },
        ))
    } else {
        Err(ConfidenceError::from(
            ParseError::new(input.input, ParseErrorKind::ExpectedArrayDeref),
            Confidence::Low,
        ))
    }
}

fn statement_variable_declare(input: State) -> CResult<State, ast::Statement> {
    let (s0, _) = skip_whitespace(token("let"))(input)?;
    let (s1, variable_type) = skip_whitespace(parse_type)(s0)?;
    let (s2, id) = skip_whitespace(with_confidence(identifier, Confidence::Low))(s1)?;
    let (s3, _) = skip_whitespace(token(":="))(s2)?;
    let (s4, expr) = skip_whitespace(expression)(s3).map_err(ConfidenceError::elevate)?;
    let (s5, _) = skip_whitespace(token(";"))(s4).map_err(ConfidenceError::elevate)?;
    Ok((
        s5,
        ast::Statement::Declare(id, Some(variable_type), Some(Box::new(expr))),
    ))
}

fn statement_variable_declare_novalue(input: State) -> CResult<State, ast::Statement> {
    let (s0, _) = skip_whitespace(token("let"))(input)?;
    let (s1, variable_type) = skip_whitespace(parse_type)(s0)?;
    let (s2, id) = skip_whitespace(with_confidence(identifier, Confidence::Low))(s1)?;
    let (s3, _) = skip_whitespace(token(";"))(s2).map_err(ConfidenceError::elevate)?;
    Ok((s3, ast::Statement::Declare(id, Some(variable_type), None)))
}

fn statement_variable_declare_infer(input: State) -> CResult<State, ast::Statement> {
    let (s0, _) = skip_whitespace(token("let"))(input)?;
    let (s1, id) = skip_whitespace(with_confidence(identifier, Confidence::Low))(s0)?;
    let (s2, _) = skip_whitespace(token(":="))(s1)?;
    let (s3, expr) = skip_whitespace(expression)(s2).map_err(ConfidenceError::elevate)?;
    let (s4, _) = skip_whitespace(token(";"))(s3).map_err(ConfidenceError::elevate)?;
    Ok((s4, ast::Statement::Declare(id, None, Some(Box::new(expr)))))
}

fn statement_return(input: State) -> CResult<State, ast::Statement> {
    let (s0, _) = skip_whitespace(token("return"))(input)?;
    let (s1, expr) = skip_whitespace(expression)(s0)?;
    let (s2, _) = skip_whitespace(token(";"))(s1).map_err(ConfidenceError::elevate)?;
    Ok((s2, ast::Statement::Return(expr)))
}

fn statement_if(input: State) -> CResult<State, ast::Statement> {
    let (s0, _) = skip_whitespace(token("if"))(input)?;
    let (s1, cond) = wrapped(
        skip_whitespace(token("(")),
        expression,
        skip_whitespace(token(")")),
    )(s0)
    .map_err(ConfidenceError::elevate)?;
    let (mut s2, body) = wrapped(
        skip_whitespace(token("{")),
        repeat1(skip_whitespace(statement)),
        skip_whitespace(token("}")),
    )(s1)
    .map_err(ConfidenceError::elevate)?;
    s2.expr_precedence = 0;
    let (sn, else_body) = match skip_whitespace(token("else"))(s2) {
        Ok((s3, _)) => {
            let (s4, eb) = wrapped(
                skip_whitespace(token("{")),
                repeat1(skip_whitespace(statement)),
                skip_whitespace(token("}")),
            )(s3)
            .map_err(ConfidenceError::elevate)?;
            (s4, Some(eb))
        }
        Err(_) => (s2, None),
    };
    Ok((
        sn,
        ast::Statement::If {
            cond,
            body,
            else_body,
        },
    ))
}

fn statement_while(input: State) -> CResult<State, ast::Statement> {
    let (s0, _) = skip_whitespace(token("while"))(input)?;
    let (s1, cond) = wrapped(
        skip_whitespace(token("(")),
        expression,
        skip_whitespace(token(")")),
    )(s0)
    .map_err(ConfidenceError::elevate)?;
    let (sn, body) = wrapped(
        skip_whitespace(token("{")),
        repeat1(skip_whitespace(statement)),
        skip_whitespace(token("}")),
    )(s1)
    .map_err(ConfidenceError::elevate)?;
    Ok((sn, ast::Statement::While { cond, body }))
}

fn statement_continue(input: State) -> CResult<State, ast::Statement> {
    let (s0, res) = map(skip_whitespace(token("continue")), |_| {
        ast::Statement::Continue
    })(input)?;
    let (s1, _) = skip_whitespace(token(";"))(s0).map_err(ConfidenceError::elevate)?;
    Ok((s1, res))
}

fn statement_break(input: State) -> CResult<State, ast::Statement> {
    let (s0, res) = map(skip_whitespace(token("break")), |_| ast::Statement::Break)(input)?;
    let (s1, _) = skip_whitespace(token(";"))(s0).map_err(ConfidenceError::elevate)?;
    Ok((s1, res))
}

fn statement_expression(input: State) -> CResult<State, ast::Statement> {
    let (s0, res) = map(expression, ast::Statement::Expression)(input)?;
    let (s1, _) = skip_whitespace(token(";"))(s0).map_err(ConfidenceError::elevate)?;
    Ok((s1, res))
}

fn statement(mut input: State) -> CResult<State, ast::Statement> {
    input.expr_precedence = 0;
    AnyCollectErr::new(vec![
        statement_array_index,
        statement_if,
        statement_while,
        statement_variable_assign_deref,
        statement_variable_assign_struct_fields,
        statement_variable_assign,
        statement_variable_declare_infer,
        statement_variable_declare,
        statement_variable_declare_novalue,
        statement_return,
        statement_continue,
        statement_break,
        statement_expression,
    ])
    .run(input)
    .map_err(|v| ConfidenceError::select(&v))
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
    use crate::ast::*;

    fn run_parser<'a, T, E>(p: impl Parser<State<'a>, T, E>, s: &'a str) -> Result<T, E> {
        crate::parse::run_parser(p, State::new(s))
    }

    #[test]
    fn test_expression_literal_int() {
        assert_eq!(
            Expression::LiteralInt(5),
            run_parser(expression_literal_int_base10, "5").unwrap()
        );
        assert_eq!(
            Expression::LiteralInt(99),
            run_parser(expression_literal_int_base10, "99").unwrap()
        );
        assert_eq!(
            Expression::LiteralInt(0x1234),
            run_parser(expression_literal_int_base16, "0x1234").unwrap()
        );
        assert_eq!(
            Expression::LiteralInt(0xffc0),
            run_parser(expression_literal_int_base16, "0xffc0").unwrap()
        );
    }

    #[test]
    fn test_expression_call() {
        {
            let expected = "putch('a')";
            assert_eq!(
                expected,
                run_parser(expression_call, &expected.to_string())
                    .unwrap()
                    .to_string()
            );
        }
        {
            let expected = "foo(1, 2, 3)";
            assert_eq!(
                expected,
                run_parser(expression_call, &expected.to_string())
                    .unwrap()
                    .to_string()
            );
        }
    }

    #[test]
    fn test_assignment() {
        let expected =
            Statement::Assign(Identifier::new("foo"), Box::new(Expression::LiteralInt(88)));
        assert_eq!(
            expected,
            run_parser(statement_variable_assign, "foo    :=       \n 88;").unwrap()
        );
    }

    #[test]
    fn test_assign_deref() {
        {
            let expected = "*(x + 1) := 57;";
            assert_eq!(
                expected,
                run_parser(statement_variable_assign_deref, expected)
                    .unwrap()
                    .to_string()
            );
        }
        {
            let expected = "*x := 3 * y + foo(7);";
            assert_eq!(
                expected,
                run_parser(statement_variable_assign_deref, expected)
                    .unwrap()
                    .to_string()
            );
        }
    }

    #[test]
    fn test_expr_deref() {
        let expected = "*foobar";
        assert_eq!(
            expected,
            run_parser(expression_deref, expected).unwrap().to_string()
        );
    }

    #[test]
    fn test_declare() {
        {
            let expected = Statement::Declare(
                Identifier::new("bar"),
                Some(Type::Char),
                Some(Box::new(Expression::LiteralChar('a'))),
            );
            assert_eq!(
                expected,
                run_parser(statement_variable_declare, "let char bar := 'a';").unwrap()
            );
        }
        {
            let expected = "let Foo x;";
            assert_eq!(
                expected,
                run_parser(statement_variable_declare_novalue, expected)
                    .unwrap()
                    .to_string()
            );
        }
        {
            let expected = "let Foo x;";
            assert_eq!(
                expected,
                run_parser(statement, expected).unwrap().to_string()
            );
        }
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
    fn test_if() {
        {
            let expected = "if (a > 0) {\nfoo := bar;\nreturn 5;\n}\n";
            assert_eq!(
                expected,
                run_parser(statement_if, expected).unwrap().to_string()
            );
        }
        {
            let expected = "if (a) {\nfoo := bar;\n} else {\nfoo := baz;\n}\n";
            assert_eq!(
                expected,
                run_parser(statement_if, expected).unwrap().to_string()
            );
        }
        {
            let expected = "if (i <= 1) {\nfoo := bar;\nreturn 5;\n}\n";
            assert_eq!(
                expected,
                run_parser(statement_if, expected).unwrap().to_string()
            );
        }
    }

    #[test]
    fn test_while() {
        {
            let expected = "while (l < 0) {\nfoo := bar;\nreturn foo;\n}\n";
            assert_eq!(
                expected,
                run_parser(statement_while, expected).unwrap().to_string()
            );
        }
        {
            let expected = "while (a <= 10) {\na := a + 1;\n}\n";
            assert_eq!(
                expected,
                run_parser(statement_while, expected).unwrap().to_string()
            );
        }
    }

    #[test]
    fn test_conditions() {
        {
            let expected = "i <= 1";
            assert_eq!(
                expected,
                run_parser(expression, expected).unwrap().to_string()
            );
        }
        {
            let expected = "foo < 79";
            assert_eq!(
                expected,
                run_parser(expression, expected).unwrap().to_string()
            );
        }
        {
            let expected = "xyzz > 9";
            assert_eq!(
                expected,
                run_parser(expression, expected).unwrap().to_string()
            );
        }
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
    fn test_expr_brackets() {
        {
            let expected = "(5 * x) + (3 * y)";
            assert_eq!(
                expected,
                run_parser(expression, expected).unwrap().to_string()
            );
        }
        {
            let expected = "(3 * foo(x + (y + (z * (3 + bar()))))) + 18";
            assert_eq!(
                expected,
                run_parser(expression, expected).unwrap().to_string()
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
    fn test_struct_fields() {
        {
            let expected = "foo.bar.baz.xyz";
            assert_eq!(
                expected,
                run_parser(expression_struct_fields, expected)
                    .unwrap()
                    .to_string()
            );
        }
        {
            let expected = "foo.bar.baz.xyz";
            assert_eq!(
                expected,
                run_parser(expression, expected).unwrap().to_string()
            );
        }
        {
            let expected = "bar.baz.xyz + foo.x.y";
            assert_eq!(
                expected,
                run_parser(expression, expected).unwrap().to_string()
            );
        }
    }

    #[test]
    fn test_array_index() {
        {
            let expected = "a[5]";
            assert_eq!(
                expected,
                run_parser(expression, expected).unwrap().to_string(),
            );
        }
        {
            let expected = "(foo.bar + 3)[6]";
            assert_eq!(
                expected,
                run_parser(expression, expected).unwrap().to_string(),
            );
        }
        {
            let expected = "a[5] := 7;";
            assert_eq!(
                expected,
                run_parser(statement, expected).unwrap().to_string(),
            );
        }
        {
            let expected = "(foo.bar + 3)[6] := baz;";
            assert_eq!(
                expected,
                run_parser(statement, expected).unwrap().to_string(),
            );
        }
        {
            let expected = "(a + 3)[(b + 6)[c + 9]] := (foo.bar[16] + 12)[18 + foo()];";
            assert_eq!(
                expected,
                run_parser(statement, expected).unwrap().to_string(),
            );
        }
    }

    #[test]
    fn sizeof() {
        {
            let expected = "sizeof(int)";
            assert_eq!(
                expected,
                run_parser(expression_builtin_sizeof, expected)
                    .unwrap()
                    .to_string(),
            );
        }
        {
            let expected = "sizeof(FooBar)";
            assert_eq!(
                expected,
                run_parser(expression_builtin_sizeof, expected)
                    .unwrap()
                    .to_string(),
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

    #[test]
    fn test_string_literal() {
        {
            let expected = "\"foo bar baz\"";
            assert_eq!(
                expected,
                run_parser(expression, expected).unwrap().to_string(),
            );
        }
    }

    #[test]
    fn test_binops() {
        {
            let expected = "10 + 101 + a";
            assert_eq!(
                expected,
                run_parser(expression, expected).unwrap().to_string(),
            );
        }
        {
            let expected = "12 + foo.bar - a[12] + foo() + foo(bar())";
            assert_eq!(
                expected,
                run_parser(expression, expected).unwrap().to_string(),
            );
        }
    }
}
