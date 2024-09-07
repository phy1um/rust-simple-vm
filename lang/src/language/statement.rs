use crate::combinator::*;
use crate::language::expression::*;
use crate::language::*;

pub(crate) fn statement_variable_assign(input: State) -> CResult<State, ast::Statement> {
    let (s0, id) = identifier(input)?;
    let (s1, _) = symbol(":=")(s0)?;
    let (s2, expr) = expression(s1).map_err(ConfidenceError::elevate)?;
    let (s3, _) = symbol(";")(s2).map_err(ConfidenceError::elevate)?;
    Ok((s3, ast::Statement::Assign(id, Box::new(expr))))
}

pub(crate) fn statement_variable_assign_struct_fields(
    input: State,
) -> CResult<State, ast::Statement> {
    let (s0, id) = identifier(input)?;
    let (s1, mut fields) = repeat1(dotted_field)(s0)?;
    fields.insert(0, id);
    let (s2, _) = symbol(":=")(s1)?;
    let (s3, rhs) = expression(s2).map_err(ConfidenceError::elevate)?;
    let (s4, _) = symbol(";")(s3).map_err(ConfidenceError::elevate)?;
    Ok((s4, ast::Statement::AssignStructField { fields, rhs }))
}

pub(crate) fn statement_variable_assign_deref(input: State) -> CResult<State, ast::Statement> {
    let (s0, _) = symbol("*")(input)?;
    let (s1, lhs) = expression(s0)?;
    let (mut s2, _) = symbol(":=")(s1)?;
    s2.expr_precedence = 0;
    let (s3, rhs) = expression(s2).map_err(ConfidenceError::elevate)?;
    let (s4, _) = symbol(";")(s3).map_err(ConfidenceError::elevate)?;
    Ok((s4, ast::Statement::AssignDeref { lhs, rhs }))
}

pub(crate) fn statement_array_index(input: State) -> CResult<State, ast::Statement> {
    let (s0, lhs) = expression(input)?;
    if let ast::Expression::ArrayDeref { lhs, index } = lhs {
        let (mut s1, _) = symbol(":=")(s0).map_err(ConfidenceError::into_high)?;
        s1.expr_precedence = 0;
        let (s2, rhs) = expression(s1).map_err(ConfidenceError::into_high)?;
        let (s3, _) = symbol(";")(s2).map_err(ConfidenceError::into_high)?;
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
            ParseError::from_state(&input, ParseErrorKind::ExpectedArrayDeref),
            Confidence::Low,
        ))
    }
}

pub(crate) fn statement_variable_declare(input: State) -> CResult<State, ast::Statement> {
    let (s0, _) = name("let")(input)?;
    let (s1, variable_type) = parse_type(s0)?;
    let (s2, id) = identifier(s1)?;
    let (s3, _) = symbol(":=")(s2)?;
    let (s4, expr) = expression(s3).map_err(ConfidenceError::elevate)?;
    let (s5, _) = symbol(";")(s4).map_err(ConfidenceError::elevate)?;
    Ok((
        s5,
        ast::Statement::Declare(id, Some(variable_type), Some(Box::new(expr))),
    ))
}

pub(crate) fn statement_variable_declare_novalue(input: State) -> CResult<State, ast::Statement> {
    let (s0, _) = name("let")(input)?;
    let (s1, variable_type) = parse_type(s0)?;
    let (s2, id) = identifier(s1)?;
    let (s3, _) = symbol(";")(s2).map_err(ConfidenceError::elevate)?;
    Ok((s3, ast::Statement::Declare(id, Some(variable_type), None)))
}

pub(crate) fn statement_variable_declare_infer(input: State) -> CResult<State, ast::Statement> {
    let (s0, _) = name("let")(input)?;
    let (s1, id) = identifier(s0)?;
    let (s2, _) = symbol(":=")(s1)?;
    let (s3, expr) = expression(s2).map_err(ConfidenceError::elevate)?;
    let (s4, _) = symbol(";")(s3).map_err(ConfidenceError::elevate)?;
    Ok((s4, ast::Statement::Declare(id, None, Some(Box::new(expr)))))
}

pub(crate) fn statement_return(input: State) -> CResult<State, ast::Statement> {
    let (s0, _) = name("return")(input)?;
    let (s1, expr) = expression(s0)?;
    let (s2, _) = symbol(";")(s1).map_err(ConfidenceError::elevate)?;
    Ok((s2, ast::Statement::Return(expr)))
}

pub(crate) fn statement_if(input: State) -> CResult<State, ast::Statement> {
    let (s0, _) = name("if")(input)?;
    let (s1, cond) =
        wrapped(symbol("("), expression, symbol(")"))(s0).map_err(ConfidenceError::elevate)?;
    let (mut s2, body) = wrapped(symbol("{"), repeat1(statement), symbol("}"))(s1)
        .map_err(ConfidenceError::elevate)?;
    s2.expr_precedence = 0;
    let (sn, else_body) = match name("else")(s2) {
        Ok((s3, _)) => {
            let (s4, eb) = wrapped(symbol("{"), repeat1(statement), symbol("}"))(s3)
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

pub(crate) fn statement_while(input: State) -> CResult<State, ast::Statement> {
    let (s0, _) = name("while")(input)?;
    let (s1, cond) =
        wrapped(symbol("("), expression, symbol(")"))(s0).map_err(ConfidenceError::elevate)?;
    let (sn, body) = wrapped(symbol("{"), repeat1(statement), symbol("}"))(s1)
        .map_err(ConfidenceError::elevate)?;
    Ok((sn, ast::Statement::While { cond, body }))
}

pub(crate) fn statement_continue(input: State) -> CResult<State, ast::Statement> {
    let (s0, res) = map(name("continue"), |_| ast::Statement::Continue)(input)?;
    let (s1, _) = symbol(";")(s0).map_err(ConfidenceError::elevate)?;
    Ok((s1, res))
}

pub(crate) fn statement_break(input: State) -> CResult<State, ast::Statement> {
    let (s0, res) = map(name("break"), |_| ast::Statement::Break)(input)?;
    let (s1, _) = symbol(";")(s0).map_err(ConfidenceError::elevate)?;
    Ok((s1, res))
}

pub(crate) fn statement_expression(input: State) -> CResult<State, ast::Statement> {
    let (s0, res) = map(expression, ast::Statement::Expression)(input)?;
    let (s1, _) = symbol(";")(s0).map_err(ConfidenceError::elevate)?;
    Ok((s1, res))
}

pub(crate) fn statement(mut input: State) -> CResult<State, ast::Statement> {
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::*;

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
    fn test_assignment() {
        let expected =
            Statement::Assign(Identifier::new("foo"), Box::new(Expression::LiteralInt(88)));
        assert_eq!(
            expected,
            run_parser!(statement_variable_assign, "foo    :=       \n 88;").unwrap()
        );
    }

    #[test]
    fn test_assign_deref() {
        {
            let expected = "*(x + 1) := 57;";
            assert_eq!(
                expected,
                run_parser!(statement_variable_assign_deref, expected)
                    .unwrap()
                    .to_string()
            );
        }
        {
            let expected = "*x := 3 * y + foo(7);";
            assert_eq!(
                expected,
                run_parser!(statement_variable_assign_deref, expected)
                    .unwrap()
                    .to_string()
            );
        }
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
                run_parser!(statement_variable_declare, "let char bar := 'a';").unwrap()
            );
        }
        {
            let expected = "let Foo x;";
            assert_eq!(
                expected,
                run_parser!(statement_variable_declare_novalue, expected)
                    .unwrap()
                    .to_string()
            );
        }
        {
            let expected = "let Foo x;";
            assert_eq!(
                expected,
                run_parser!(statement, expected).unwrap().to_string()
            );
        }
    }

    #[test]
    fn test_if() {
        {
            let expected = "if (a > 0) {\nfoo := bar;\nreturn 5;\n}\n";
            assert_eq!(
                expected,
                run_parser!(statement_if, expected).unwrap().to_string()
            );
        }
        {
            let expected = "if (a) {\nfoo := bar;\n} else {\nfoo := baz;\n}\n";
            assert_eq!(
                expected,
                run_parser!(statement_if, expected).unwrap().to_string()
            );
        }
        {
            let expected = "if (i <= 1) {\nfoo := bar;\nreturn 5;\n}\n";
            assert_eq!(
                expected,
                run_parser!(statement_if, expected).unwrap().to_string()
            );
        }
    }

    #[test]
    fn test_while() {
        {
            let expected = "while (l < 0) {\nfoo := bar;\nreturn foo;\n}\n";
            assert_eq!(
                expected,
                run_parser!(statement_while, expected).unwrap().to_string()
            );
        }
        {
            let expected = "while (a <= 10) {\na := a + 1;\n}\n";
            assert_eq!(
                expected,
                run_parser!(statement_while, expected).unwrap().to_string()
            );
        }
    }
}
