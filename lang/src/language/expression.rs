use crate::language::*;

use std::str::FromStr;

pub(crate) fn expression(input: State) -> CResult<State, ast::Expression> {
    let (mut rest, mut expr) = AnyCollectErr::new(vec![
        expression_literal_int,
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
    if let Ok((tail, expr)) = precedence_climb_iterative(expr.clone(), rest) {
        return Ok((tail, expr));
    };
    Ok((rest, expr))
}

fn precedence_climb_iterative(
    res: ast::Expression,
    input: State,
) -> CResult<State, ast::Expression> {
    let mut lhs = res;
    let mut state = input;
    loop {
        if let Ok((mut s0, op)) = binop(state) {
            if op.get_precedence() >= s0.expr_precedence {
                s0.expr_precedence =
                    op.get_precedence() + if op.is_left_associative() { 1 } else { 0 };
                if let Ok((s1, rhs)) = expression(s0).map_err(ConfidenceError::elevate) {
                    lhs = ast::Expression::BinOp(Box::new(lhs), Box::new(rhs), op);
                    let old_prec = state.expr_precedence;
                    state = s1;
                    state.expr_precedence = old_prec;
                    continue;
                }
            }
        }
        return Ok((state, lhs));
    }
}

fn expression_literal_int(input: State) -> CResult<State, ast::Expression> {
    if let Some(x) = input.first() {
        if let LexedTokenKind::LiteralInt(i, _) = x.value {
            return Ok((input.succ(), ast::Expression::LiteralInt(i)));
        };
        Err(ConfidenceError::low(ParseError::from_token(
            x,
            ParseErrorKind::ExpectedInt,
        )))
    } else {
        Err(ConfidenceError::low(ParseError::end_of_input()))
    }
}

fn expression_literal_char(input: State) -> CResult<State, ast::Expression> {
    if let Some(x) = input.first() {
        if let LexedTokenKind::LiteralChar(c) = x.value {
            return Ok((input.succ(), ast::Expression::LiteralChar(c)));
        };
        Err(ConfidenceError::low(ParseError::from_token(
            x,
            ParseErrorKind::ExpectedChar,
        )))
    } else {
        Err(ConfidenceError::low(ParseError::end_of_input()))
    }
}

fn expression_literal_string(input: State) -> CResult<State, ast::Expression> {
    if let Some(x) = input.first() {
        if let LexedTokenKind::LiteralString(s) = &x.value {
            return Ok((input.succ(), ast::Expression::LiteralString(s.to_string())));
        }
        Err(ConfidenceError::low(ParseError::from_token(
            x,
            ParseErrorKind::ExpectedString,
        )))
    } else {
        Err(ConfidenceError::low(ParseError::end_of_input()))
    }
}

fn expression_variable(input: State) -> CResult<State, ast::Expression> {
    let (s0, id) = identifier(input)?;
    let (s1, mut fields) = repeat0(dotted_field)(s0)?;
    fields.insert(0, id);
    Ok((s1, ast::Expression::Variable(fields)))
}

fn expression_address_of(input: State) -> CResult<State, ast::Expression> {
    let (s0, _) = symbol("&")(input)?;
    let (s1, name) = identifier(s0)?;
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
    let (s0, id) = identifier(input)?;
    let (s1, args) = wrapped(
        symbol("("),
        allow_empty(delimited(reset_prec(expression), symbol(","))),
        symbol(")"),
    )(s0)?;
    Ok((s1, ast::Expression::FunctionCall(id, args)))
}

fn expression_deref(input: State) -> CResult<State, ast::Expression> {
    let (s0, _) = symbol("*")(input)?;
    let (sn, expr) = expression(s0)?;
    Ok((sn, ast::Expression::Deref(Box::new(expr))))
}

fn expression_bracketed(mut input: State) -> CResult<State, ast::Expression> {
    let store_prec = input.expr_precedence;
    input.expr_precedence = 0;
    let (mut s, res) = map(wrapped(symbol("("), expression, symbol(")")), |x| {
        ast::Expression::Bracketed(Box::new(x))
    })(input)?;
    // TODO: consider this
    s.expr_precedence = store_prec;
    Ok((s, res))
}

fn expression_builtin_sizeof(input: State) -> CResult<State, ast::Expression> {
    let (s0, _) = name("sizeof")(input)?;
    let (s1, tt) = wrapped(symbol("("), parse_type, symbol(")"))(s0)?;
    Ok((s1, ast::Expression::BuiltinSizeof(tt)))
}

fn expression_struct_fields(input: State) -> CResult<State, ast::Expression> {
    let (s0, id) = identifier(input)?;
    let (s1, mut fields) = repeat1(dotted_field)(s0)?;
    fields.insert(0, id);
    Ok((s1, ast::Expression::Variable(fields)))
}

fn expression_part_array_index(input: State) -> CResult<State, ast::Expression> {
    let (s0, _) = symbol("[")(input)?;
    let (s1, index) = expression(s0)?;
    let (s2, _) = symbol("]")(s1)?;
    Ok((s2, index))
}

fn binop(input: State) -> CResult<State, ast::BinOp> {
    map(
        require(
            Any::new(vec![
                symbol("+"),
                symbol("-"),
                symbol("*"),
                symbol("%"),
                symbol("=="),
                symbol(">="),
                symbol(">"),
                symbol("<="),
                symbol("<"),
                symbol("!="),
            ]),
            ConfidenceError::low(ParseError::end_of_input()),
        ),
        |x| ast::BinOp::from_str(x).unwrap(),
    )
    .run(input)
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
    fn test_expression_literal_int() {
        assert_eq!(
            Expression::LiteralInt(5),
            run_parser!(expression_literal_int, "5").unwrap()
        );
        assert_eq!(
            Expression::LiteralInt(99),
            run_parser!(expression_literal_int, "99").unwrap()
        );
        assert_eq!(
            Expression::LiteralInt(0x1234),
            run_parser!(expression_literal_int, "0x1234").unwrap()
        );
        assert_eq!(
            Expression::LiteralInt(0xffc0),
            run_parser!(expression_literal_int, "0xffc0").unwrap()
        );
    }

    #[test]
    fn test_expression_call() {
        {
            let expected = "putch('a')";
            assert_eq!(
                expected,
                run_parser!(expression_call, &expected.to_string())
                    .unwrap()
                    .to_string()
            );
        }
        {
            let expected = "foo(1, 2, 3)";
            assert_eq!(
                expected,
                run_parser!(expression_call, &expected.to_string())
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
            run_parser!(expression_deref, expected).unwrap().to_string()
        );
    }

    #[test]
    fn test_conditions() {
        {
            let expected = "i <= 1";
            assert_eq!(
                expected,
                run_parser!(expression, expected).unwrap().to_string()
            );
        }
        {
            let expected = "foo < 79";
            assert_eq!(
                expected,
                run_parser!(expression, expected).unwrap().to_string()
            );
        }
        {
            let expected = "xyzz > 9";
            assert_eq!(
                expected,
                run_parser!(expression, expected).unwrap().to_string()
            );
        }
    }

    #[test]
    fn test_expr_brackets() {
        {
            let expected = "(5 * x) + (3 * y)";
            assert_eq!(
                expected,
                run_parser!(expression, expected).unwrap().to_string()
            );
        }
        {
            let expected = "(3 * foo(x + (y + (z * (3 + bar()))))) + 18";
            assert_eq!(
                expected,
                run_parser!(expression, expected).unwrap().to_string()
            );
        }
    }

    #[test]
    fn test_struct_fields() {
        {
            let expected = "foo.bar.baz.xyz";
            assert_eq!(
                expected,
                run_parser!(expression_struct_fields, expected)
                    .unwrap()
                    .to_string()
            );
        }
        {
            let expected = "foo.bar.baz.xyz";
            assert_eq!(
                expected,
                run_parser!(expression, expected).unwrap().to_string()
            );
        }
        {
            let expected = "bar.baz.xyz + foo.x.y";
            assert_eq!(
                expected,
                run_parser!(expression, expected).unwrap().to_string()
            );
        }
    }

    #[test]
    fn test_array_index() {
        {
            let expected = "a[5]";
            println!("{expected}");
            assert_eq!(
                expected,
                run_parser!(expression, expected).unwrap().to_string(),
            );
        }
        {
            let expected = "(foo.bar + 3)[6]";
            println!("{expected}");
            assert_eq!(
                expected,
                run_parser!(expression, expected).unwrap().to_string(),
            );
        }
        {
            let expected = "a[5] := 7;";
            println!("{expected}");
            assert_eq!(
                expected,
                run_parser!(statement, expected).unwrap().to_string(),
            );
        }
        {
            let expected = "(foo.bar + 3)[6] := baz;";
            println!("{expected}");
            assert_eq!(
                expected,
                run_parser!(statement, expected).unwrap().to_string(),
            );
        }
        {
            let expected = "(a + 3)[(b + 6)[c + 9]] := (foo.bar[16] + 12)[18 + foo()];";
            println!("{expected}");
            assert_eq!(
                expected,
                run_parser!(statement, expected).unwrap().to_string(),
            );
        }
    }

    #[test]
    fn sizeof() {
        {
            let expected = "sizeof(int)";
            assert_eq!(
                expected,
                run_parser!(expression_builtin_sizeof, expected)
                    .unwrap()
                    .to_string(),
            );
        }
        {
            let expected = "sizeof(FooBar)";
            assert_eq!(
                expected,
                run_parser!(expression_builtin_sizeof, expected)
                    .unwrap()
                    .to_string(),
            );
        }
    }
    #[test]
    fn test_string_literal() {
        {
            let expected = "\"foo bar baz\"";
            assert_eq!(
                expected,
                run_parser!(expression, expected).unwrap().to_string(),
            );
        }
    }

    fn op(e0: Expression, e1: Expression, op: BinOp) -> Expression {
        Expression::BinOp(Box::new(e0), Box::new(e1), op)
    }

    fn sym(s: &str) -> Expression {
        Expression::Variable(vec![Identifier::new(s)])
    }

    #[test]
    fn test_binops() {
        {
            let expected = "10 + 101 + a";
            assert_eq!(
                expected,
                run_parser!(expression, expected).unwrap().to_string(),
            );
        }
        {
            let prec = "b * c + a";
            let expected = op(
                op(sym("b"), sym("c"), BinOp::Multiply),
                sym("a"),
                BinOp::Add,
            );
            assert_eq!(expected, run_parser!(expression, prec).unwrap(),);
        }

        {
            let expected = "12 + foo.bar - a[12] + foo() + foo(bar())";
            assert_eq!(
                expected,
                run_parser!(expression, expected).unwrap().to_string(),
            );
        }
        /*
        {
            let chain = "a + b * c * d + e * (f * g + e) + y";
            let expected = op(
                op(
                    sym("a"),
                    op(
                        op(sym("b"), sym("c"), BinOp::Multiply),
                        sym("d"),
                        BinOp::Multiply,
                    ),
                    BinOp::Add,
                ),
                op(
                    sym("e"),
                    op(
                        Expression::Bracketed(Box::new(op(
                            op(sym("f"), sym("g"), BinOp::Multiply),
                            sym("e"),
                            BinOp::Add,
                        ))),
                        sym("y"),
                        BinOp::Add,
                    ),
                    BinOp::Multiply,
                ),
                BinOp::Add,
            );
            let parsed = run_parser!(expression, chain).unwrap();
            println!("{parsed}");
            assert_eq!(parsed, expected);
        }
        */
        {
            let chain = "a*x + b*(c+y) + d*a*b";
            let expected = op(
                op(
                    op(sym("a"), sym("x"), BinOp::Multiply),
                    op(
                        sym("b"),
                        Expression::Bracketed(Box::new(op(sym("c"), sym("y"), BinOp::Add))),
                        BinOp::Multiply,
                    ),
                    BinOp::Add,
                ),
                op(
                    op(sym("d"), sym("a"), BinOp::Multiply),
                    sym("b"),
                    BinOp::Multiply,
                ),
                BinOp::Add,
            );
            let parsed = run_parser!(expression, chain).unwrap();
            println!("{parsed}");
            assert_eq!(parsed, expected);
        }
    }
}
