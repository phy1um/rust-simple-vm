use crate::parse::Parser;
use crate::combinator::*;
use crate::character::*;
use crate::ast;
use crate::error::{ParseError, ParseErrorKind};

use std::str::FromStr;

fn parse_type(input: &str) -> Result<(&str, ast::Type), ParseError> {
    let (s, res) = map(repeat1(alpha), |x| x.iter().collect::<String>())(input)?;
    let tt = ast::Type::from_str(&res).map_err(|_s| ParseError::new(input, ParseErrorKind::ExpectedType))?;
    Ok((s, tt))
}

// TODO: underscore :)
fn identifier(input: &str) -> Result<(&str, ast::Identifier), ParseError> {
    let (s0, fst) = alpha(input)?;
    let (s1, rest) = repeat0(alphanumeric)(s0)?;
    let id_str = fst.to_string() + &rest.iter().collect::<String>();
    Ok((s1, ast::Identifier::new(&id_str)))
}

fn expression_literal_int(input: &str) -> Result<(&str, ast::Expression), ParseError> {
    map(repeat1(numeric), |x| ast::Expression::LiteralInt(x.iter().collect::<String>().parse::<i32>().unwrap()))(input)
}

fn expression_literal_char(input: &str) -> Result<(&str, ast::Expression), ParseError> {
    map(wrapped(token("'"), not_char("'"), token("'")), |c| ast::Expression::LiteralChar(c))(input)
}

fn expression_variable(input: &str) -> Result<(&str, ast::Expression), ParseError> {
    map(identifier, |s| ast::Expression::Variable(s.0))(input)
}

pub fn expression_call(input: &str) -> Result<(&str, ast::Expression), ParseError> {
    let (s0, id) = skip_whitespace(identifier)(input)?;
    let (s1, args) = wrapped(token("("), allow_empty(delimited(skip_whitespace(expression), skip_whitespace(token(",")))), token(")"))(s0)?;
    Ok((s1, ast::Expression::FunctionCall(id, args)))
}

pub fn binop(input: &str) -> Result<(&str, ast::BinOp), ParseError> {
    map(
        require(Any::new(vec![
                token("+"), token("-"), token("*"), token("%"), 
                token("=="), token(">="), token(">"), token("<="), token("<"),
                token("!="),
            ]), 
            ParseError::new(input, ParseErrorKind::ExpectedBinop)), 
        |x| ast::BinOp::from_str(x).unwrap(),
    ).run(input)
}

pub fn expression_binop(input: &str) -> Result<(&str, ast::Expression), ParseError> {
    let (s0, expr0) = skip_whitespace(expression_lhs)(input)?;
    let (s1, op) = skip_whitespace(binop)(s0)?;
    let (s2, expr1) = skip_whitespace(expression)(s1)?;
    Ok((s2, ast::Expression::BinOp(Box::new(expr0), Box::new(expr1), op)))
}

fn expression_lhs(input: &str) -> Result<(&str, ast::Expression), ParseError> {
    require(Any::new(vec![
        expression_literal_int,
        expression_literal_char,
        expression_call,
        expression_variable,
    ]), ParseError::new(input, ParseErrorKind::ExpectedExpressionLHS)).run(input)
}

fn expression(input: &str) -> Result<(&str, ast::Expression), ParseError> {
    require(Any::new(vec![
        expression_binop,
        expression_lhs,
    ]), ParseError::new(input, ParseErrorKind::ExpectedExpression).tag("expression")).run(input)
}


pub fn statement_variable_assign(input: &str) -> Result<(&str, ast::Statement), ParseError> {
    let (s0, id) = identifier(input)?;
    let (s1, _) = skip_whitespace(token(":="))(s0)?;
    let (s2, expr) = skip_whitespace(expression)(s1)?;
    Ok((s2, ast::Statement::Assign(id, Box::new(expr))))
}

pub fn statement_variable_declare(input: &str) -> Result<(&str, ast::Statement), ParseError> {
    let (s0, _) = skip_whitespace(token("let"))(input)?;
    let (s1, variable_type) = skip_whitespace(parse_type)(s0)?;
    let (s2, id) = skip_whitespace(identifier)(s1)?;
    let (s3, _) = skip_whitespace(token(":="))(s2)?;
    let (s4, expr) = skip_whitespace(expression)(s3)?;
    Ok((s4, ast::Statement::Declare(id, variable_type, Some(Box::new(expr)))))
}

pub fn statement_return(input: &str) -> Result<(&str, ast::Statement), ParseError> {
    let (s0, _) = skip_whitespace(token("return"))(input)?;
    let (s1, expr) = skip_whitespace(expression)(s0)?;
    Ok((s1, ast::Statement::Return(expr)))
}

pub fn statement_if(input: &str) -> Result<(&str, ast::Statement), ParseError> {
    let (s0, _) = skip_whitespace(token("if"))(input)?;
    let (s1, cond) = wrapped(skip_whitespace(token("(")), expression, skip_whitespace(token(")")))(s0)?;
    let (s2, body) = wrapped(skip_whitespace(token("{")), 
                             repeat1(skip_whitespace(statement_terminated)),
                             skip_whitespace(token("}")))(s1)?;     
    let (sn, else_body) = match skip_whitespace(token("else"))(s2) {
        Ok((s3, _)) => {
             let (s4, eb) = wrapped(skip_whitespace(token("{")), 
                 repeat1(skip_whitespace(statement_terminated)),
                 skip_whitespace(token("}")))(s3)?;
             (s4, Some(eb))
        }
        Err(_) => (s2, None),
    };
    Ok((sn, ast::Statement::If{cond, body, else_body}))
}

fn statement_while(input: &str) -> Result<(&str, ast::Statement), ParseError> {
    let (s0, _) = skip_whitespace(token("while"))(input)?;
    let (s1, cond) = wrapped(skip_whitespace(token("(")), expression, skip_whitespace(token(")")))(s0)?;
    let (sn, body) = wrapped(skip_whitespace(token("{")), 
                             repeat1(skip_whitespace(statement_terminated)),
                             skip_whitespace(token("}")))(s1)?;     
    Ok((sn, ast::Statement::While{cond, body}))
}


pub fn statement_continue(input: &str) -> Result<(&str, ast::Statement), ParseError> {
        map(skip_whitespace(token("continue")), |_| ast::Statement::Continue)(input)
}

pub fn statement_break(input: &str) -> Result<(&str, ast::Statement), ParseError> {
        map(skip_whitespace(token("break")), |_| ast::Statement::Break)(input)
}

pub fn statement(input: &str) -> Result<(&str, ast::Statement), ParseError> {
    AnyCollectErr::new(vec![
        statement_if,
        statement_while,
        statement_variable_assign,
        statement_variable_declare,
        statement_return,
        statement_continue,
        statement_break,
    ]).run(input).map_err(|v| ParseError::from_errs(input, v).tag("STMT"))
}

fn skip_whitespace<'a, T, F>(f: F) -> impl Fn(&'a str) -> Result<(&'a str, T), ParseError> 
    where F: Parser<&'a str, T, ParseError>
{
    move |input| {
        let (s0, _) = discard(repeat0(whitespace))(input)?;
        f.run(s0)
    }
}

fn statement_terminated(input: &str) -> Result<(&str, ast::Statement), ParseError> {
    let (s0, stmt) = statement(input)?;
    let (s1, _) = skip_whitespace(token(";"))(s0)?;
    Ok((s1, stmt))
}

fn named_arg(input: &str) -> Result<(&str, (ast::Identifier, ast::Type)), ParseError> {
    let (s0, ty) = skip_whitespace(parse_type)(input)?;  
    let (s1, name) = skip_whitespace(identifier)(s0)?;
    Ok((s1, (name, ty)))
}

fn function_definition(input: &str) -> Result<(&str, ast::TopLevel), ParseError> {
    let (s0, return_type) = skip_whitespace(parse_type)(input)?;  
    let (s1, name) = skip_whitespace(identifier)(s0)?;
    let (s2, _) = skip_whitespace(token("("))(s1)?;
    let (s3, args) = allow_empty(delimited(named_arg, skip_whitespace(token(","))))(s2)?;
    let (s4, _) = skip_whitespace(token(")"))(s3)?;
    let (s5, body) = wrapped(skip_whitespace(token("{")), repeat1(skip_whitespace(statement_terminated)), skip_whitespace(token("}")))(s4)?;
    Ok((s5, ast::TopLevel::FunctionDefinition{
        name, return_type, body, args,
    }))
}

fn inline_asm(input: &str) -> Result<(&str, ast::TopLevel), ParseError> {
    let (s0, _) = skip_whitespace(token("asm!"))(input)?;
    let (s1, name) = skip_whitespace(identifier)(s0)?;
    let (s2, _) = skip_whitespace(token("("))(s1)?;
    let (s3, args) = allow_empty(delimited(named_arg, skip_whitespace(token(","))))(s2)?;
    let (s4, _) = skip_whitespace(token(")"))(s3)?;
    let (s5, body) = wrapped(skip_whitespace(token("{")), repeat1(not_char("{}")), skip_whitespace(token("}")))(s4)?;
    Ok((s5, ast::TopLevel::InlineAsm{
        name, body: body.iter().collect::<String>(), args,
    }))
}



pub fn parse_ast(input: &str) -> Result<(&str, Vec<ast::TopLevel>), ParseError> {
    let mut out = Vec::new();
    let mut current_state = input;
    loop {
        let (state_next, _) = discard(repeat0(whitespace))(current_state)?;
        if state_next.is_empty() {
            return Ok((state_next, out))
        } else {
            let (snn, res) = 
                AnyCollectErr::new(vec![
                    function_definition,
                    inline_asm,
                ]).run(state_next).map_err(|x| ParseError::from_errs(current_state, x))?; 
            out.push(res);
            current_state = snn;
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parse::run_parser;
    use crate::ast::*;

    #[test]
    fn test_expression_literal_int() {
        assert_eq!(Expression::LiteralInt(5), run_parser(expression_literal_int, "5").unwrap());
        assert_eq!(Expression::LiteralInt(99), run_parser(expression_literal_int, "99").unwrap());
    }

    #[test]
    fn test_expression_call() {
        {
            let expected = "putch('a')";
            assert_eq!(expected, run_parser(expression_call, &expected.to_string()).unwrap().to_string());
        }
        {
            let expected = "foo(1, 2, 3)";
            assert_eq!(expected, run_parser(expression_call, &expected.to_string()).unwrap().to_string());
        }
    }

    #[test]
    fn test_assignment() {
        let expected = Statement::Assign(Identifier::new("foo"), Box::new(Expression::LiteralInt(88)));
        assert_eq!(expected, run_parser(statement_variable_assign, "foo    :=       \n 88").unwrap());
    }

    #[test]
    fn test_declare() {
        let expected = Statement::Declare(Identifier::new("bar"), Type::Char, Some(Box::new(Expression::LiteralChar('a'))));
        assert_eq!(expected, run_parser(statement_variable_declare, "let char bar := 'a'").unwrap());
    }

    #[test]
    fn test_function() -> Result<(), ParseError> {
        let expected = "int foo() {\nlet int a := 7;\na := 99;\n}\n";
        assert_eq!(expected, run_parser(function_definition, expected)?.to_string());
        Ok(())
    }

    #[test]
    fn test_if() {
        {
            let expected = "if (a > 0) {\nfoo := bar;\nreturn 5;\n}\n";
            assert_eq!(expected, run_parser(statement_if, expected).unwrap().to_string());
        }
        {
            let expected = "if (a) {\nfoo := bar;\n} else {\nfoo := baz;\n}\n";
            assert_eq!(expected, run_parser(statement_if, expected).unwrap().to_string());
        }
        {
            let expected = "if (i <= 1) {\nfoo := bar;\nreturn 5;\n}\n";
            assert_eq!(expected, run_parser(statement_if, expected).unwrap().to_string());
        }
    }

    #[test]
    fn test_while() {
        {
            let expected = "while (l < 0) {\nfoo := bar;\nreturn foo;\n}\n";
            assert_eq!(expected, run_parser(statement_while, expected).unwrap().to_string());
        }
        {
            let expected = "while (a <= 10) {\na := a + 1;\n}\n";
            assert_eq!(expected, run_parser(statement_while, expected).unwrap().to_string());
        }
    }

    #[test]
    fn test_conditions() {
        {
            let expected = "i <= 1";
            assert_eq!(expected, run_parser(expression_binop, expected).unwrap().to_string());
        }
    }

    #[test]
    fn test_inline_asm() {
        {
            let expected = "asm! foobar(int a, int b) {\nAdd A B A\nSub A B A\n}\n";
            assert_eq!(expected, run_parser(inline_asm, expected).unwrap().to_string());
        }
    }

}
