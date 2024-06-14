use crate::parse::Parser;
use crate::combinator::*;
use crate::character::*;
use crate::ast;

use std::str::FromStr;

fn parse_type(input: &str) -> Result<(&str, ast::Type), String> {
    let (s, res) = map(repeat1(alpha), |x| x.iter().collect::<String>())(input)?;
    let tt = ast::Type::from_str(&res)?;
    Ok((s, tt))
}

fn identifier(input: &str) -> Result<(&str, ast::Identifier), String> {
    let (s0, fst) = alpha(input)?;
    let (s1, rest) = repeat0(alphanumeric)(s0)?;
    let id_str = fst.to_string() + &rest.iter().collect::<String>();
    Ok((s1, ast::Identifier::new(&id_str)))
}

fn expression_literal_int(input: &str) -> Result<(&str, ast::Expression), String> {
    map(repeat1(numeric), |x| ast::Expression::LiteralInt(x.iter().collect::<String>().parse::<i32>().unwrap()))(input)
}

fn expression_literal_char(input: &str) -> Result<(&str, ast::Expression), String> {
    map(wrapped(token("'"), not_char("'"), token("'")), |c| ast::Expression::LiteralChar(c))(input)
}

pub fn expression_call(input: &str) -> Result<(&str, ast::Expression), String> {
    let (s0, id) = skip_whitespace(identifier)(input)?;
    let (s1, args) = wrapped(token("("), repeat0(expression), token(")"))(s0)?;
    Ok((s1, ast::Expression::FunctionCall(id, args)))
}

fn expression(input: &str) -> Result<(&str, ast::Expression), String> {
    require(Any::new(vec![
        expression_literal_int,
        expression_literal_char,
        expression_call,
    ]))(input)
}

pub fn statement_variable_assign(input: &str) -> Result<(&str, ast::Statement), String> {
    let (s0, id) = identifier(input)?;
    let (s1, _) = skip_whitespace(token(":="))(s0)?;
    let (s2, expr) = skip_whitespace(expression)(s1)?;
    Ok((s2, ast::Statement::Assign(id, Box::new(expr))))
}

pub fn statement_variable_declare(input: &str) -> Result<(&str, ast::Statement), String> {
    let (s0, _) = skip_whitespace(token("let"))(input)?;
    let (s1, variable_type) = skip_whitespace(parse_type)(s0)?;
    let (s2, id) = skip_whitespace(identifier)(s1)?;
    let (s3, _) = skip_whitespace(token(":="))(s2)?;
    let (s4, expr) = skip_whitespace(expression)(s3)?;
    Ok((s4, ast::Statement::Declare(id, variable_type, Some(Box::new(expr)))))
}


pub fn statement(input: &str) -> Result<(&str, ast::Statement), String> {
    require(Any::new(vec![
        statement_variable_assign,
        statement_variable_declare,
    ]))(input)
}

fn skip_whitespace<'a, T, F>(f: F) -> impl Fn(&'a str) -> Result<(&'a str, T), String> 
    where F: Parser<&'a str, T, String>
{
    move |input| {
        let (s0, _) = discard(repeat0(whitespace))(input)?;
        f.run(s0)
    }
}

fn statement_terminated(input: &str) -> Result<(&str, ast::Statement), String> {
    let (s0, stmt) = statement(input)?;
    let (s1, _) = skip_whitespace(token(";"))(s0)?;
    Ok((s1, stmt))
}

fn function_definition(input: &str) -> Result<(&str, ast::TopLevel), String> {
    println!("try function definition: {}", input.len());
    let (s0, return_type) = skip_whitespace(parse_type)(input)?;  
    let (s1, name) = skip_whitespace(identifier)(s0)?;
    let (s2, _) = skip_whitespace(token("("))(s1)?;
    let (s3, _) = skip_whitespace(token(")"))(s2)?;
    let (s4, body) = wrapped(skip_whitespace(token("{")), repeat1(skip_whitespace(statement_terminated)), skip_whitespace(token("}")))(s3)?;
    Ok((s4, ast::TopLevel::FunctionDefinition{
        name, return_type, body, args: Vec::new(),
    }))
}

pub fn parse_ast(input: &str) -> Result<(&str, Vec<ast::TopLevel>), String> {
    repeat1(require(Any::new(vec![
        function_definition,
    ])))(input)
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
        let expected = "putch('a')";
        assert_eq!(expected, run_parser(expression_call, &expected.to_string()).unwrap().to_string());
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
    fn test_function() -> Result<(), String> {
        let expected = "int foo() {\nlet int a := 7;\na := 99;\n}\n";
        assert_eq!(expected, run_parser(function_definition, expected)?.to_string());
        Ok(())
    }

}
