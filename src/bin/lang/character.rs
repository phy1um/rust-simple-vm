pub fn is_char(c: char) -> impl Fn(&str) -> Result<(&str, char), String> {
    move |input| {
        if let Some(x) = input.chars().next() {
            if x == c {
                Ok((&input[1..], c))
            } else {
                Err(format!("expected '{c}' got '{x}'"))
            }
        } else {
            Err("empty".to_string())
        }
    }
}

pub fn token<'a, 'b>(s: &'a str) -> impl Fn(&'b str) -> Result<(&'b str, &'a str), String> {
    move |input| {
        if let Some(_) = input.strip_prefix(s) {
            Ok((&input[s.len()..], s)) 
        } else {
            Err(format!("expected \"{}\" got \"{}\"", s, &input[0..s.len()]))
        }
    }
}

pub fn alpha(input: &str) -> Result<(&str, char), String> {
    char_predicate(char::is_alphabetic, "alphabetic".to_string())(input)
}

pub fn numeric(input: &str) -> Result<(&str, char), String> {
    char_predicate(char::is_numeric, "numeric".to_string())(input)
}

pub fn alphanumeric(input: &str) -> Result<(&str, char), String> {
    char_predicate(char::is_alphanumeric, "alphanumeric".to_string())(input)
}

pub fn whitespace(input: &str) -> Result<(&str, char), String> {
    char_predicate(char::is_whitespace, "whitespace".to_string())(input)
}


pub fn char_predicate(f: fn(char) -> bool, name: String) -> impl Fn(&str) -> Result<(&str, char), String> {
    move |input| {
        if let Some(c) = input.chars().next() {
            if f(c) {
                Ok((&input[1..], c))
            } else {
                Err(format!("expected {name}, got '{c}'"))
            }
        } else {
            Err("empty".to_string())
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
            let (s, n) = numeric("1234").unwrap();
            assert_eq!("234", s);
            assert_eq!(n, '1');
        }
        {
            let (s, n) = map(repeat1(numeric), |x| x.iter().collect::<String>())("1234").unwrap();
            assert_eq!("", s);
            assert_eq!("1234", n);
        }
        {
            let (s, n) = map(repeat1(numeric), |x| x.iter().collect::<String>())("1234abcd").unwrap();
            assert_eq!("abcd", s);
            assert_eq!("1234", n);
        }
    }

    #[test]
    fn test_alpha() {
        {
            let (s, n) = alpha("xxxx").unwrap();
            assert_eq!("xxx", s);
            assert_eq!(n, 'x');
        }
        {
            let (s, n) = map(repeat1(alpha), |x| x.iter().collect::<String>())("abcdef").unwrap();
            assert_eq!("", s);
            assert_eq!("abcdef", n);
        }
        {
            let (s, n) = map(repeat1(alpha), |x| x.iter().collect::<String>())("xyz:123").unwrap();
            assert_eq!(":123", s);
            assert_eq!("xyz", n);
        }
    }

}
