use crate::parse::Parser;

use std::marker::PhantomData;

#[derive(Debug, Default)]
pub struct Sequence<S, T, E, F> 
where F: Parser<S, T, E>
{
    _t: PhantomData<(S, T, E)>,
    items: Vec<F>,
}

impl<S, T, E, F> Sequence<S, T, E, F> 
where F: Parser<S, T, E>
{
    pub fn new(items: Vec<F>) -> Self {
        Self { items, _t: PhantomData::default() }
    }
}

impl<S, T, E, F: Parser<S, T, E>> Parser<S, Vec<T>, E> for Sequence<S, T, E, F> {
    fn run(&self, s: S) -> Result<(S, Vec<T>), E> {
        let mut state = s;
        let mut out = Vec::new();
        for i in &self.items {
            let (s, res) = i.run(state)?; 
            state = s;
            out.push(res);
        };
        Ok((state, out))
    }
}

#[derive(Debug, Default)]
pub struct Any<S, T, E, F>
where F: Parser<S, T, E>
{
    _t: PhantomData<(S, T, E)>,
    items: Vec<F>,
}

impl <S, T, E, F> Any<S, T, E, F> 
where F: Parser<S, T, E>
{
    pub fn new(items: Vec<F>) -> Self {
        Self {items, _t: PhantomData::default() }
    }
}

impl<S: Clone, T, E, F: Parser<S, T, E>> Parser<S, Option<T>, E> for Any<S, T, E, F> {
    fn run(&self, s: S) -> Result<(S, Option<T>), E> {
        for item in &self.items {
            if let Ok((sn, res)) = item.run(s.clone()) {
                return Ok((sn, Some(res)))
            };
        };
        Ok((s, None))
    }
}

pub fn map<S, T, E, U, F, G>(f: F, g: G) -> impl Fn(S) -> Result<(S, U), E> 
where F: Parser<S,T,E>,
      G: Fn(T) -> U,
{
    move |input| {
        let (s, res) = f.run(input)?; 
        Ok((s, g(res)))
    }
}

pub fn require<S, T, E: Default, F>(f: F) -> impl Fn(S) -> Result<(S, T), E>
where F: Parser<S, Option<T>, E>
{
    move |input| {
        let (s, res) = f.run(input)?;
        if let Some(t) = res {
            Ok((s, t))
        } else {
            Err(E::default())
        }
    }
}

pub fn repeat0<S: Clone, T, E: Clone + Default, F>(f: F) -> impl Fn(S) -> Result<(S, Vec<T>), E> 
where F: Parser<S, T, E>
{
    repeat(0, E::default(), f)
}

pub fn repeat1<S: Clone, T, E: Clone + Default, F>(f: F) -> impl Fn(S) -> Result<(S, Vec<T>), E> 
where F: Parser<S, T, E>
{
    repeat(1, E::default(), f)
}

pub fn discard<S, T, E, F>(f: F) -> impl Fn(S) -> Result<(S, ()), E> 
where F: Parser<S, T, E>
{
    map(f, |_| ())
}

fn repeat<S: Clone, T, E: Clone, F>(min_matches: usize, err: E, f: F) -> impl Fn(S) -> Result<(S, Vec<T>), E> 
where F: Parser<S, T, E>
{
    move |input| {
        let mut out = Vec::new();
        let mut state = input;
        loop {
            if let Ok((s, res)) = f.run(state.clone()) {
                out.push(res);
                state = s;
            } else {
                return if out.len() < min_matches {
                    Err(err.clone())
                } else {
                    Ok((state, out))
                };
            }
        }
    }
}

fn wrapped<A, B, C, S, E, F, G, H>(f: F, g: G, h: H) -> impl Fn(S) -> Result<(S, B), E> 
where F: Parser<S, A, E>,
      G: Parser<S, B, E>,
      H: Parser<S, C, E>,
{
    move |input| {
        let (input0, _) = f.run(input)?;
        let (input1, res) = g.run(input0)?;
        let (input2, _) = h.run(input1)?;
        Ok((input2, res))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::character::*;

    #[test]
    fn test_wrapped() {
        {
            let (s, n) = wrapped(token("{"), alpha, token("}"))("{a}").unwrap();
            assert_eq!('a', n); 
            assert_eq!("", s); 
        }
        {
            let (s, n) = wrapped(token("<<"), map(repeat1(alpha), |x| x.iter().collect::<String>()), token("}}"))("<<foobar}}").unwrap();
            assert_eq!("foobar", n); 
            assert_eq!("", s); 
        }
        {
            let (s, n) = wrapped(
                token("\""), 
                map(repeat1(not_char("\"")), |x| x.iter().collect::<String>()),
                token("\""),
            )("\"
Hello world this is some text. \"").unwrap();
            assert_eq!("\nHello world this is some text. ", n);
            assert_eq!("", s);
        }
    }

    #[test]
    fn test_any() {
        let (s, n) = require(Any::new(vec![alpha, numeric]))("1").unwrap();
        assert_eq!("", s);
        assert_eq!('1', n);
    }
}
