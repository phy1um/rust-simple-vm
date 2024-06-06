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

pub fn map<S, T, E, U, F, G>(f: F, g: G) -> impl Fn(S) -> Result<(S, U), E> 
where F: Parser<S,T,E>,
      G: Fn(T) -> U,
{
    move |input| {
        let (s, res) = f.run(input)?; 
        Ok((s, g(res)))
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

