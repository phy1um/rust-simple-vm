use std::marker::PhantomData;

trait Parser<S, T, E> {
    fn run(&self, s: S) -> Result<(S, T), E>;
}

impl<S, T, E, F> Parser<S, T, E> for F 
where F: Fn(S) -> Result<(S, T), E>
{
    fn run(&self, s: S) -> Result<(S,T), E> {
        self(s)
    }
}

#[derive(Debug, Default)]
struct Sequence<S, T, E, F> 
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

fn run_parser<S, T, E>(parser: impl Parser<S, T, E>, input: S) -> Result<T, E> 
{
    let (_, res) = parser.run(input)?;
    Ok(res)
}

fn is_char(c: char) -> impl Fn(&str) -> Result<(&str, char), String> {
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

fn main() -> Result<(), String> {
    let cmb = Sequence::new(vec![is_char('a'), is_char('o'), is_char('o')]);
    let c = run_parser(cmb, "aoo bar")?; 
    println!("got: {}", c.iter().collect::<String>());
    Ok(())
}

