pub trait Parser<S, T, E> {
    fn run(&self, s: S) -> Result<(S, T), E>;
}

impl<S, T, E, F> Parser<S, T, E> for F
where
    F: Fn(S) -> Result<(S, T), E>,
{
    fn run(&self, s: S) -> Result<(S, T), E> {
        self(s)
    }
}

pub fn run_parser<S, T, E>(parser: impl Parser<S, T, E>, input: S) -> Result<T, E> {
    let (_, res) = parser.run(input)?;
    Ok(res)
}
