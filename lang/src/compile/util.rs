use rand::{distributions::Alphanumeric, Rng};

pub fn gensym(r: impl Rng) -> String {
    r.sample_iter(&Alphanumeric)
        .take(16)
        .map(char::from)
        .collect::<String>()
}
