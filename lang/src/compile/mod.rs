
mod error;
mod util;
mod resolve;
mod codegen;
mod block;
mod context;

pub use codegen::compile;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_empty_compile() {
        let _ = compile(Vec::new(), 0).unwrap();
    }
}
