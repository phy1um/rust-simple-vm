mod block;
mod codegen;
mod context;
mod error;
mod resolve;
mod util;

pub use codegen::compile;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_empty_compile() {
        let _ = compile(Vec::new(), 0).unwrap();
    }
}
