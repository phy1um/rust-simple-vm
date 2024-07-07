
use simplevm::Register::*;

mod common;
use common::*;

#[test]
fn addr_of_global() {
   let test = "
global int test;
global int foo;

void main() {
    let *int b := &foo;
    return b;
}
   ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 2);
}

