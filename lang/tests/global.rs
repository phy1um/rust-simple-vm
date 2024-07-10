use simplevm::Register::*;

mod common;
use common::*;

#[test]
fn global_int() {
    let test = "
global int test;

void main() {
    test := 2;
    let int b := foobar(test, 7);
    return b;
}

int foobar(int a, int b) {
  return a;
}

   ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 2);
}

#[test]
fn two_globals() {
    let test = "
global int test;
global int foo;

void main() {
    test := 82;
    foo := test;
    return foo;
}
   ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 82);
}
