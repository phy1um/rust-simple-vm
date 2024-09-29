use simplevm::Register::*;

mod common;
use common::*;

#[test]
fn binop_chain() {
    let test = "
void main() {
    return op(1,2,3,4);
}

int op(int a, int b, int c, int d) {
    return a*2 + b*(c+7) + d*a*b;
}
   ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 30);
}

#[test]
fn long_chain() {
    let test = "
void main() {
    let int b := 3;
    let int x := 1 + b;
    return (x + b) * 2 * (x*b*x*(x + 2 + 7 + 9 + b + x + b));
}
   ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 21504);
}

#[test]
fn chain_with_fncall() {
    let test = "

int foo() {
    return 7;
}

int bar() {
    return foo() + 1;
}

void main() {
    return 8 + foo() + bar();
}
   ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 23);
}
