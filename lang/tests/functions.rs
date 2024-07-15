use simplevm::Register::*;

mod common;
use common::*;

#[test]
fn deep_calls() {
    let test = "
void main() {
    let int i := a();
    return i;
}

int a() {
    return b();
}

int b() {
    return c();
}

int c() {
    return d();
}

int d() {
    return 7+8;
}
   ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 15);
}

#[test]
fn deep_arg_calls() {
    let test = "
void main() {
    let int i := a(1, 2);
    return i;
}

int a(int x, int y) {
    return b(x+1, y+2);
}

int b(int x, int y) {
    return c(x+2, y+3);
}

int c(int x, int y) {
    return d(x+3, y+4);
}

int d(int x, int y) {
    return x+y;
}
   ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 18);
}

#[test]
fn recursive_calls() {
    let test = "
void main() {
    let int i := a(5, 11);
    return i;
}

int a(int x, int y) {
    if (x <= 1) {
        return y;
    } else {
        return a(x-1, y+1);
    }
}
   ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 15);
}

#[test]
fn stack_shuffling() {
    let test = "
int main() {
    let x := 5;
    let y := 7;
    let z := 2;
    return foo(foo(x, z)-1, y);
}

int foo(int x, int y) {
    return x+y;    
}

";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 13);
}
