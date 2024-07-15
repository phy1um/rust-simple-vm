use simplevm::Register::*;
mod common;
use common::*;

#[test]
fn inline_asm() {
    let test = "
void main() {
    let int a := 7;
    let int b := addnums(a, 11);
    return b;
}

asm! addnums(int a, int b) {
    LoadStackOffset A SP 3    
    LoadStackOffset B SP 4    
    Add A B A
}

   ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 18);
}

#[test]
fn expr_brackets() {
    // TODO: make this better with multiplication
    let test = "
void main() {
    let int x := 7;
    let int y := 2;
    return (5+x) + (2+y);
}

";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 16);
}

#[test]
fn soft_multiply() {
    // TODO: make this better with multiplication
    let test = "
int main() {
    return mult(7, 8);
}

int mult(int a, int b) {
    let int c := 0;    
    let int out := 0;
    while (c < b) {
        out := out + a;
        c := c + 1;
    }
    return out;
}

";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 56);
}
