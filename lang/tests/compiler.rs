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

