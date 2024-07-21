use simplevm::Register::*;

mod common;
use common::*;

#[test]
fn array_return() {
    let test = "
void main() {
    let *int b := 0x1000;
    b[0] := 17; 
    b[1] := 10;
    return b[0];
}
   ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 17);
}
