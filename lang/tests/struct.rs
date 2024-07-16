use simplevm::Register::*;

mod common;
use common::*;

#[test]
fn struct_in_frame() {
    let test = "
type Foo := struct {
    int x,
    int y,
    int z,
};

void main() {
    let Foo f;
    f.x := 0x34c;
    f.y := 0x2e6;
    f.z := 0x1ff;
    return 5;
}
   ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 5);
}
