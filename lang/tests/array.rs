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

#[test]
fn array_in_struct() {
    let test = "
global int mhead;

type Foo := struct {
    int size,
    struct {
        *int elems,
    } bar,
};

*int malloc(int n) {
    let out := mhead;    
    mhead := mhead + n;
    return out;
}

void main() {
    mhead := 0x1000;
    let Foo foo;
    foo.bar.elems := malloc(100);
    foo.bar.elems[20] := 0xff;
    return foo.bar.elems[20];
}
   ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 0xff);
}
