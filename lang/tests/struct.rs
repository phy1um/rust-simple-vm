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

#[test]
fn struct_field_set() {
    let test = "
type Array := struct {
  *void ptr,
  int len,
};

int main() {
  let *Array y := makearray();
  setlen(y, 82);
  return y.len;
}

*Array makearray() {
  let *Array out := 0x1000;
  out.ptr := 0x2000;
  out.len := 0;
  return out;
}

void setlen(*Array a, int len) {
  a.len := len; 
}
";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 82);
}
