use simplevm::Register::*;
use std::cell::RefCell;
use std::rc::Rc;
use std::str;

mod common;
use common::*;

#[test]
fn hello_world() {
    let test = "
global *char IO_OUT;

type string := *char;

int strlen(string s) {
  return *(s-2);
}

void print(string s) {
  let i := 0;
  let pp := strlen(s);
  while (i < pp) {
    *IO_OUT := s[i];
    i := i + 1;
  }
}

void main() {
    IO_OUT := 0xe000;
    let aa := \"hello world!\";
    print(aa);
    return strlen(aa);
}
   ";
    let mut vm = build_machine(test).unwrap();
    let data = Rc::new(RefCell::new(Vec::new()));
    let io_device = SharedBufferDevice::new(data.clone());
    vm.map(0xe000, 1, Box::new(io_device)).unwrap();
    execute_loaded_program(&mut vm).unwrap();
    assert_eq!(vm.get_register(A), 12);
    let data_slice = &data.borrow();
    let contents = str::from_utf8(data_slice).unwrap();
    assert_eq!(contents, "hello world!");
}
