use simplevm::Register::*;

use crate::*;
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
    let prog = run_parser(parse_ast, test).unwrap();
    let res = compile(prog, 0).unwrap();
    let instructions = res.get_instructions().unwrap();
    let mut vm = make_test_vm(0x8000).unwrap();
    run(&mut vm, &instructions).unwrap();
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
    let prog = run_parser(parse_ast, test).unwrap();
    let res = compile(prog, 0).unwrap();
    let instructions = res.get_instructions().unwrap();
    let mut vm = make_test_vm(0x8000).unwrap();
    run(&mut vm, &instructions).unwrap();
    assert_eq!(vm.get_register(A), 18);
}

#[test]
fn if_branch() {
    let test = "
void main() {
    let int i := a(2);
    return i;
}

int a(int x) {
    if (x <= 1) {
        return 7;
    } else {
        return 12;
    };
}
   ";
    let prog = run_parser(parse_ast, test).unwrap();
    let res = compile(prog, 0).unwrap();
    let instructions = res.get_instructions().unwrap();
    let mut vm = make_test_vm(0x8000).unwrap();
    run(&mut vm, &instructions).unwrap();
    assert_eq!(vm.get_register(A), 12);
   
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
    };
}
   ";
    let prog = run_parser(parse_ast, test).unwrap();
    let res = compile(prog, 0).unwrap();
    let instructions = res.get_instructions().unwrap();
    let mut vm = make_test_vm(0x8000).unwrap();
    run(&mut vm, &instructions).unwrap();
    assert_eq!(vm.get_register(A), 15);
}
