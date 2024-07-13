use simplevm::Register::*;

mod common;
use common::*;

#[test]
fn while_loop() {
    let test = "
void main() {
    let a := 1;
    while (a <= 10) {
      a := a + 1;
    }
    return a;
}

   ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 11);
}

#[test]
fn nested_while_loop() {
    let test = "
void main() {
    let a := 0;
    let b := 0;
    let c := 0;
    while (a <= 9) {
      a := a + 1;
      b := 0;
      while (b <= 9) {
        b := b + 1;
        c := c + 1;
      }
    }
    return c;
}

   ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 100);
}

#[test]
fn break_loop() {
    let test = "
void main() {
    let a := 0;
    while (a <= 5) {
        a := a + 1;
        break;
    }
    return a;
}

   ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 1);
}

#[test]
fn break_nested_loop() {
    let test = "
void main() {
    let a := 0;
    let c := 0;
    while (a <= 3) {
        a := a + 1;
        c := c + 1;
        let b := 0;
        while (b <= 2) {
            b := b + 1;
            c := c + b;
            break;
        }
    }
    return c;
}

   ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 8);
}

#[test]
fn continue_loop() {
    let test = "
void main() {
    let a := 0;
    let c := 0;
    while (a <= 3) {
        a := a + 1;
        c := c + 1;
        continue;
        let b := 0;
        while (b <= 2) {
            b := b + 1;
            c := c + b;
        }
    }
    return c;
}

   ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 4);
}

#[test]
fn continue_nested_loop() {
    let test = "
void main() {
    let a := 0x0;
    let c := 0;
    while (a <= 3) {
        a := a + 1;
        c := c + 1;
        let b := 0;
        while (b <= 2) {
            b := b + 1;
            continue;
            c := c + b;
        }
    }
    return c;
}
   ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 4);
}

#[test]
fn code_after_while() {
    let test = "
void main() {
    let int a := 0;
    let int b := 0;
    let int c := 0;
    while (a < 40) {
        b := 0;
        while (b < 40) {
            c := c + 1;
            b := b + 1;
        }
        a := a + 1;
    }
    return c;
}
   ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 1600);
}
