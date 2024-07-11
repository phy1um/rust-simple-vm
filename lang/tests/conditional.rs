use simplevm::Register::*;

mod common;
use common::*;

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
    }
}
   ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 12);
}

#[test]
fn nested_if() {
    let test = "
void main() {
    let int i := a(7);
    return i;
}

int a(int x) {
    if (x <= 9) {
        if (x <= 8) {
            if (x <= 7) {
                return 5;
            } else {
                return 99;
            }
        } else {
            return 98;
        }
    } else {
        return 97;
    }
}
   ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 5);
}
