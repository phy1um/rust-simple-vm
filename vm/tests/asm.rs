mod common;
use common::*;

use simplevm::binfmt::BinaryFile;
use simplevm::pp::macros;
use simplevm::pp::PreProcessor;
use simplevm::*;

#[test]
fn processed_binary_test() {
    let mut pp = PreProcessor::default();
    macros::setup_std_macros(&mut pp);
    pp.handle(
        "
.section code 0 RO
Imm A 1
Imm B 2
Add C A B
System Zero Zero $f
    ",
    )
    .unwrap();
    let bin: BinaryFile = pp.try_into().unwrap();
    let mut vm = make_test_vm(0).unwrap();
    run_binary(&mut vm, &bin).unwrap();
    assert_eq!(vm.get_register(Register::C), 3);
}
