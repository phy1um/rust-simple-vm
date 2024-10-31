use lang::*;
use simplevm::*;
use std::cell::RefCell;
use std::rc::Rc;

#[macro_export]
macro_rules! assert_reg {
    ($vm: expr, $r:expr, $v:expr) => {
        assert!(
            $vm.get_register($r) == $v,
            "expected {:X}, {} == {:X}",
            $v,
            $r,
            $vm.get_register($r)
        );
    };
}

#[macro_export]
macro_rules! assert_mem {
    ($vm: expr, $addr:expr, $v:expr) => {{
        let result = $vm.memory.read(($addr) as u32)?;
        assert!(
            result == $v,
            "expected {:X} @{:X}, got {:X}",
            $v,
            $addr,
            result
        );
    }};
}

pub const SIGHALT: u8 = 0xf0;

pub fn signal_halt(vm: &mut VM, _: u16) -> Result<(), String> {
    vm.halt = true;
    Ok(())
}

pub const MAX_TEST_CYCLES: u32 = 100_000;
pub fn build_machine(program: &str) -> Result<Machine, String> {
    let mut vm = Machine::default();
    vm.set_register(Register::SP, 1024 * 3);
    vm.define_handler(SIGHALT, signal_halt);

    let prog = parse_ast(program).unwrap();
    for p in &prog {
        println!("{p}");
    }
    let res = compile(prog, 0).unwrap();
    println!("{res}");
    let bin = res.to_binary()?;
    println!("{bin}");
    bin.load_to_vm(&mut vm)?;
    vm.set_program_counter(bin.entrypoint.into());
    Ok(vm)
}

pub fn execute_loaded_program(vm: &mut Machine) -> Result<(), String> {
    let mut cycle_count = 0;
    while !vm.is_halt() {
        vm.step().map_err(|s| error_with_context(&vm, &s))?;
        cycle_count += 1;
        if cycle_count >= MAX_TEST_CYCLES {
            return Err("max cycles exceeded".to_string());
        }
    }
    Ok(())
}

#[allow(dead_code)]
pub fn run_program(program: &str) -> Result<Machine, String> {
    let mut vm = build_machine(program)?;
    execute_loaded_program(&mut vm)?;
    Ok(vm)
}

fn error_with_context(vm: &Machine, s: &str) -> String {
    format!(
        "!! VM ERROR !!: {s} @ {}\nA: {} | B: {} | C: {} | D: {} \nM: {} | BP: {} | SP: {}",
        vm.get_program_counter(),
        vm.get_register(Register::A),
        vm.get_register(Register::B),
        vm.get_register(Register::C),
        vm.get_register(Register::D),
        vm.get_register(Register::M),
        vm.get_register(Register::BP),
        vm.get_register(Register::SP),
    )
}

pub struct SharedBufferDevice {
    pub data: Rc<RefCell<Vec<u8>>>,
}

#[allow(dead_code)]
impl SharedBufferDevice {
    pub fn new(data: Rc<RefCell<Vec<u8>>>) -> Self {
        Self { data }
    }
}

impl Addressable for SharedBufferDevice {
    fn read(&mut self, _addr: u32) -> Result<u8, MemoryError> {
        Ok(0)
    }

    fn write(&mut self, _addr: u32, value: u8) -> Result<(), MemoryError> {
        self.data.borrow_mut().push(value);
        Ok(())
    }

    fn zero_all(&mut self) -> Result<(), MemoryError> {
        Ok(())
    }
}
