use std::collections::HashMap;

use crate::memory::{Addressable, MemoryMapper};
use crate::op::Instruction;
use crate::op_fields::{StackOp, TestOp};
use crate::register::{Flag, Register};

pub trait SignalHandler {
    fn handle(&self, m: &mut VM, arg: u16) -> Result<(), String>;
}

impl<F> SignalHandler for F
where
    F: Fn(&mut VM, u16) -> Result<(), String>,
{
    fn handle(&self, m: &mut VM, arg: u16) -> Result<(), String> {
        self(m, arg)
    }
}

#[derive(Default)]
pub struct VM {
    registers: [u16; 8],
    flags: u16,
    program_counter: u32,
    pub halt: bool,
    pub memory: MemoryMapper,
}

#[derive(Default)]
pub struct Machine {
    signal_handlers: HashMap<u8, Box<dyn SignalHandler>>,
    pub vm: VM,
}

impl Machine {
    pub fn is_halt(&self) -> bool {
        self.vm.halt
    }

    pub fn define_handler(&mut self, index: u8, f: impl SignalHandler + 'static) {
        self.signal_handlers.insert(index, Box::new(f));
    }

    pub fn step(&mut self) -> Result<(), String> {
        self.vm.step(&self.signal_handlers)
    }

    pub fn map(
        &mut self,
        start: usize,
        size: usize,
        a: Box<dyn Addressable>,
    ) -> Result<(), String> {
        self.vm.map(start, size, a)
    }

    pub fn reset(&mut self) {
        self.vm.reset()
    }

    pub fn state(&self) -> String {
        self.vm.state()
    }

    pub fn get_register(&self, r: Register) -> u16 {
        self.vm.get_register(r)
    }

    pub fn set_register(&mut self, r: Register, v: u16) {
        self.vm.set_register(r, v)
    }

    pub fn set_program_counter(&mut self, addr: u32) {
        self.vm.set_program_counter(addr);
    }

    pub fn get_program_counter(&self) -> u32 {
        self.vm.get_program_counter()
    }

    pub fn set_flag(&mut self, flag: Flag, state: bool) {
        self.vm.set_flag(flag, state)
    }

    pub fn test_flag(&self, flag: Flag) -> bool {
        self.vm.test_flag(flag)
    }
}

impl VM {
    pub fn new() -> Self {
        Self { ..Self::default() }
    }

    pub fn map(
        &mut self,
        start: usize,
        size: usize,
        a: Box<dyn Addressable>,
    ) -> Result<(), String> {
        self.memory.map(start, size, a)
    }

    pub fn reset(&mut self) {
        let _ = self.memory.zero_all();
        self.registers = [0; 8];
        self.flags = 0;
        self.halt = false;
        self.program_counter = 0;
    }

    pub fn state(&self) -> String {
        format!(
            "A: {} | B: {} | C: {} | D: {}
SP: {} | M: {} | BP: {}
PC @ {}, Flags: {:016b}",
            self.get_register(Register::A),
            self.get_register(Register::B),
            self.get_register(Register::C),
            self.get_register(Register::D),
            self.get_register(Register::M),
            self.get_register(Register::SP),
            self.get_register(Register::BP),
            self.get_program_counter(),
            self.flags,
        )
    }

    pub fn get_register(&self, r: Register) -> u16 {
        if r == Register::Zero {
            0
        } else {
            self.registers[r as usize]
        }
    }

    pub fn set_register(&mut self, r: Register, v: u16) {
        if r == Register::Zero {
            return;
        };
        self.registers[r as usize] = v;
    }

    pub fn set_program_counter(&mut self, addr: u32) {
        self.set_flag(Flag::DidJump, true);
        self.program_counter = addr;
    }

    pub fn get_program_counter(&self) -> u32 {
        self.program_counter
    }

    pub fn pop(&mut self, stack_pointer_register: Register) -> Result<u16, String> {
        let sp = self.get_register(stack_pointer_register) - 2;
        let v = self.memory.read2(sp as u32).map_err(|x| x.to_string())?;
        self.set_register(stack_pointer_register, sp);
        Ok(v)
    }

    pub fn peek(&mut self, stack_pointer_register: Register) -> Result<u16, String> {
        let sp = self.get_register(stack_pointer_register) - 2;
        self.memory.read2(sp as u32).map_err(|x| x.to_string())
    }

    pub fn push(&mut self, stack_pointer_register: Register, v: u16) -> Result<(), String> {
        let sp = self.get_register(stack_pointer_register);
        self.set_register(stack_pointer_register, sp + 2);
        self.memory.write2(sp as u32, v).map_err(|x| x.to_string())
    }

    pub fn set_flag(&mut self, flag: Flag, state: bool) {
        if state {
            self.flags |= flag as u16;
        } else {
            self.flags &= !(flag as u16);
        }
    }

    pub fn test_flag(&self, flag: Flag) -> bool {
        self.flags & (flag as u16) != 0
    }

    pub fn step(
        &mut self,
        signal_handlers: &HashMap<u8, Box<dyn SignalHandler>>,
    ) -> Result<(), String> {
        let pc = self.get_program_counter();
        let instruction = self.memory.read2(pc).map_err(|x| x.to_string())?;
        self.set_flag(Flag::DidJump, false);
        let op = Instruction::try_from(instruction)?;
        // println!("running {}", op);
        match op {
            Instruction::Invalid => Err("0 instruction".to_string()),
            Instruction::Imm(reg, v) => {
                self.set_register(reg, v.value);
                Ok(())
            }
            // binops
            Instruction::Add(dst, r0, r1) => {
                let a = self.get_register(r0);
                let b = self.get_register(r1);
                let (res, overflow) = a.overflowing_add(b);
                self.set_register(dst, res);
                self.set_flag(Flag::Overflow, overflow);
                Ok(())
            }
            Instruction::Sub(dst, r0, r1) => {
                let a = self.get_register(r0);
                let b = self.get_register(r1);
                let (res, overflow) = a.overflowing_sub(b);
                self.set_register(dst, res);
                self.set_flag(Flag::Overflow, overflow);
                Ok(())
            }
            Instruction::Mul(dst, r0, r1) => {
                let a = self.get_register(r0);
                let b = self.get_register(r1);
                let (res, overflow) = a.overflowing_mul(b);
                self.set_register(dst, res);
                self.set_flag(Flag::Overflow, overflow);
                Ok(())
            }
            Instruction::And(dst, r0, r1) => {
                let a = self.get_register(r0);
                let b = self.get_register(r1);
                self.set_register(dst, a & b);
                Ok(())
            }
            Instruction::Or(dst, r0, r1) => {
                let a = self.get_register(r0);
                let b = self.get_register(r1);
                self.set_register(dst, a | b);
                Ok(())
            }
            Instruction::Xor(dst, r0, r1) => {
                let a = self.get_register(r0);
                let b = self.get_register(r1);
                self.set_register(dst, a ^ b);
                Ok(())
            }
            Instruction::Mod(dst, r0, r1) => {
                let a = self.get_register(r0);
                let b = self.get_register(r1);
                self.set_register(dst, a % b);
                Ok(())
            }
            // immediates
            Instruction::AddImm(r, i) => {
                let (res, _overflow) = self.get_register(r).overflowing_add(i.value as u16);
                self.set_register(r, res);
                Ok(())
            }
            Instruction::AddImmSigned(r, i) => {
                let raw_register_value = self.get_register(r);
                let imm_signed = i.as_signed();
                unsafe {
                    let register_signed: i16 = std::mem::transmute(raw_register_value);
                    let (res, _overflow) = register_signed.overflowing_add(imm_signed as i16);
                    self.set_register(r, std::mem::transmute(res));
                }
                Ok(())
            }
            Instruction::ShiftLeft(dst, r0, offset) => {
                let base = self.get_register(r0);
                self.set_register(dst, base << (offset.value as u16));
                Ok(())
            }
            Instruction::ShiftRightLogical(dst, r0, offset) => {
                let base = self.get_register(r0);
                self.set_register(dst, base >> (offset.value as u16));
                Ok(())
            }
            Instruction::ShiftRightArithmetic(dst, r0, offset) => {
                let base = self.get_register(r0);
                unsafe {
                    let as_signed: i16 = std::mem::transmute(base);
                    let shifted: i16 = as_signed >> (offset.value as u16);
                    let res: u16 = std::mem::transmute(shifted);
                    self.set_register(dst, res);
                }
                Ok(())
            }
            Instruction::LoadWord(dst, r1, r2) => {
                let base = self.get_register(r1);
                let page = self.get_register(r2);
                let addr = (base as u32) + ((page as u32) << 16);
                let w = self.memory.read2(addr).map_err(|x| x.to_string())?;
                self.set_register(dst, w);
                Ok(())
            }
            Instruction::LoadByte(dst, r1, r2) => {
                let base = self.get_register(r1);
                let page = self.get_register(r2);
                let addr = (base as u32) + ((page as u32) << 16);
                let w = self.memory.read(addr).map_err(|x| x.to_string())?;
                self.set_register(dst, w as u16);
                Ok(())
            }
            Instruction::StoreWord(src, r1, r2) => {
                let base = self.get_register(r1);
                let page = self.get_register(r2);
                let addr = (base as u32) + ((page as u32) << 16);
                self.memory
                    .write2(addr, self.get_register(src))
                    .map_err(|x| x.to_string())
            }
            Instruction::StoreByte(src, r1, r2) => {
                let base = self.get_register(r1);
                let page = self.get_register(r2);
                let addr = (base as u32) + ((page as u32) << 16);
                self.memory
                    .write(addr, (self.get_register(src) & 0xff) as u8)
                    .map_err(|x| x.to_string())
            }
            Instruction::SetAndSave(r0, r1, save) => {
                let v = self.get_register(r1); // save this upfront in case r0 == r1!
                self.set_register(save, self.get_register(r0));
                self.set_register(r0, v);
                Ok(())
            }
            Instruction::AddAndSave(r0, r1, save) => {
                let v = self.get_register(r0);
                self.set_register(save, v);
                self.set_register(r0, v + self.get_register(r1));
                Ok(())
            }
            Instruction::Test(r0, r1, op) => {
                let a = self.get_register(r0);
                let b = self.get_register(r1);
                let res = match op {
                    TestOp::Eq => a == b,
                    TestOp::Neq => a != b,
                    TestOp::Lt => a < b,
                    TestOp::Lte => a <= b,
                    TestOp::Gt => a > b,
                    TestOp::Gte => a >= b,
                    TestOp::BothZero => a == 0 && b == 0,
                    TestOp::EitherNonZero => a != 0 || b != 0,
                    TestOp::BothNonZero => a != 0 && b != 0,
                };
                self.set_flag(Flag::Compare, res);
                Ok(())
            }
            Instruction::AddIf(r0, r1, offset) => {
                if self.test_flag(Flag::Compare) {
                    self.set_register(r0, self.get_register(r1) + 2 * (offset.value as u16));
                    self.set_flag(Flag::Compare, false);
                }
                Ok(())
            }
            Instruction::Stack(r0, sp, op) => {
                match op {
                    StackOp::Push => {
                        let v = self.get_register(r0);
                        self.push(sp, v)?
                    }
                    StackOp::Pop => {
                        let v = self.pop(sp)?;
                        self.set_register(r0, v);
                    }
                    StackOp::Peek => {
                        let v = self.peek(sp)?;
                        self.set_register(r0, v)
                    }
                    StackOp::Dup => {
                        let head = self.peek(sp)?;
                        self.push(sp, head)?;
                    }
                    StackOp::Swap => {
                        let a = self.pop(sp)?;
                        let b = self.pop(sp)?;
                        self.push(sp, a)?;
                        self.push(sp, b)?;
                    }
                    StackOp::Rotate => {
                        let a = self.pop(sp)?;
                        let b = self.pop(sp)?;
                        let c = self.pop(sp)?;
                        self.push(sp, a)?;
                        self.push(sp, c)?;
                        self.push(sp, b)?;
                    }
                    StackOp::Add => {
                        let a = self.pop(sp)?;
                        let b = self.pop(sp)?;
                        self.push(sp, a.wrapping_add(b))?;
                    }
                    StackOp::Sub => {
                        let a = self.pop(sp)?;
                        let b = self.pop(sp)?;
                        self.push(sp, a.wrapping_sub(b))?;
                    }
                    StackOp::PushPC => {
                        // TODO: what if bigger than u16?
                        self.push(sp, self.program_counter as u16)?;
                    }
                };
                Ok(())
            }
            Instruction::LoadStackOffset(tgt, sp, word_offset) => {
                let base = self.get_register(sp);
                let addr = base - ((word_offset.value as u16) * 2);
                let stack_value = self.memory.read2(addr as u32).map_err(|x| x.to_string())?;
                self.set_register(tgt, stack_value);
                Ok(())
            }
            Instruction::Jump(addr) => {
                self.program_counter = (addr.value as u32) << 4;
                self.set_flag(Flag::DidJump, true);
                Ok(())
            }
            Instruction::JumpRegister(reg_page, reg_tgt) => {
                let page = self.get_register(reg_page);
                let addr = self.get_register(reg_tgt);
                let full_addr = ((page as u32) << 16) + (addr as u32);
                self.program_counter = full_addr;
                self.set_flag(Flag::DidJump, true);
                Ok(())
            }
            Instruction::BranchIf(offset) => {
                if self.test_flag(Flag::Compare) {
                    self.program_counter = self
                        .program_counter
                        .wrapping_add_signed(offset.as_signed() as i32);
                    self.set_flag(Flag::DidJump, true);
                    self.set_flag(Flag::Compare, false);
                };
                Ok(())
            }
            Instruction::Branch(offset) => {
                self.program_counter = self
                    .program_counter
                    .wrapping_add_signed(offset.as_signed() as i32);
                self.set_flag(Flag::DidJump, true);
                Ok(())
            }
            Instruction::BranchRegisterIf(reg_offset, lit_offset) => {
                if self.test_flag(Flag::Compare) {
                    let offset =
                        (self.get_register(reg_offset) as i32) + (lit_offset.as_signed() as i32);
                    self.program_counter = self.program_counter.wrapping_add_signed(offset);
                    self.set_flag(Flag::DidJump, true);
                    self.set_flag(Flag::Compare, false);
                };
                Ok(())
            }
            Instruction::System(Register::Zero, reg_arg, signal) => {
                let handler = signal_handlers
                    .get(&signal.value)
                    .ok_or(format!("unknown signal: 0x{:X}", signal.value))?;
                let arg = self.get_register(reg_arg);
                handler.handle(self, arg)
            }
            Instruction::System(sig, _, arg) => {
                let sig_value = self.get_register(sig);
                if sig_value > 0xff {
                    Err(format!(
                        "unknown signal: 0x{:X}, must be <= 0xFF",
                        sig_value
                    ))
                } else {
                    let handler = signal_handlers
                        .get(&(sig_value as u8))
                        .ok_or(format!("unknown signal: 0x{:X}", sig_value))?;
                    handler.handle(self, arg.value as u16)
                }
            }
        }?;
        if !self.test_flag(Flag::DidJump) {
            self.program_counter += 2;
            self.set_flag(Flag::DidJump, false);
        };
        Ok(())
    }
}
