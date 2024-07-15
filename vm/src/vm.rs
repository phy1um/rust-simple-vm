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
    }

    pub fn state(&self) -> String {
        format!(
            "A: {} | B: {} | C: {} | M: {}
SP: {} | PC: {} | BP: {}
Flags: {:016b}",
            self.get_register(Register::A),
            self.get_register(Register::B),
            self.get_register(Register::C),
            self.get_register(Register::M),
            self.get_register(Register::SP),
            self.get_register(Register::PC),
            self.get_register(Register::BP),
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
        if r == Register::PC {
            self.set_flag(Flag::DidJump, true);
        }
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
        let pc = self.get_register(Register::PC);
        let instruction = self.memory.read2(pc as u32).map_err(|x| x.to_string())?;
        self.set_flag(Flag::DidJump, false);
        let op = Instruction::try_from(instruction)?;
        println!("running {}", op);
        match op {
            Instruction::Invalid => Err("0 instruction".to_string()),
            Instruction::Imm(reg, v) => {
                self.set_register(reg, v.value);
                Ok(())
            }
            Instruction::Add(r0, r1, dst) => {
                let a = self.get_register(r0);
                let b = self.get_register(r1);
                self.set_register(dst, a + b);
                Ok(())
            }
            Instruction::Sub(r0, r1, dst) => {
                let a = self.get_register(r0);
                let b = self.get_register(r1);
                self.set_register(dst, a.wrapping_sub(b));
                Ok(())
            }
            Instruction::Mul(r0, r1, dst) => {
                let a = self.get_register(r0);
                let b = self.get_register(r1);
                self.set_register(dst, a * b);
                Ok(())
            }
            Instruction::AddImm(r, i) => {
                self.set_register(r, self.get_register(r) + (i.value as u16));
                Ok(())
            }
            Instruction::AddImmSigned(r, i) => {
                let raw_register_value = self.get_register(r);
                let imm_signed = i.as_signed();
                unsafe {
                    let register_signed: i16 = std::mem::transmute(raw_register_value);
                    self.set_register(
                        r,
                        std::mem::transmute(register_signed + (imm_signed as i16)),
                    );
                }
                Ok(())
            }
            Instruction::ShiftLeft(r0, r1, offset) => {
                let base = self.get_register(r0);
                self.set_register(r1, base << (offset.value as u16));
                Ok(())
            }
            Instruction::ShiftRightLogical(r0, r1, offset) => {
                let base = self.get_register(r0);
                self.set_register(r1, base >> (offset.value as u16));
                Ok(())
            }
            Instruction::ShiftRightArithmetic(r0, r1, offset) => {
                let base = self.get_register(r0);
                unsafe {
                    let as_signed: i16 = std::mem::transmute(base);
                    let shifted: i16 = as_signed >> (offset.value as u16);
                    let res: u16 = std::mem::transmute(shifted);
                    self.set_register(r1, res);
                }
                Ok(())
            }
            Instruction::LoadWord(r0, r1, r2) => {
                let base = self.get_register(r1);
                let page = self.get_register(r2);
                let addr = (base as u32) + ((page as u32) << 16);
                let w = self.memory.read2(addr).map_err(|x| x.to_string())?;
                self.set_register(r0, w);
                Ok(())
            }
            Instruction::LoadByte(r0, r1, r2) => {
                let base = self.get_register(r1);
                let page = self.get_register(r2);
                let addr = (base as u32) + ((page as u32) << 16);
                let w = self.memory.read(addr).map_err(|x| x.to_string())?;
                self.set_register(r0, w as u16);
                Ok(())
            }
            Instruction::StoreWord(r0, r1, r2) => {
                let base = self.get_register(r0);
                let page = self.get_register(r1);
                let addr = (base as u32) + ((page as u32) << 16);
                self.memory
                    .write2(addr, self.get_register(r2))
                    .map_err(|x| x.to_string())
            }
            Instruction::StoreByte(r0, r1, r2) => {
                let base = self.get_register(r0);
                let page = self.get_register(r1);
                let addr = (base as u32) + ((page as u32) << 16);
                self.memory
                    .write(addr, (self.get_register(r2) & 0xff) as u8)
                    .map_err(|x| x.to_string())
            }
            Instruction::JumpOffset(b) => {
                self.set_register(Register::PC, self.get_register(Register::PC) + b.value);
                Ok(())
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
            self.set_register(Register::PC, pc + 2);
            self.set_flag(Flag::DidJump, false);
        };
        Ok(())
    }
}
