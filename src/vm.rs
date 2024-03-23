use std::collections::HashMap;

use crate::memory::{Addressable, LinearMemory};
use crate::op::{Instruction};
use crate::op_fields::{StackOp, TestOp};
use crate::register::{Flag, Register};

type SignalFunction = fn(&mut Machine, arg: u16) -> Result<(), String>;

pub struct Machine {
    registers: [u16; 8],
    signal_handlers: HashMap<u8, SignalFunction>,
    flags: u16,
    pub halt: bool,
    pub memory: Box<dyn Addressable>,
}

impl Default for Machine {
    fn default() -> Self {
        Self {
            registers: [0; 8],
            signal_handlers: HashMap::new(),
            halt: false,
            flags: 0,
            memory: Box::new(LinearMemory::new(8 * 1024)),
        }
    }
}

impl Machine {
    pub fn new(memory_words: usize) -> Self {
        Self {
            memory: Box::new(LinearMemory::new(2 * memory_words)),
            ..Self::default()
        }
    }

    pub fn reset(&mut self) {
        self.memory.zero_all();
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

    pub fn define_handler(&mut self, index: u8, f: SignalFunction) {
        self.signal_handlers.insert(index, f);
    }

    pub fn pop(&mut self, stack_pointer_register: Register) -> Result<u16, String> {
        let sp = self.get_register(stack_pointer_register) - 2;
        if let Some(v) = self.memory.read2(sp as u32) {
            self.set_register(stack_pointer_register, sp);
            Ok(v)
        } else {
            Err(format!("memory read fault @ 0x{:X}", sp))
        }
    }

    pub fn peek(&mut self, stack_pointer_register: Register) -> Result<u16, String> {
        let sp = self.get_register(stack_pointer_register) - 2;
        if let Some(v) = self.memory.read2(sp as u32) {
            Ok(v)
        } else {
            Err(format!("memory read fault @ 0x{:X}", sp))
        }
    }

    pub fn push(&mut self, stack_pointer_register: Register, v: u16) -> Result<(), String> {
        let sp = self.get_register(stack_pointer_register);
        if !self.memory.write2(sp as u32, v) {
            return Err(format!("memory write fault @ 0x{:X}", sp));
        }
        self.set_register(stack_pointer_register, sp + 2);
        Ok(())
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

    pub fn step(&mut self) -> Result<(), String> {
        let pc = self.get_register(Register::PC);
        let instruction = self
            .memory
            .read2(pc as u32)
            .ok_or(format!("pc read fail @ 0x{:X}", pc))?;
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
            Instruction::Load(r0, r1, r2) => {
                let base = self.get_register(r1);
                let page = self.get_register(r2);
                let addr = (base as u32) + ((page as u32) << 16);
                let w = self
                    .memory
                    .read2(addr)
                    .ok_or(format!("failed to read word @ {}", addr))?;
                self.set_register(r0, w);
                Ok(())
            }
            Instruction::Store(r0, r1, r2) => {
                let base = self.get_register(r0);
                let page = self.get_register(r1);
                let addr = (base as u32) + ((page as u32) << 16);
                match self.memory.write2(addr, self.get_register(r2)) {
                    true => Ok(()),
                    false => Err(format!(
                        "failed to write word {} @ {}",
                        self.get_register(r2),
                        addr
                    )),
                }
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
                        self.push(sp, a + b)?;
                    }
                    StackOp::Sub => {
                        let a = self.pop(sp)?;
                        let b = self.pop(sp)?;
                        self.push(sp, a - b)?;
                    }
                };
                Ok(())
            }
            Instruction::LoadStackOffset(tgt, sp, word_offset) => {
                let base = self.get_register(sp);
                let addr = base - ((word_offset.value as u16) * 2);
                self.set_register(
                    tgt,
                    self.memory.read2(addr as u32).ok_or(format!(
                        "invalid stack read: stack={}, offset={:X} @ {:X}",
                        sp, word_offset.value, addr
                    ))?,
                );
                Ok(())
            }
            Instruction::System(Register::Zero, reg_arg, signal) => {
                let sig_fn = self
                    .signal_handlers
                    .get(&signal.value)
                    .ok_or(format!("unknown signal: 0x{:X}", signal.value))?;
                let arg = self.get_register(reg_arg);
                sig_fn(self, arg)
            }
            Instruction::System(sig, _, arg) => {
                let sig_value = self.get_register(sig);
                if sig_value > 0xff {
                    Err(format!(
                        "unknown signal: 0x{:X}, must be <= 0xFF",
                        sig_value
                    ))
                } else {
                    let sig_fn = self
                        .signal_handlers
                        .get(&(sig_value as u8))
                        .ok_or(format!("unknown signal: 0x{:X}", sig_value))?;
                    sig_fn(self, arg.value as u16)
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
