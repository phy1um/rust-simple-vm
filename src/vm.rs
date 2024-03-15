use std::collections::HashMap;

use crate::memory::{Addressable, LinearMemory};
use crate::op::Instruction;
use crate::register::Register;

type SignalFunction = fn(&mut Machine, arg: u16) -> Result<(), String>;

pub struct Machine {
    registers: [u16; 8],
    signal_handlers: HashMap<u8, SignalFunction>,
    pub halt: bool,
    pub memory: Box<dyn Addressable>,
}

impl Machine {
    pub fn new() -> Self {
        Self {
            registers: [0; 8],
            signal_handlers: HashMap::new(),
            halt: false,
            memory: Box::new(LinearMemory::new(8 * 1024)),
        }
    }

    pub fn state(&self) -> String {
        format!(
            "A: {} | B: {} | C: {} | M: {}
SP: {} | PC: {} | BP: {}",
            self.get_register(Register::A),
            self.get_register(Register::B),
            self.get_register(Register::C),
            self.get_register(Register::M),
            self.get_register(Register::SP),
            self.get_register(Register::PC),
            self.get_register(Register::BP),
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

    pub fn define_handler(&mut self, index: u8, f: SignalFunction) {
        self.signal_handlers.insert(index, f);
    }

    pub fn pop(&mut self) -> Result<u16, String> {
        let sp = self.registers[Register::SP as usize] - 2;
        if let Some(v) = self.memory.read2(sp) {
            self.registers[Register::SP as usize] -= 2;
            Ok(v)
        } else {
            Err(format!("memory read fault @ 0x{:X}", sp))
        }
    }

    pub fn push(&mut self, v: u16) -> Result<(), String> {
        let sp = self.registers[Register::SP as usize];
        if !self.memory.write2(sp, v) {
            return Err(format!("memory write fault @ 0x{:X}", sp));
        }
        self.registers[Register::SP as usize] += 2;
        Ok(())
    }

    /*
    fn set_flag(&mut self, flag: RegisterFlag) {
        self.registers[Register::FLAGS as usize] |= flag as u16;
    }

    fn test_flag(&self, flag: RegisterFlag) -> bool {
        (self.registers[Register::FLAGS as usize] & (flag as u16)) != 0
    }
    */

    pub fn step(&mut self) -> Result<(), String> {
        let pc = self.get_register(Register::PC);
        let instruction = self
            .memory
            .read2(pc)
            .ok_or(format!("pc read fail @ 0x{:X}", pc))?;
        self.set_register(Register::PC, pc + 2);
        let op = Instruction::try_from(instruction)?;
        println!("running {}", op);
        match op {
            Instruction::Imm(reg, value) => {
                self.set_register(reg, value);
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
                self.set_register(dst, a - b);
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
            } /*
              Instruction::Push(v) => self.push(v.into()),
              Instruction::PopRegister(r) => {
                  let value = self.pop()?;
                  self.registers[r as usize] = value;
                  Ok(())
              }
              Instruction::PushRegister(r) => {
                  self.push(self.registers[r as usize])?;
                  Ok(())
              }
              Instruction::SetRegister(a, b) => {
                  self.registers[a as usize] = self.registers[b as usize];
                  Ok(())
              }
              Instruction::LoadARegister(r) => {
                  let addr = self.registers[r as usize];
                  match self.memory.read2(addr) {
                      Some(x) => {
                          self.registers[Register::A as usize] = x;
                          Ok(())
                      }
                      None => Err(format!("failed to read {addr:X}")),
                  }
              }
              Instruction::LoadAImm(b) => match self.memory.read2(b as u16) {
                  Some(x) => {
                      self.registers[Register::A as usize] = x;
                      Ok(())
                  }
                  None => Err(format!("failed to read {b:X}")),
              },
              Instruction::LoadBRegister(r) => {
                  let addr = self.registers[r as usize];
                  match self.memory.read2(addr) {
                      Some(x) => {
                          self.registers[Register::B as usize] = x;
                          Ok(())
                      }
                      None => Err(format!("failed to read {addr:X}")),
                  }
              }
              Instruction::LoadBImm(b) => match self.memory.read2(b as u16) {
                  Some(x) => {
                      self.registers[Register::B as usize] = x;
                      Ok(())
                  }
                  None => Err(format!("failed to read {b:X}")),
              },
              Instruction::LoadCRegister(r) => {
                  let addr = self.registers[r as usize];
                  match self.memory.read2(addr) {
                      Some(x) => {
                          self.registers[Register::C as usize] = x;
                          Ok(())
                      }
                      None => Err(format!("failed to read {addr:X}")),
                  }
              }
              Instruction::LoadCImm(b) => match self.memory.read2(b as u16) {
                  Some(x) => {
                      self.registers[Register::C as usize] = x;
                      Ok(())
                  }
                  None => Err(format!("failed to read {b:X}")),
              },
              Instruction::AddStack => {
                  let a = self.pop()?;
                  let b = self.pop()?;
                  self.push(a + b)
              }
              Instruction::AddRegister(r1, r2) => {
                  self.registers[r1 as usize] += self.registers[r2 as usize];
                  Ok(())
              }
              Instruction::SubStack => {
                  let a = self.pop()?;
                  let b = self.pop()?;
                  self.push(a - b)
              }
              Instruction::SubRegister(r1, r2) => {
                  self.registers[r1 as usize] -= self.registers[r2 as usize];
                  Ok(())
              }
              Instruction::IfZero(r) => {
                  if self.registers[r as usize] == 0 {
                      self.set_flag(RegisterFlag::Compare);
                  }
                  Ok(())
              }
              Instruction::BranchImm(x) => {
                  if self.test_flag(RegisterFlag::Compare) {
                      self.registers[Register::PC as usize] = pc.wrapping_add_signed(x as i16);
                  }
                  Ok(())
              }
              Instruction::BranchRegister(r) => {
                  if self.test_flag(RegisterFlag::Compare) {
                      self.registers[Register::PC as usize] = self.registers[r as usize];
                  }
                  Ok(())
              }
                          */
        }
    }
}
