use std::collections::HashMap;

use crate::memory::{LinearMemory, Addressable};
use crate::op::{OpCode, Instruction};
use crate::register::Register;

fn parse_instruction_arg(ins: u16) -> u8 {
    ((ins & 0xff00) >> 8) as u8
}
/**
 * instruction = [ 0 0 0 0 0 0 0 0 | 0 0 0 0 0 0 0 0 ]
 *                 OPERATOR        | ARG(s)
 *                                 | 8 bit literal
 *                                 | REG1  | REG2
*/
fn parse_instruction(ins: u16) -> Result<Instruction, String> {
    let op = (ins & 0xff) as u8; 
    match OpCode::from_u8(op).ok_or(format!("unknown op: {:X}", op))? {
        OpCode::Nop => Ok(Instruction::Nop),
        OpCode::Push => {
            let arg = parse_instruction_arg(ins);
            Ok(Instruction::Push(arg))
        },
        OpCode::PopRegister => {
            let reg = (ins&0xf00) >> 8;
            Register::from_u8(reg as u8)
                .ok_or(format!("unknown register 0x{:X}", reg))
                .map(|r| Instruction::PopRegister(r))
        },
        OpCode::PushRegister => {
            let reg = (ins&0xf00) >> 8;
            Register::from_u8(reg as u8)
                .ok_or(format!("unknown register 0x{:X}", reg))
                .map(|r| Instruction::PushRegister(r))
        }
        OpCode::AddStack => {
            Ok(Instruction::AddStack)
        },
        OpCode::AddRegister => {
            let reg1_raw = (ins&0xf00)>>8;
            let reg2_raw = (ins&0xf000)>>12;
            let reg1 = Register::from_u8(reg1_raw as u8)
                .ok_or(format!("unknown register 0x{:X}", reg1_raw))?;
            let reg2 = Register::from_u8(reg2_raw as u8)
                .ok_or(format!("unknown register 0x{:X}", reg2_raw))?;
            Ok(Instruction::AddRegister(reg1, reg2))
        }
        OpCode::Signal => {
            let arg = parse_instruction_arg(ins);
            Ok(Instruction::Signal(arg))
        },
    }
}

type SignalFunction = fn(&mut Machine) -> Result<(), String>;

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
            memory: Box::new(LinearMemory::new(8*1024)),
        }
    }

    pub fn state(&self) -> String {
        format!("A: {} | B: {} | C: {} | M: {}
SP: {} | PC: {} | BP: {}
FLAGS: {:X}", 
            self.get_register(Register::A),
            self.get_register(Register::B),
            self.get_register(Register::C),
            self.get_register(Register::M),
            self.get_register(Register::SP),
            self.get_register(Register::PC),
            self.get_register(Register::BP),
            self.get_register(Register::FLAGS))
    }

    pub fn get_register(&self, r: Register) -> u16 {
        self.registers[r as usize]
    }
    
    pub fn set_register(&mut self, r: Register, v: u16) {
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

    pub fn step(&mut self) -> Result<(), String> {
        let pc = self.registers[Register::PC as usize];
        let instruction = self.memory.read2(pc).ok_or(format!("pc read fail @ 0x{:X}", pc))?;
        self.registers[Register::PC as usize] = pc + 2;
        let op = parse_instruction(instruction)?;
        match op {
            Instruction::Nop => Ok(()),
            Instruction::Push(v) => {
                self.push(v.into())
            },
            Instruction::PopRegister(r) => {
                let value = self.pop()?;
                self.registers[r as usize] = value;
                Ok(())
            },
            Instruction::PushRegister(r) => {
                self.push(self.registers[r as usize])?;
                Ok(())
            }
            Instruction::AddStack => {
                let a = self.pop()?;
                let b = self.pop()?;
                self.push(a + b)
            },
            Instruction::AddRegister(r1, r2) => {
                self.registers[r1 as usize] += self.registers[r2 as usize];
                Ok(())
            },
            Instruction::Signal(signal) => {
                let sig_fn = self.signal_handlers.get(&signal)
                    .ok_or(format!("unknown signal: 0x{:X}", signal))?;
                sig_fn(self)
            },
        }
    }
}

