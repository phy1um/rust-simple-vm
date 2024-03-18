
use std::str::FromStr;

use wasm_bindgen::prelude::*;
use crate::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: String);

    #[wasm_bindgen(js_namespace = console, js_name = log)]
    fn log_u16(v: u16);
}

fn signal_write(_: &mut Machine, v: u16) -> Result<(), String> {
    log_u16(v); 
    Ok(())
}

fn signal_halt(m: &mut Machine, _: u16) -> Result<(), String> {
    m.halt = true;
    Ok(())
}

#[wasm_bindgen(js_name = VM)]
struct JSMachine {
    m: Machine,
    tick_rate: f32,
    tick_acc: f32,
}

#[wasm_bindgen(js_class = VM)]
impl JSMachine {
    #[wasm_bindgen(constructor)]
    pub fn new(memory_size: usize) -> Self {
        let mut m = Machine::new(memory_size);
        m.halt = true;
        m.define_handler(0xf0, signal_halt);
        m.define_handler(0x1, signal_write);
        Self {
            m,
            tick_rate: 0.1,
            tick_acc: 0.0,
        }
    }

    #[wasm_bindgen]
    pub fn write_program(&mut self, b: &[u8]) -> Result<(), String> {
        let _ = self.m.memory.load_from_vec(b, 0);
        Ok(())
    }

    #[wasm_bindgen]
    pub fn reset(&mut self) {
        self.m.reset();
    }

    #[wasm_bindgen]
    pub fn set_halt(&mut self, b: bool) {
        self.m.halt = b;
    }

    #[wasm_bindgen]
    pub fn set_register(&mut self, register_name: &str, value: u16) -> Result<(), String> {
        let register = Register::from_str(register_name)?;
        self.m.set_register(register, value);
        Ok(())
    }

    #[wasm_bindgen]
    pub fn get_register(&self, register_name: &str) -> Result<u16, String> {
        let register = Register::from_str(register_name)?;
        Ok(self.m.get_register(register))
    }

    #[wasm_bindgen]
    pub fn tick(&mut self, dt: f32) -> Result<(), String> {
        if self.m.halt {
            return Ok(());
        }
        self.tick_acc += dt;
        if self.tick_acc > self.tick_rate {
            self.tick_acc -= self.tick_rate;
            self.m.step()?;
        }
        Ok(())
    }

    #[wasm_bindgen]
    pub fn state(&self) -> String {
        self.m.state() 
    }
}

#[wasm_bindgen]
pub fn assemble_line(line: &str) -> Result<u16, String> {
    let ins = Instruction::from_str(line).map_err(|x| format!("failed to parse: {:?}", x))?;
    Ok(ins.encode_u16())
}

