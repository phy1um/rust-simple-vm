
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

#[wasm_bindgen]
struct PreProcessor {
    pp: pp::PreProcessor,
}

#[wasm_bindgen(js_class= PreProcessor)]
impl PreProcessor {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        let mut pp = pp::PreProcessor::new();
        pp::macros::setup_std_macros(&mut pp);
        Self { pp }
    }

    #[wasm_bindgen]
    pub fn resolve(&mut self, input: &str) -> Result<Vec<String>, String> {
        let lines = self.pp.resolve(input).map_err(|_| "failed to resolve".to_string())?;
        let mut out: Vec<String> = Vec::new();
        for l in lines {
            out.push(self.pp.resolve_pass2(&l).map_err(|_| format!("error @ line {}", l.get_line_number()))?);
        }
        Ok(out)
    }
}

#[wasm_bindgen(js_name = VM)]
struct JSMachine {
    m: Machine,
    on_run_instruction: Option<js_sys::Function>,
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
            on_run_instruction: None,
        }
    }

    #[wasm_bindgen]
    pub fn write_program(&mut self, b: &[u8]) -> Result<(), String> {
        let _ = self.m.memory.load_from_vec(b, 0);
        Ok(())
    }

    #[wasm_bindgen]
    pub fn instruction_callback(&mut self, f: js_sys::Function) {
        self.on_run_instruction = Some(f);
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
            self.step()?;
        }
        Ok(())
    }

    #[wasm_bindgen]
    pub fn step(&mut self) -> Result<(), String> {
        if let Some(f) = &self.on_run_instruction {
            let this = JsValue::null();
            let ins = self.m.memory.read2(self.m.get_register(Register::PC) as u32).unwrap();
            let _ = f.call1(&this, &JsValue::from(ins));
        };
        self.m.step()
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

#[wasm_bindgen]
pub fn dissasemble_instruction(i: u16) -> Result<String, String> {
    let ins = Instruction::try_from(i)?;
    Ok(format!("{}", ins))
}

