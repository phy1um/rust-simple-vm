use std::str::FromStr;

use crate::*;
use wasm_bindgen::prelude::*;

struct JSMemCallback {
    on_read: js_sys::Function,
    on_write: js_sys::Function,
}

impl JSMemCallback {
    fn new(on_read: js_sys::Function, on_write: js_sys::Function) -> Self {
        Self { on_read, on_write }
    }
}

impl Addressable for JSMemCallback {
    fn read(&mut self, addr: u32) -> Result<u8, MemoryError> {
        let this = JsValue::null();
        if let Ok(res) = self.on_read.call1(&this, &JsValue::from(addr)) {
            let value = res.as_f64().ok_or(MemoryError::InternalMapperError(addr))?;
            Ok(value as u8)
        } else {
            Err(MemoryError::InternalMapperError(addr))
        }
    }

    fn write(&mut self, addr: u32, value: u8) -> Result<(), MemoryError> {
        let this = JsValue::null();
        self.on_write
            .call2(&this, &JsValue::from(addr), &JsValue::from(value));
        Ok(())
    }

    fn zero_all(&mut self) -> Result<(), MemoryError> {
        Ok(())
    }
}

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
        let mut pp = pp::PreProcessor::default();
        pp::macros::setup_std_macros(&mut pp);
        Self { pp }
    }

    #[wasm_bindgen]
    pub fn resolve(&mut self, input: &str) -> Result<Vec<String>, String> {
        let lines = self
            .pp
            .resolve(input)
            .map_err(|_| "failed to resolve".to_string())?;
        let mut out: Vec<String> = Vec::new();
        for l in lines {
            out.push(
                self.pp
                    .resolve_pass2(&l)
                    .map_err(|_| format!("error @ line {}", l.get_line_number()))?,
            );
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
    pub fn new(tick_every_secs: f32) -> Self {
        let mut m = Machine::new();
        m.halt = true;
        m.define_handler(0xf0, signal_halt);
        m.define_handler(0x1, signal_write);
        Self {
            m,
            tick_rate: tick_every_secs,
            tick_acc: 0.0,
            on_run_instruction: None,
        }
    }

    #[wasm_bindgen]
    pub fn map_memory_array(
        &mut self,
        start: usize,
        size: usize,
    ) {
        self.m.map(start, size, Box::new(LinearMemory::new(size)));
    }

    #[wasm_bindgen]
    pub fn map_memory_func(
        &mut self,
        start: usize,
        size: usize,
        on_read: js_sys::Function,
        on_write: js_sys::Function,
    ) {
        self.m
            .map(start, size, Box::new(JSMemCallback::new(on_read, on_write)));
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
        while self.tick_acc > self.tick_rate {
            self.tick_acc -= self.tick_rate;
            self.step()?;
            if self.m.halt {
                self.tick_acc = 0.0;
                return Ok(());
            }
        }
        Ok(())
    }

    #[wasm_bindgen]
    pub fn step(&mut self) -> Result<(), String> {
        if let Some(f) = &self.on_run_instruction {
            let this = JsValue::null();
            let ins = self
                .m
                .memory
                .read2(self.m.get_register(Register::PC) as u32)
                .unwrap();
            let _ = f.call1(&this, &JsValue::from(ins));
        };
        self.m.step()
    }

    #[wasm_bindgen]
    pub fn state(&self) -> String {
        self.m.state()
    }

    #[wasm_bindgen]
    pub fn read_memory(&mut self, addr: u32) -> Result<u16, String> {
        self.m.memory.read2(addr).map_err(|x| x.to_string())
    }

    #[wasm_bindgen]
    pub fn write_memory(&mut self, addr: u32, value: u16) -> Result<(), String> {
        self.m.memory.write2(addr, value).map_err(|x| x.to_string())
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
