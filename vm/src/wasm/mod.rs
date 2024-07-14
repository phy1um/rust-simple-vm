use std::str::FromStr;

use crate::*;
use serde::{Deserialize, Serialize};
use wasm_bindgen::convert::FromWasmAbi;
use wasm_bindgen::prelude::*;

mod asm;
pub use asm::*;

struct JSSignalHandler {
    handler: js_sys::Function,
    result_callback: js_sys::Function,
}

fn js_error_to_string(js: JsValue) -> String {
    if let Some(s) = js.as_string() {
        s
    } else {
        let x: &js_sys::Object = js.dyn_ref().unwrap();
        x.to_string().into()
    }
}

impl SignalHandler for JSSignalHandler {
    fn handle(&self, vm: &mut VM, arg: u16) -> Result<(), String> {
        let this = JsValue::null();
        let res = self
            .handler
            .call1(&this, &JsValue::from(arg))
            .map_err(js_error_to_string)?;
        if res.is_undefined() {
            log("no return".to_string());
            Ok(())
        } else {
            let res_ptr = js_sys::Reflect::get(&res, &JsValue::from_str("__wbg_ptr"))
                .map_err(|_| "failed to get __wbg_ptr".to_string())?;
            let res_ptr_u32 = res_ptr
                .as_f64()
                .ok_or(JsValue::NULL)
                .map_err(|_| "__wbg_ptr is not number".to_string())?
                as u32;
            log(format!("return something: {res_ptr_u32}"));
            unsafe {
                let actions: VMActionSet = FromWasmAbi::from_abi(res_ptr_u32);
                log(format!("got actions: {}", actions.0.len()));
                let mut out = Vec::<u16>::new();
                for action in actions.0 {
                    match action {
                        VMAction::MemoryRead(addr) => {
                            let v = vm.memory.read(addr)?;
                            out.push(v as u16);
                        }
                        VMAction::MemoryWrite(addr, v) => {
                            vm.memory.write(addr, v)?;
                        }
                        VMAction::RegisterRead(r) => out.push(vm.get_register(r)),
                        VMAction::RegisterWrite(r, value) => vm.set_register(r, value),
                        VMAction::Halt => vm.halt = true,
                    }
                }
                let js_vec = serde_wasm_bindgen::to_value(&out)
                    .map_err(|x| format!("serde to wasm: {x}"))?;
                self.result_callback
                    .call1(&this, &js_vec)
                    .map_err(|_| format!("failed to run mem read callback"))?;
            }
            Ok(())
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
enum VMAction {
    MemoryRead(u32),
    MemoryWrite(u32, u8),
    RegisterRead(Register),
    RegisterWrite(Register, u16),
    Halt,
}

#[wasm_bindgen]
#[derive(Debug, Default, Serialize, Deserialize)]
struct VMActionSet(Vec<VMAction>);

#[wasm_bindgen]
impl VMActionSet {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn read_memory(&mut self, addr: u32) {
        self.0.push(VMAction::MemoryRead(addr));
    }

    pub fn write_memory(&mut self, addr: u32, value: u8) {
        self.0.push(VMAction::MemoryWrite(addr, value));
    }

    pub fn get_register(&mut self, name: &str) -> Result<(), String> {
        self.0
            .push(VMAction::RegisterRead(Register::from_str(name)?));
        Ok(())
    }

    pub fn set_register(&mut self, name: &str, value: u16) -> Result<(), String> {
        self.0
            .push(VMAction::RegisterWrite(Register::from_str(name)?, value));
        Ok(())
    }

    pub fn halt(&mut self) {
        self.0.push(VMAction::Halt);
    }
}

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
        let res = self
            .on_read
            .call1(&this, &JsValue::from(addr))
            .map_err(|x| MemoryError::InternalMapperWithMessage(addr, js_error_to_string(x)))?;
        let value = res.as_f64().ok_or(MemoryError::InternalMapperWithMessage(
            addr,
            "result not number".to_string(),
        ))?;
        Ok(value as u8)
    }

    fn write(&mut self, addr: u32, value: u8) -> Result<(), MemoryError> {
        let this = JsValue::null();
        self.on_write
            .call2(&this, &JsValue::from(addr), &JsValue::from(value))
            .map_err(|_| MemoryError::InternalMapperError(addr))?;
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
        let mut m = Machine::default();
        m.vm.halt = true;
        Self {
            m,
            tick_rate: tick_every_secs,
            tick_acc: 0.0,
            on_run_instruction: None,
        }
    }

    #[wasm_bindgen]
    pub fn map_memory_array(&mut self, start: usize, size: usize) {
        let _ = self.m.map(start, size, Box::new(LinearMemory::new(size)));
    }

    #[wasm_bindgen]
    pub fn map_memory_func(
        &mut self,
        start: usize,
        size: usize,
        on_read: js_sys::Function,
        on_write: js_sys::Function,
    ) {
        let _ = self
            .m
            .map(start, size, Box::new(JSMemCallback::new(on_read, on_write)));
    }

    #[wasm_bindgen]
    pub fn write_program(&mut self, b: &[u8], offset: u32) -> Result<(), String> {
        let _ = self.m.vm.memory.load_from_vec(b, offset);
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
        self.m.vm.halt = b;
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
        if self.m.is_halt() {
            return Ok(());
        }
        self.tick_acc += dt;
        while self.tick_acc > self.tick_rate {
            self.tick_acc -= self.tick_rate;
            self.step()?;
            if self.m.is_halt() {
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
                .vm
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
        self.m.vm.memory.read2(addr).map_err(|x| x.to_string())
    }

    #[wasm_bindgen]
    pub fn write_memory(&mut self, addr: u32, value: u16) -> Result<(), String> {
        self.m
            .vm
            .memory
            .write2(addr, value)
            .map_err(|x| x.to_string())
    }

    #[wasm_bindgen]
    pub fn bind_handler(
        &mut self,
        id: u8,
        handler: js_sys::Function,
        result_callback: js_sys::Function,
    ) {
        self.m.define_handler(
            id,
            JSSignalHandler {
                handler,
                result_callback,
            },
        );
    }
}
