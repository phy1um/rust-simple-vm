use std::cell::RefCell;
use std::rc::Rc;

use crate::compile::context::{Context, Global};
use crate::compile::resolve::{Symbol, Type, UnresolvedInstruction};

#[allow(dead_code)]
#[derive(Debug)]
pub enum BlockVariable {
    Local(usize, Type),
    Arg(usize, Type),
    Const(u16),
    Global(usize, Type),
    // TODO: Static(usize),
}

#[derive(Debug)]
pub struct LoopLabels {
    pub top: Symbol,
    pub bottom: Symbol,
}

#[derive(Debug, Clone)]
pub struct LocalDefinition {
    name: String,
    offset: usize,
    var_type: Type,
}

impl LocalDefinition {
    fn new(name: &str, offset: usize, var_type: &Type) -> Self {
        Self {
            name: name.to_owned(),
            offset,
            var_type: var_type.clone(),
        }
    }
}

#[derive(Debug, Default)]
pub struct BlockScope {
    parent_func: Rc<RefCell<Block>>,
    pub loop_labels: Option<LoopLabels>,
    pub locals: Vec<LocalDefinition>,
}

impl BlockScope {
    pub fn new(parent_func: Rc<RefCell<Block>>) -> Self {
        Self {
            parent_func,
            ..Self::default()
        }
    }

    pub fn child(&self) -> Self {
        Self {
            parent_func: self.parent_func.clone(),
            locals: self.locals.clone(),
            ..Self::default()
        }
    }

    pub fn child_in_loop(&self, top: Symbol, bottom: Symbol) -> Self {
        Self {
            parent_func: self.parent_func.clone(),
            locals: self.locals.clone(),
            loop_labels: Some(LoopLabels { top, bottom }),
        }
    }

    pub fn define_local(&mut self, s: &str, t: &Type) -> usize {
        let mut fn_block = self.parent_func.borrow_mut();
        let offset = fn_block.allocate_local_space(t);
        println!("declare local: {s} {t} @ {offset:X}");
        self.locals.push(LocalDefinition::new(s, offset, t));
        offset
    }

    fn get_local(&self, s: &str) -> Option<(usize, Type)> {
        for def in &self.locals {
            if def.name == s {
                return Some((def.offset, def.var_type.clone()));
            }
        }
        None
    }

    fn get_arg(&self, s: &str) -> Option<(usize, Type)> {
        let fn_block = self.parent_func.borrow();
        for (i, (k, t)) in fn_block.args.iter().enumerate() {
            if k == s {
                return Some((i, t.clone()));
            }
        }
        None
    }

    pub fn get(&self, ctx: &Context, s: &str) -> Option<BlockVariable> {
        if let Some((i, t)) = self.get_local(s) {
            Some(BlockVariable::Local(i, t))
        } else if let Some((i, t)) = self.get_arg(s) {
            Some(BlockVariable::Arg(i, t))
        } else if let Some(Global { address, var_type }) = ctx.globals.get(s) {
            Some(BlockVariable::Global(*address, var_type.clone()))
        } else {
            None
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct Block {
    pub instructions: Vec<UnresolvedInstruction>,
    pub offset: u32,
    pub local_count: usize,
    pub local_offset: usize,
    pub args: Vec<(String, Type)>,
}

impl Block {
    pub fn register_labels(&self, ctx: &mut Context, function_offset: u32) {
        let mut offset = 0;
        for ins in &self.instructions {
            if let UnresolvedInstruction::Label(s) = ins {
                ctx.define(s, function_offset + (offset as u32));
            } else {
                offset += 2;
            }
        }
    }

    pub fn define_arg(&mut self, s: &str, t: &Type) {
        self.args.push((s.to_owned(), t.clone()));
    }

    fn allocate_local_space(&mut self, t: &Type) -> usize {
        let out = self.local_offset;
        let size = t.size_bytes();
        let align = if size > 1 && out % 2 != 0 { 1 } else { 0 };
        self.local_offset += t.size_bytes() + align;
        out + align
    }
}
