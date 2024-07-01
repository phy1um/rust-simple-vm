use std::cell::RefCell;
use std::rc::Rc;

use crate::compile::resolve::UnresolvedInstruction;
use crate::compile::context::Context;

#[allow(dead_code)]
#[derive(Debug)]
pub enum BlockVariable {
    Local(usize),
    Arg(usize),
    Const(u16),
    // TODO: Static(usize),
}

#[allow(dead_code)]
#[derive(Debug, Default)]
pub struct BlockScope {
    parent_func: Rc<RefCell<Block>>,
    pub locals: Vec<(String, usize)>,
}

impl BlockScope {
    pub fn new(parent_func: Rc<RefCell<Block>>) -> Self {
        Self {
            parent_func, 
            locals: Vec::new(),
        }
    }
    pub fn child(&self) -> Self {
        Self {
            parent_func: self.parent_func.clone(),
            locals: self.locals.clone(),
        }
    }

    pub fn define_local(&mut self, s: &str) -> usize {
        let mut fn_block = self.parent_func.borrow_mut();
        let index = fn_block.next_local_index();
        self.locals.push((s.to_owned(), index));
        index
    }

    fn get_local(&self, s: &str) -> Option<usize> {
        for (k, i) in &self.locals {
            if k == s {
                return Some(*i)
            }
        };
        None
    }

    fn get_arg(&self, s: &str) -> Option<usize> {
        let fn_block = self.parent_func.borrow();
         for (i, k) in fn_block.args.iter().enumerate() {
            if k == s {
                return Some(i)
            }
        };
        None
    }

    pub fn get(&self, s: &str) -> Option<BlockVariable> {
        if let Some(i) = self.get_local(s) {
            Some(BlockVariable::Local(i))
        } else if let Some(i) = self.get_arg(s) {
            Some(BlockVariable::Arg(i))
        } else {
            None
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Default)]
pub struct Block {
    pub instructions: Vec<UnresolvedInstruction>, 
    pub offset: u32,
    pub local_count: usize,
    pub args: Vec<String>,
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

    pub fn define_arg(&mut self, s: &str) {
        self.args.push(s.to_owned());
    }

    pub fn next_local_index(&mut self) -> usize {
        let out = self.local_count;
        self.local_count += 1;
        out
    }
}


