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
pub struct BlockScope<'a> {
    pub locals: Vec<String>,
    args: Vec<String>,
    parent: Option<&'a BlockScope<'a>>,
}

impl<'a> BlockScope<'a> {
    pub fn define_local(&mut self, s: &str) -> usize {
        self.locals.push(s.to_owned());
        self.locals.len() - 1
    }

    pub fn define_arg(&mut self, s: &str) {
        self.args.push(s.to_owned());
    }

    fn get_local(&self, s: &str) -> Option<usize> {
        for (i, k) in self.locals.iter().enumerate() {
            if k == s {
                return Some(i)
            }
        };
        None
    }

    fn get_arg(&self, s: &str) -> Option<usize> {
         for (i, k) in self.args.iter().enumerate() {
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
        } else if let Some(par) = self.parent {
            par.get(s)
        } else {
            None
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Default)]
pub struct Block<'a> {
    pub instructions: Vec<UnresolvedInstruction>, 
    pub scope: BlockScope<'a>,
    pub offset: u32,
}

impl Block<'_> {
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
}


