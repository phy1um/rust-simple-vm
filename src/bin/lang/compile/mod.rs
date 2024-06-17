use simplevm::{Instruction, Register, Literal12Bit, Literal7Bit, Literal10Bit, Nibble, StackOp};
use crate::ast;

use std::collections::HashMap;

#[derive(Debug)]
pub enum CompilerError {
    LiteralOutOfBounds(u32, u32, u32),
    UnknownSymbol(Symbol),
    VariableAlreadyDefined(String),
    VariableUndefined(String),
}

#[derive(Debug, Default)]
pub struct Context<'a> {
    symbols: HashMap<String, u32>,
    functions: HashMap<String, Block<'a>>,
    function_defs: HashMap<String, FunctionDefinition>,
    init: Vec<UnresolvedInstruction>,
}

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
    locals: Vec<String>,
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

impl Context<'_> {
    pub fn get(&self, s: &Symbol) -> Result<u32, CompilerError> {
        self.symbols.get(&s.0).ok_or(CompilerError::UnknownSymbol(s.clone())).copied()
    }

    pub fn define(&mut self, s: &Symbol, v: u32) {
        self.symbols.insert(s.0.to_owned(), v);
    }

    pub fn load_init(&mut self, prog: Vec<UnresolvedInstruction>) {
        self.init = prog;
    }

    pub fn get_instructions(&self) -> Result<Vec<Instruction>, CompilerError> {
        let mut out = Vec::new();
        for ins in &self.init {
            if let Some(c) = ins.resolve(self)? {
                out.push(c);
            }
        }
        for (_, func) in &self.functions {
            for ins in &func.instructions {
                 if let Some(c) = ins.resolve(self)? {
                    out.push(c);
                }
            }
        }
        Ok(out)
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum UnresolvedInstruction {
    Instruction(Instruction),
    Imm(Register, Symbol),
    AddImm(Register, Symbol),
    AddImmSigned(Register, Symbol),
    JumpOffset(Symbol),
    Label(Symbol),
}

impl UnresolvedInstruction {
    pub fn resolve(&self, ctx: &Context) -> Result<Option<Instruction>, CompilerError> {
        match self {
            Self::Instruction(i) => Ok(Some(i.clone())),
            Self::Imm(reg, sym) => ctx.get(sym).and_then(|v| Literal12Bit::new_checked(v as u16).map_err(|_| CompilerError::LiteralOutOfBounds(v, 0, 0xfff)).map(|x| Some(Instruction::Imm(reg.clone(), x)))),
            Self::AddImm(reg, sym) => ctx.get(sym).and_then(|v| Literal7Bit::new_checked(v as u8).map_err(|_| CompilerError::LiteralOutOfBounds(v, 0, 0x7f)).map(|x| Some(Instruction::AddImm(reg.clone(), x)))),
            Self::AddImmSigned(reg, sym) => ctx.get(sym).and_then(|v| Literal7Bit::new_checked(v as u8).map_err(|_| CompilerError::LiteralOutOfBounds(v, 0, 0x7f)).map(|x| Some(Instruction::AddImmSigned(reg.clone(), x)))),
            Self::JumpOffset(sym) => ctx.get(sym).and_then(|v| Literal10Bit::new_checked(v as u16).map_err(|_| CompilerError::LiteralOutOfBounds(v, 0, 0x3ff)).map(|x| Some(Instruction::JumpOffset(x)))),
            Self::Label(_) => Ok(None),
        }
    }

    pub fn size(&self) -> u32 {
        match self {
            Self::Label(_) => 0,
            _ => 2,
        }
    }

}

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol(pub String);

impl Symbol {
    pub fn new(s: &str) -> Self {
        Self(s.to_owned())
    }
}

#[allow(dead_code)]
#[derive(Debug, Default)]
pub struct Block<'a> {
    instructions: Vec<UnresolvedInstruction>, 
    scope: BlockScope<'a>,
    offset: u32,
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct FunctionDefinition {
    arg_types: Vec<ast::Type>,
    return_type: ast::Type,
}

impl Block<'_> {
    pub fn register_labels(&self, ctx: &mut Context, function_offset: u32) {
        for (offset, ins) in self.instructions.iter().enumerate() {
            if let UnresolvedInstruction::Label(s) = ins {
                ctx.define(s, function_offset + (offset as u32));
            }
        }
    }
}

fn compile_body<'a>(_x: &mut Context<'a>, statements: Vec<ast::Statement>, name: &str, offset: u32) -> Result<Block<'a>, CompilerError> {
    let mut block = Block {
        offset,
        ..Block::default()
    };
    block.instructions.push(UnresolvedInstruction::Label(Symbol::new(name)));
    let local_count_sym = format!("__internal_{name}_local_count");
    block.instructions.push(UnresolvedInstruction::AddImm(Register::SP, Symbol::new(&local_count_sym)));
    for s in statements {
        match s {
            ast::Statement::Declare(id, _t, Some(expr)) => {
                if block.scope.get(&id.0).is_some() {
                    return Err(CompilerError::VariableAlreadyDefined(id.0.to_string()))
                }
                let local_index = block.scope.define_local(&id.0);
                // put expression on top of stack
                let mut compiled_expr = compile_expression(&mut block, *expr)?;
                block.instructions.append(&mut compiled_expr);
                block.instructions.push(UnresolvedInstruction::Instruction(
                        Instruction::Stack(Register::C, Register::SP, StackOp::Pop)));
                block.instructions.push(UnresolvedInstruction::Instruction(
                        Instruction::Add(Register::BP, Register::Zero, Register::B)));
                block.instructions.push(UnresolvedInstruction::Instruction(
                        Instruction::AddImm(Register::B, Literal7Bit::new_checked(local_index as u8 * 2).unwrap())));
                block.instructions.push(UnresolvedInstruction::Instruction(
                        Instruction::Store(Register::B, Register::Zero, Register::C))); 
            }
            ast::Statement::Declare(id, _t, None) => {
                if block.scope.get(&id.0).is_some() {
                    return Err(CompilerError::VariableAlreadyDefined(id.0.to_string()))
                }
                block.scope.define_local(&id.0);
            }
            ast::Statement::Assign(id, expr) => {
                if let Some(BlockVariable::Local(index)) = block.scope.get(&id.0) {
                    let mut compiled_expr = compile_expression(&mut block, *expr)?;
                    block.instructions.append(&mut compiled_expr);
                    block.instructions.push(UnresolvedInstruction::Instruction(
                            Instruction::Stack(Register::C, Register::SP, StackOp::Pop)));
                    block.instructions.push(UnresolvedInstruction::Instruction(
                            Instruction::Add(Register::BP, Register::Zero, Register::B)));
                    block.instructions.push(UnresolvedInstruction::Instruction(
                            Instruction::AddImm(Register::B, Literal7Bit::new_checked(index as u8 * 2).unwrap())));
                    block.instructions.push(UnresolvedInstruction::Instruction(
                            Instruction::Store(Register::B, Register::Zero, Register::C))); 
                } else {
                    return Err(CompilerError::VariableUndefined(id.0.to_string()))
                }

            }
        }
    };
    // load return address -> C
    block.instructions.push(UnresolvedInstruction::Instruction(
            Instruction::LoadStackOffset(Register::C, Register::BP, Nibble::new_checked(1).unwrap()))); 
    // load previous SP = BP - 2
    block.instructions.push(UnresolvedInstruction::Instruction(
            Instruction::Add(Register::BP, Register::Zero, Register::SP)));
    block.instructions.push(UnresolvedInstruction::Instruction(
            Instruction::AddImmSigned(Register::SP, Literal7Bit::from_signed(-2).unwrap())));
    // load previous BP
    block.instructions.push(UnresolvedInstruction::Instruction(
            Instruction::LoadStackOffset(Register::BP, Register::BP, Nibble::new_checked(2).unwrap()))); 
    block.instructions.push(UnresolvedInstruction::Instruction(
            Instruction::AddImm(Register::C, Literal7Bit::new_checked(6).unwrap())));
    block.instructions.push(UnresolvedInstruction::Instruction(
            Instruction::Add(Register::C, Register::Zero, Register::PC)));
    Ok(block)
}

fn compile_expression(block: &mut Block, expr: ast::Expression) -> Result<Vec<UnresolvedInstruction>, CompilerError> {
    match expr {
        ast::Expression::LiteralInt(i) => Ok(vec![
            UnresolvedInstruction::Instruction(Instruction::Imm(Register::C, Literal12Bit::new_checked(i as u16).unwrap())),
            UnresolvedInstruction::Instruction(Instruction::Stack(Register::C, Register::SP, StackOp::Push)),

        ]),
        ast::Expression::LiteralChar(c) => Ok(vec![
            UnresolvedInstruction::Instruction(Instruction::Imm(Register::C, Literal12Bit::new_checked(c as u16).unwrap())),
            UnresolvedInstruction::Instruction(Instruction::Stack(Register::C, Register::SP, StackOp::Push)),
        ]),
        ast::Expression::FunctionCall(id, args) => {
            let mut out = Vec::new(); 
            for a in args {
                out.append(&mut compile_expression(block, a)?);
            }
            out.append(&mut vec![
                UnresolvedInstruction::Instruction(
                    Instruction::Stack(Register::BP, Register::SP, StackOp::Push)
                ),
                UnresolvedInstruction::Instruction(
                    Instruction::Stack(Register::PC, Register::SP, StackOp::Push)
                ),
                UnresolvedInstruction::Instruction(
                    Instruction::Add(Register::SP, Register::Zero, Register::BP)
                ),
                UnresolvedInstruction::Imm(Register::PC, Symbol::new(&id.0)),
                UnresolvedInstruction::Instruction(
                    Instruction::Stack(Register::A, Register::SP, StackOp::Push)
                ),
            ]);
            Ok(out)
        },
   }
}

pub fn compile<'a>(program: Vec<ast::TopLevel>, offset: u32) -> Result<Context<'a>, CompilerError> {
    let mut ctx = Context::default();
    ctx.load_init(vec![
        UnresolvedInstruction::Instruction(
            Instruction::Imm(Register::SP, Literal12Bit::new_checked(0x3ff).unwrap())
        ),
        UnresolvedInstruction::Instruction(
            Instruction::ShiftLeft(Register::SP, Register::SP, Nibble::new_checked(4).unwrap())
        ),
        UnresolvedInstruction::Instruction(
            Instruction::Stack(Register::BP, Register::SP, StackOp::Push)
        ),
        UnresolvedInstruction::Instruction(
            Instruction::Stack(Register::PC, Register::SP, StackOp::Push)
        ),
        UnresolvedInstruction::Instruction(
            Instruction::Add(Register::SP, Register::Zero, Register::BP)
        ),
        UnresolvedInstruction::Imm(Register::PC, Symbol::new("main")),
        UnresolvedInstruction::Instruction(
            Instruction::Imm(Register::C, Literal12Bit::new_checked(0xf0).unwrap())
        ),
        UnresolvedInstruction::Instruction(
            Instruction::System(Register::C, Register::Zero, Nibble::default())
        ),
    ]);
    for p in &program {
        match p {
            ast::TopLevel::FunctionDefinition{name, return_type, args, ..} => {
                ctx.function_defs.insert(name.0.to_string(), FunctionDefinition{
                    arg_types: args.iter().map(|(_, ty)| ty.clone()).collect::<Vec<_>>(),
                    return_type: return_type.clone(),
                });
            }
        }
    };
    let mut program_offset: u32 = offset + ctx.init.iter().map(|x| x.size()).sum::<u32>();
    for p in program {
        match p {
            ast::TopLevel::FunctionDefinition{name, body, ..} => {
                let block = compile_body(&mut ctx, body, &name.0, program_offset)?;
                block.register_labels(&mut ctx, program_offset);
                let block_size: u32 = block.instructions.iter().map(|x| x.size()).sum();
                let local_count_sym = format!("__internal_{name}_local_count");
                ctx.define(&Symbol::new(&local_count_sym), block.scope.locals.len() as u32 *2);
                ctx.functions.insert(name.0, block);
                program_offset += block_size;
            }
        }
    };
    Ok(ctx)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_empty_compile() {
        let _ = compile(Vec::new()).unwrap();
    }
}
