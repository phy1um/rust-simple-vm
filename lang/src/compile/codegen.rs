use simplevm::{Instruction, Register, Literal12Bit, Literal7Bit, Nibble, StackOp, TestOp};

use crate::compile::context::{Context, FunctionDefinition};
use crate::compile::block::{Block, BlockVariable};
use crate::compile::error::CompilerError;
use crate::compile::resolve::{UnresolvedInstruction, Symbol};
use crate::ast;
use crate::compile::util::*;

fn compile_block<'a>(ctx: &mut Context<'a>, block: &mut Block<'a>, statements: Vec<ast::Statement>) -> Result<Vec<UnresolvedInstruction>, CompilerError> {
    let mut out = Vec::new();
    for s in statements {
        match s {
            ast::Statement::While{cond, body}=> {
                let block_identifier = gensym(rand::thread_rng());
                let label_test = Symbol::new(&(block_identifier.to_string() + "_while_lbl_test"));
                let label_out = Symbol::new(&(block_identifier + "_while_lbl_out"));
                let mut compiled_cond = compile_expression(block, cond)?;
                out.push(UnresolvedInstruction::Label(label_test.clone()));
                out.append(&mut compiled_cond);
                out.push(UnresolvedInstruction::Instruction(
                    Instruction::Stack(Register::C, Register::SP, StackOp::Pop)
                ));
                out.push(UnresolvedInstruction::Instruction(
                    Instruction::Test(Register::C, Register::Zero, TestOp::EitherNonZero)
                ));
                out.push(UnresolvedInstruction::Instruction(
                    Instruction::AddIf(Register::PC, Register::PC, Nibble::new_checked(2).unwrap())
                ));
                out.push(UnresolvedInstruction::Imm(Register::PC, label_out.clone()));
                out.append(&mut compile_block(ctx, block, body)?);
                out.push(UnresolvedInstruction::Imm(Register::PC, label_test.clone()));
                out.push(UnresolvedInstruction::Label(label_out));
            }
            ast::Statement::If{cond, body, else_body} => {
                let block_identifier = gensym(rand::thread_rng());
                let label_true = Symbol::new(&(block_identifier.to_string() + "_if_lbl_true"));
                let label_out = Symbol::new(&(block_identifier + "_if_lbl_out"));
                let mut compiled_cond = compile_expression(block, cond)?;
                out.append(&mut compiled_cond);
                // test if condition is FALSY
                out.push(UnresolvedInstruction::Instruction(
                    Instruction::Stack(Register::C, Register::SP, StackOp::Pop)
                ));
                out.push(UnresolvedInstruction::Instruction(
                    Instruction::Test(Register::C, Register::Zero, TestOp::BothZero)
                ));
                out.push(UnresolvedInstruction::Instruction(
                    Instruction::AddIf(Register::PC, Register::PC, Nibble::new_checked(2).unwrap())
                ));
                out.push(UnresolvedInstruction::Imm(Register::PC, label_true.clone()));
                // condition == FALSE
                if let Some(b) = else_body {
                    out.append(&mut compile_block(ctx, block, b)?);
                };
                out.push(UnresolvedInstruction::Imm(Register::PC, label_out.clone()));
                // condition == TRUE 
                out.push(UnresolvedInstruction::Label(label_true));
                out.append(&mut compile_block(ctx, block, body)?);
                out.push(UnresolvedInstruction::Imm(Register::PC, label_out.clone()));
                out.push(UnresolvedInstruction::Label(label_out.clone()));
            }
            ast::Statement::Declare(id, _t, Some(expr)) => {
                if block.scope.get(&id.0).is_some() {
                    return Err(CompilerError::VariableAlreadyDefined(id.0.to_string()))
                }
                let local_index = block.scope.define_local(&id.0);
                // put expression on top of stack
                let mut compiled_expr = compile_expression(block, *expr)?;
                out.append(&mut compiled_expr);
                out.push(UnresolvedInstruction::Instruction(
                        Instruction::Stack(Register::C, Register::SP, StackOp::Pop)));
                out.push(UnresolvedInstruction::Instruction(
                        Instruction::Add(Register::BP, Register::Zero, Register::B)));
                out.push(UnresolvedInstruction::Instruction(
                        Instruction::AddImm(Register::B, Literal7Bit::new_checked(local_index as u8 * 2).unwrap())));
                out.push(UnresolvedInstruction::Instruction(
                        Instruction::StoreWord(Register::B, Register::Zero, Register::C))); 
            }
            ast::Statement::Declare(id, _t, None) => {
                if block.scope.get(&id.0).is_some() {
                    return Err(CompilerError::VariableAlreadyDefined(id.0.to_string()))
                }
                block.scope.define_local(&id.0);
            }
            ast::Statement::Assign(id, expr) => {
                if let Some(BlockVariable::Local(index)) = block.scope.get(&id.0) {
                    let mut compiled_expr = compile_expression(block, *expr)?;
                    out.append(&mut compiled_expr);
                    out.push(UnresolvedInstruction::Instruction(
                            Instruction::Stack(Register::C, Register::SP, StackOp::Pop)));
                    out.push(UnresolvedInstruction::Instruction(
                            Instruction::Add(Register::BP, Register::Zero, Register::B)));
                    out.push(UnresolvedInstruction::Instruction(
                            Instruction::AddImm(Register::B, Literal7Bit::new_checked(index as u8 * 2).unwrap())));
                    out.push(UnresolvedInstruction::Instruction(
                            Instruction::StoreWord(Register::B, Register::Zero, Register::C))); 
                } else {
                    return Err(CompilerError::VariableUndefined(id.0.to_string()))
                }
            }
            ast::Statement::Return(expr) => {
                let mut compiled_expr = compile_expression(block, expr)?;
                out.append(&mut compiled_expr);
                // return in the A register
                out.push(UnresolvedInstruction::Instruction(
                        Instruction::Stack(Register::A, Register::SP, StackOp::Pop)));
            }
        }
    };
    Ok(out)
}

fn compile_body<'a>(ctx: &mut Context<'a>, statements: Vec<ast::Statement>, name: &str, offset: u32) -> Result<Block<'a>, CompilerError> {
    let mut block = Block {
        offset,
        ..Block::default()
    };
    block.instructions.push(UnresolvedInstruction::Label(Symbol::new(name)));
    for (name, _type) in &ctx.function_defs.get(name).unwrap().args {
        block.scope.define_arg(name); 
    }
    // function setup
    let local_count_sym = format!("__internal_{name}_local_count");
    block.instructions.push(UnresolvedInstruction::AddImm(Register::SP, Symbol::new(&local_count_sym)));
    let mut compiled = compile_block(ctx, &mut block, statements)?;
    block.instructions.append(&mut compiled);
    // function exit
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
        ast::Expression::Variable(s) => {
            if let Some(v) = block.scope.get(&s) {
                match v {
                    BlockVariable::Local(i) => Ok(vec![
                        UnresolvedInstruction::Instruction(
                            Instruction::Add(Register::BP, Register::Zero, Register::C)),
                        UnresolvedInstruction::Instruction(
                            Instruction::AddImm(Register::C, Literal7Bit::new_checked(i as u8 *2).unwrap())),
                        UnresolvedInstruction::Instruction(
                            Instruction::LoadWord(Register::C, Register::C, Register::Zero)),
                        UnresolvedInstruction::Instruction(
                            Instruction::Stack(Register::C, Register::SP, StackOp::Push)),
                    ]),
                    BlockVariable::Arg(i) => Ok(vec![
                        UnresolvedInstruction::Instruction(
                            Instruction::LoadStackOffset(Register::C, Register::BP, Nibble::new_checked(i as u8+3).unwrap())),
                        UnresolvedInstruction::Instruction(
                            Instruction::Stack(Register::C, Register::SP, StackOp::Push)),
                    ]),
                    _ => panic!("block variable type unhandled"),
                }
            } else {
                Err(CompilerError::VariableUndefined(s))
            }
        }
        ast::Expression::FunctionCall(id, args) => {
            let mut out = Vec::new(); 
            for a in args.iter().rev() {
                out.append(&mut compile_expression(block, a.clone())?);
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
                // functions return in register A, so push this
                UnresolvedInstruction::Instruction(
                    Instruction::Stack(Register::A, Register::SP, StackOp::Push)
                ),
            ]);
            Ok(out)
        },
        ast::Expression::BinOp(e0, e1, op) => {
            let mut out = Vec::new();
            out.append(&mut compile_expression(block, *e1)?);
            // expression 1 is on top of stack
            out.append(&mut compile_expression(block, *e0)?);
            // stack = [rv0, rv1]
            match op {
                ast::BinOp::Add => 
                    out.push(UnresolvedInstruction::Instruction(
                        Instruction::Stack(Register::Zero, Register::SP, StackOp::Add))),
                ast::BinOp::Subtract => 
                    out.push(UnresolvedInstruction::Instruction(
                        Instruction::Stack(Register::Zero, Register::SP, StackOp::Sub))),
                ast::BinOp::LessThanEqual => {
                     out.push(UnresolvedInstruction::Instruction(
                        Instruction::Stack(Register::B, Register::SP, StackOp::Pop)));
                     out.push(UnresolvedInstruction::Instruction(
                        Instruction::Stack(Register::C, Register::SP, StackOp::Pop)));
                     out.push(UnresolvedInstruction::Instruction(
                        Instruction::Test(Register::B, Register::C, TestOp::Lte)));
                     out.push(UnresolvedInstruction::Instruction(
                        Instruction::Add(Register::Zero, Register::Zero, Register::C)));
                     out.push(UnresolvedInstruction::Instruction(
                        Instruction::AddIf(Register::C, Register::Zero, Nibble::new_checked(1).unwrap())));
                     out.push(UnresolvedInstruction::Instruction(
                        Instruction::Stack(Register::C, Register::SP, StackOp::Push)));
                }
                _ => panic!("unimplemented binop {op}"),
            }
            Ok(out)
        }
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
                    args: args.iter().map(|(name, ty)| (name.to_string(), ty.clone())).collect::<Vec<_>>(),
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
                ctx.functions.push(block);
                program_offset += block_size;
            }
        }
    };
    Ok(ctx)
}


