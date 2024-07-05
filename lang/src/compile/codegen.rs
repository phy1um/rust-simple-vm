use std::cell::RefCell;
use std::rc::Rc;
use std::str::FromStr;
use std::collections::HashMap;

use simplevm::{Instruction, InstructionParseError, Register, Literal12Bit, Literal7Bit, Nibble, StackOp, TestOp};
use simplevm::pp;
use simplevm::pp::PreProcessor;

use crate::compile::context::{Context, FunctionDefinition};
use crate::compile::block::{Block, BlockVariable, BlockScope, LoopLabels};
use crate::compile::error::CompilerError;
use crate::compile::resolve::{UnresolvedInstruction, Symbol, Type, type_of};
use crate::ast;
use crate::compile::util::*;

fn compile_block(ctx: &mut Context, mut scope: BlockScope, statements: Vec<ast::Statement>) -> Result<Vec<UnresolvedInstruction>, CompilerError> {
    let mut out = Vec::new();
    for s in statements {
        match s {
            ast::Statement::Break => {
                if let Some(LoopLabels{ref bottom, ..}) = scope.loop_labels {
                    out.push(UnresolvedInstruction::Imm(Register::PC, bottom.clone()));
                } else {
                    return Err(CompilerError::BreakNotInLoop)
                }
            }
            ast::Statement::Continue => {
                if let Some(LoopLabels{ref top, ..}) = scope.loop_labels {
                    out.push(UnresolvedInstruction::Imm(Register::PC, top.clone()));
                } else {
                    return Err(CompilerError::ContinueNotInLoop)
                }
            }
            ast::Statement::While{cond, body}=> {
                let block_identifier = gensym(rand::thread_rng());
                let label_test = Symbol::new(&(block_identifier.to_string() + "_while_lbl_test"));
                let label_out = Symbol::new(&(block_identifier + "_while_lbl_out"));
                out.push(UnresolvedInstruction::Label(label_test.clone()));
                let mut compiled_cond = compile_expression(ctx, &mut scope, cond)?;
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
                let child_scope = scope.child_in_loop(label_test.clone(), label_out.clone());
                out.append(&mut compile_block(ctx, child_scope, body)?);
                out.push(UnresolvedInstruction::Imm(Register::PC, label_test.clone()));
                out.push(UnresolvedInstruction::Label(label_out));
            }
            ast::Statement::If{cond, body, else_body} => {
                let block_identifier = gensym(rand::thread_rng());
                let label_true = Symbol::new(&(block_identifier.to_string() + "_if_lbl_true"));
                let label_out = Symbol::new(&(block_identifier + "_if_lbl_out"));
                let mut compiled_cond = compile_expression(ctx, &mut scope, cond)?;
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
                    let child_scope = scope.child();
                    out.append(&mut compile_block(ctx, child_scope, b)?);
                };
                out.push(UnresolvedInstruction::Imm(Register::PC, label_out.clone()));
                // condition == TRUE 
                out.push(UnresolvedInstruction::Label(label_true));
                let child_scope = scope.child();
                out.append(&mut compile_block(ctx, child_scope, body)?);
                out.push(UnresolvedInstruction::Imm(Register::PC, label_out.clone()));
                out.push(UnresolvedInstruction::Label(label_out.clone()));
            }
            ast::Statement::Declare(id, t, Some(expr)) => {
                if scope.get(ctx, &id.0).is_some() {
                    return Err(CompilerError::VariableAlreadyDefined(id.0.to_string()))
                }

                // type check
                let expr_type = type_of(ctx, &scope, &expr);
                let tt: Type = t.into();
                if !tt.can_assign_from(&expr_type) {
                    return Err(CompilerError::TypeAssign{from: expr_type, to: tt});
                }

                let local_index = scope.define_local(&id.0);
                // put expression on top of stack
                let mut compiled_expr = compile_expression(ctx, &mut scope, *expr)?;
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
                if scope.get(ctx, &id.0).is_some() {
                    return Err(CompilerError::VariableAlreadyDefined(id.0.to_string()))
                }
                scope.define_local(&id.0);
            }
            ast::Statement::Assign(id, expr) => {
                if let Some(bv) = scope.get(ctx, &id.0) {
                    match bv {
                        BlockVariable::Local(index, _) => {
                            let mut compiled_expr = compile_expression(ctx, &mut scope, *expr)?;
                            out.append(&mut compiled_expr);
                            out.push(UnresolvedInstruction::Instruction(
                                    Instruction::Stack(Register::C, Register::SP, StackOp::Pop)));
                            out.push(UnresolvedInstruction::Instruction(
                                    Instruction::Add(Register::BP, Register::Zero, Register::B)));
                            out.push(UnresolvedInstruction::Instruction(
                                    Instruction::AddImm(Register::B, Literal7Bit::new_checked(index as u8 * 2).unwrap())));
                            out.push(UnresolvedInstruction::Instruction(
                                    Instruction::StoreWord(Register::B, Register::Zero, Register::C))); 
                        }
                        BlockVariable::Global(addr, t) => {
                            // type check
                            let expr_type = type_of(ctx, &scope, &expr);
                            let tt: Type = t.into();
                            if !tt.can_assign_from(&expr_type) {
                                return Err(CompilerError::TypeAssign{from: expr_type, to: tt});
                            }

                            let mut compiled_expr = compile_expression(ctx, &mut scope, *expr)?;
                            out.append(&mut compiled_expr);
                            out.push(UnresolvedInstruction::Instruction(
                                    Instruction::Stack(Register::C, Register::SP, StackOp::Pop)));
                            out.extend(load_address_to(addr, Register::B, Register::M));
                            out.push(UnresolvedInstruction::Instruction(
                                        Instruction::StoreWord(Register::B, Register::Zero, Register::C)));
                        }
                        _ => todo!("unimplemented"),
                    }
                } else {
                    return Err(CompilerError::VariableUndefined(id.0.to_string()))
                }
            }
            ast::Statement::Return(expr) => {
                let mut compiled_expr = compile_expression(ctx, &mut scope, expr)?;
                out.append(&mut compiled_expr);
                // return in the A register
                out.push(UnresolvedInstruction::Instruction(
                        Instruction::Stack(Register::A, Register::SP, StackOp::Pop)));
            }
        }
    };
    Ok(out)
}

fn compile_body(ctx: &mut Context, statements: Vec<ast::Statement>, name: &str, offset: u32) -> Result<Block, CompilerError> {
    let mut block = Block {
        offset,
        ..Block::default()
    };
    block.instructions.push(UnresolvedInstruction::Label(Symbol::new(name)));
    for (name, _type) in &ctx.function_defs.get(name).unwrap().args {
        block.define_arg(name); 
    }
    // function setup
    let local_count_sym = format!("__internal_{name}_local_count");
    block.instructions.push(UnresolvedInstruction::AddImm(Register::SP, Symbol::new(&local_count_sym)));
    let cell = Rc::new(RefCell::new(block));
    let mut compiled = compile_block(ctx, BlockScope::new(cell.clone()), statements)?;
    {
        let mut block = cell.take();
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
}

fn compile_expression(ctx: &Context, scope: &mut BlockScope, expr: ast::Expression) -> Result<Vec<UnresolvedInstruction>, CompilerError> {
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
            if let Some(v) = scope.get(ctx, &s) {
                match v {
                    BlockVariable::Local(i, _) => Ok(vec![
                        UnresolvedInstruction::Instruction(
                            Instruction::Add(Register::BP, Register::Zero, Register::C)),
                        UnresolvedInstruction::Instruction(
                            Instruction::AddImm(Register::C, Literal7Bit::new_checked(i as u8 *2).unwrap())),
                        UnresolvedInstruction::Instruction(
                            Instruction::LoadWord(Register::C, Register::C, Register::Zero)),
                        UnresolvedInstruction::Instruction(
                            Instruction::Stack(Register::C, Register::SP, StackOp::Push)),
                    ]),
                    BlockVariable::Arg(i, _) => Ok(vec![
                        UnresolvedInstruction::Instruction(
                            Instruction::LoadStackOffset(Register::C, Register::BP, Nibble::new_checked(i as u8+3).unwrap())),
                        UnresolvedInstruction::Instruction(
                            Instruction::Stack(Register::C, Register::SP, StackOp::Push)),
                    ]),
                    BlockVariable::Global(addr, _) => {
                        let mut out = Vec::new();
                        out.extend(load_address_to(addr, Register::B, Register::M));
                        out.push(UnresolvedInstruction::Instruction(
                            Instruction::LoadWord(Register::C, Register::B, Register::Zero)));
                        out.push(UnresolvedInstruction::Instruction(
                            Instruction::Stack(Register::C, Register::SP, StackOp::Push)));
                        Ok(out)
                    }
                    _ => panic!("block variable type unhandled"),
                }
            } else {
                Err(CompilerError::VariableUndefined(s))
            }
        }
        ast::Expression::FunctionCall(id, args) => {
            let mut out = Vec::new(); 
            for a in args.iter().rev() {
                out.append(&mut compile_expression(ctx, scope, a.clone())?);
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
            out.append(&mut compile_expression(ctx, scope, *e1)?);
            // expression 1 is on top of stack
            out.append(&mut compile_expression(ctx, scope, *e0)?);
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

pub fn compile(program: Vec<ast::TopLevel>, offset: u32) -> Result<Context, (Context, CompilerError)> {
    let mut ctx = Context::new(offset as usize);
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
    let mut global_map: HashMap<String, Type> = HashMap::new();
    for p in &program {
        match p {
            ast::TopLevel::FunctionDefinition{name, return_type, args, ..} => {
                ctx.function_defs.insert(name.0.to_string(), FunctionDefinition{
                    args: args.iter().map(|(name, ty)| (name.to_string(), ty.clone().into())).collect::<Vec<_>>(),
                    return_type: return_type.clone().into(),
                });
            }
            ast::TopLevel::InlineAsm{name, args, ..} => {
                ctx.function_defs.insert(name.0.to_string(), FunctionDefinition{
                    args: args.iter().map(|(name, ty)| (name.to_string(), ty.clone().into())).collect::<Vec<_>>(),
                    return_type: Type::Int,
                });
            }
            ast::TopLevel::GlobalVariable{name, var_type} => {
                global_map.insert(name.0.to_string(), var_type.clone().into());
            }
        }
    };
    // global definition pass
    let global_page_size = global_map.values().fold(0, |acc, t| acc + t.size_bytes());
    for (k, t) in global_map.iter() {
        ctx.define_global(k, *t);
    }
    // codegen pass
    ctx.program_start_offset = offset + (global_page_size as u32);
    let mut program_offset: u32 = offset + (global_page_size as u32) + ctx.init.iter().map(|x| x.size()).sum::<u32>();
    for p in program {
        match p {
            ast::TopLevel::FunctionDefinition{name, body, ..} => {
                let block = compile_body(&mut ctx, body, &name.0, program_offset).map_err(|x| (ctx.clone(), x))?;
                block.register_labels(&mut ctx, program_offset);
                let block_size: u32 = block.instructions.iter().map(|x| x.size()).sum();
                let local_count_sym = format!("__internal_{name}_local_count");
                ctx.define(&Symbol::new(&local_count_sym), block.local_count as u32 *2);
                ctx.functions.push(block);
                program_offset += block_size;
            }
            ast::TopLevel::InlineAsm{name, body, args} => {
                let mut pp = PreProcessor::default();  
                let lines = pp.resolve(&body).map_err(|x| (ctx.clone(), CompilerError::InlineAsm(x.to_string())))?;
                let lines_str = lines.iter().map(|x| pp.resolve_pass2(x)).collect::<Result<Vec<String>, pp::Error>>().map_err(|x| (ctx.clone(), CompilerError::InlineAsm(x.to_string())))?;
                let mut block = Block::default(); 
                for line in lines_str {
                    match Instruction::from_str(&line) {
                        Ok(instruction) => {
                            block.instructions.push(UnresolvedInstruction::Instruction(instruction)); 
                        }
                        Err(InstructionParseError::Fail(s)) => 
                            return Err((ctx.clone(), CompilerError::InlineAsm(format!("failed to parse instruction: {line}: {s}")))),
                        _ => return Err((ctx.clone(), CompilerError::InlineAsm(format!("failed to parse instruction: {line}")))), 
                    }
                }
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

                    // TODO: copy this for function defs rather than lookup in compile_body
                for (name, _type) in args {
                    block.define_arg(&name.0); 
                }
                block.offset = program_offset;
                let block_size: u32 = block.instructions.iter().map(|x| x.size()).sum();
                program_offset += block_size;
                ctx.define(&Symbol::new(&name.0), block.offset);
                ctx.functions.push(block);
            }
            ast::TopLevel::GlobalVariable{..} => {},
        }
    };
    Ok(ctx)
}

fn load_address_to(addr: usize, target_register: Register, _page_register: Register) -> Vec<UnresolvedInstruction> {
    let mut out = Vec::new();
    if addr <= 0xfff {
        out.push(UnresolvedInstruction::Instruction(
                Instruction::Imm(
                    target_register, 
                    Literal12Bit::new_checked(addr as u16).unwrap())));
    } else if addr <= 0xffff {
        out.push(UnresolvedInstruction::Instruction(
                Instruction::Imm(
                    target_register, 
                    Literal12Bit::new_checked((addr&0xfff0) as u16 >> 4).unwrap())));
        out.push(UnresolvedInstruction::Instruction(
                Instruction::ShiftLeft(
                    target_register,
                    target_register,
                    Nibble::new_checked(4).unwrap())));
        out.push(UnresolvedInstruction::Instruction(
                Instruction::AddImm(
                    target_register,
                    Literal7Bit::new_checked((addr&0xf) as u8).unwrap())));
    } else {
        todo!("paged addressing not implemented");
    }
    out
}
