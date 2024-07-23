use crate::compile::codegen::block::compile_body;

use std::str::FromStr;

use simplevm::pp;
use simplevm::pp::PreProcessor;
use simplevm::{
    Instruction, InstructionParseError, Literal12Bit, Literal7Bit, Nibble, Register, StackOp,
};

use crate::ast;
use crate::compile::block::Block;
use crate::compile::context::{Context, FunctionDefinition};
use crate::compile::error::CompilerError;
use crate::compile::resolve::{Symbol, Type, UnresolvedInstruction};

pub fn compile(
    program: Vec<ast::TopLevel>,
    offset: u32,
) -> Result<Context, (Context, CompilerError)> {
    let mut ctx = Context::new(offset as usize);
    ctx.load_init(vec![
        UnresolvedInstruction::Instruction(Instruction::Imm(
            Register::SP,
            Literal12Bit::new_checked(0x3ff).unwrap(),
        )),
        UnresolvedInstruction::Instruction(Instruction::ShiftLeft(
            Register::SP,
            Register::SP,
            Nibble::new_checked(4).unwrap(),
        )),
        UnresolvedInstruction::Instruction(Instruction::Stack(
            Register::BP,
            Register::SP,
            StackOp::Push,
        )),
        UnresolvedInstruction::Instruction(Instruction::Stack(
            Register::PC,
            Register::SP,
            StackOp::Push,
        )),
        UnresolvedInstruction::Instruction(Instruction::Add(
            Register::SP,
            Register::Zero,
            Register::BP,
        )),
        UnresolvedInstruction::Imm(Register::PC, Symbol::new("main")),
        UnresolvedInstruction::Instruction(Instruction::Imm(
            Register::C,
            Literal12Bit::new_checked(0xf0).unwrap(),
        )),
        UnresolvedInstruction::Instruction(Instruction::System(
            Register::C,
            Register::Zero,
            Nibble::default(),
        )),
    ]);
    let mut global_map: Vec<(String, Type)> = Vec::new();
    for p in &program {
        match p {
            ast::TopLevel::FunctionDefinition {
                name,
                return_type,
                args,
                ..
            } => {
                ctx.function_defs.insert(
                    name.0.to_string(),
                    FunctionDefinition {
                        args: args
                            .iter()
                            .map(|(name, ty)| {
                                Type::from_ast(&ctx, ty).map(|t| (name.to_string(), t))
                            })
                            .collect::<Result<Vec<_>, _>>()
                            .map_err(|e| (ctx.clone(), e))?,
                        return_type: Type::from_ast(&ctx, return_type)
                            .map_err(|e| (ctx.clone(), e))?,
                    },
                );
            }
            ast::TopLevel::InlineAsm { name, args, .. } => {
                ctx.function_defs.insert(
                    name.0.to_string(),
                    FunctionDefinition {
                        args: args
                            .iter()
                            .map(|(name, ty)| {
                                Type::from_ast(&ctx, ty).map(|t| (name.to_string(), t))
                            })
                            .collect::<Result<Vec<_>, _>>()
                            .map_err(|e| (ctx.clone(), e))?,
                        return_type: Type::Int,
                    },
                );
            }
            ast::TopLevel::GlobalVariable { name, var_type } => global_map.push((
                name.0.to_string(),
                Type::from_ast(&ctx, var_type).map_err(|e| (ctx.clone(), e))?,
            )),
            ast::TopLevel::TypeDefinition { name, alias } => {
                ctx.define_user_type(
                    &name.0,
                    Type::from_ast(&ctx, alias).map_err(|e| (ctx.clone(), e))?,
                );
            }
        }
    }
    // global definition pass
    let global_page_size = global_map
        .iter()
        .fold(0, |acc, (_, t)| acc + t.size_bytes());
    for (k, t) in global_map.iter() {
        ctx.define_global(k, t.clone());
    }
    // codegen pass
    ctx.program_start_offset = offset + (global_page_size as u32);
    let mut program_offset: u32 =
        offset + (global_page_size as u32) + ctx.init.iter().map(|x| x.size()).sum::<u32>();
    for p in program {
        match p {
            ast::TopLevel::FunctionDefinition {
                name, body, args, ..
            } => {
                let block = compile_body(&mut ctx, body, &name.0, program_offset, args)
                    .map_err(|x| (ctx.clone(), x))?;
                block.register_labels(&mut ctx, program_offset);
                let block_size: u32 = block.instructions.iter().map(|x| x.size()).sum();
                let local_count_sym = format!("__internal_{name}_local_count");
                ctx.define(&Symbol::new(&local_count_sym), block.local_offset as u32);
                ctx.functions.push(block);
                program_offset += block_size;
            }
            ast::TopLevel::InlineAsm { name, body, args } => {
                let mut pp = PreProcessor::default();
                let lines = pp
                    .resolve(&body)
                    .map_err(|x| (ctx.clone(), CompilerError::InlineAsm(x.to_string())))?;
                let lines_str = lines
                    .iter()
                    .map(|x| pp.resolve_pass2(x))
                    .collect::<Result<Vec<String>, pp::Error>>()
                    .map_err(|x| (ctx.clone(), CompilerError::InlineAsm(x.to_string())))?;
                let mut block = Block::default();
                for line in lines_str {
                    match Instruction::from_str(&line) {
                        Ok(instruction) => {
                            block
                                .instructions
                                .push(UnresolvedInstruction::Instruction(instruction));
                        }
                        Err(InstructionParseError::Fail(s)) => {
                            return Err((
                                ctx.clone(),
                                CompilerError::InlineAsm(format!(
                                    "failed to parse instruction: {line}: {s}"
                                )),
                            ))
                        }
                        _ => {
                            return Err((
                                ctx.clone(),
                                CompilerError::InlineAsm(format!(
                                    "failed to parse instruction: {line}"
                                )),
                            ))
                        }
                    }
                }
                // function exit
                // load return address -> C
                block.instructions.push(UnresolvedInstruction::Instruction(
                    Instruction::LoadStackOffset(
                        Register::C,
                        Register::BP,
                        Nibble::new_checked(1).unwrap(),
                    ),
                ));
                // load previous SP = BP - 2
                block
                    .instructions
                    .push(UnresolvedInstruction::Instruction(Instruction::Add(
                        Register::BP,
                        Register::Zero,
                        Register::SP,
                    )));
                let offset = -4 - 2 * (args.len() as i8);
                block.instructions.push(UnresolvedInstruction::Instruction(
                    Instruction::AddImmSigned(
                        Register::SP,
                        Literal7Bit::from_signed(-offset).unwrap(),
                    ),
                ));
                // load previous BP
                block.instructions.push(UnresolvedInstruction::Instruction(
                    Instruction::LoadStackOffset(
                        Register::BP,
                        Register::BP,
                        Nibble::new_checked(2).unwrap(),
                    ),
                ));
                block
                    .instructions
                    .push(UnresolvedInstruction::Instruction(Instruction::AddImm(
                        Register::C,
                        Literal7Bit::new_checked(6).unwrap(),
                    )));
                block
                    .instructions
                    .push(UnresolvedInstruction::Instruction(Instruction::Add(
                        Register::C,
                        Register::Zero,
                        Register::PC,
                    )));

                // TODO: copy this for function defs rather than lookup in compile_body
                for (name, arg_type) in args {
                    block.define_arg(
                        &name.0,
                        &Type::from_ast(&ctx, &arg_type).map_err(|e| (ctx.clone(), e))?,
                    );
                }
                block.offset = program_offset;
                let block_size: u32 = block.instructions.iter().map(|x| x.size()).sum();
                program_offset += block_size;
                ctx.define(&Symbol::new(&name.0), block.offset);
                ctx.functions.push(block);
            }
            ast::TopLevel::GlobalVariable { .. } => {}
            ast::TopLevel::TypeDefinition { .. } => {}
        }
    }
    Ok(ctx)
}
