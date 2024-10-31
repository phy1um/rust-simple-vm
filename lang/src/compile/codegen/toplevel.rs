use crate::compile::codegen::block::compile_body;

use std::collections::HashMap;

use simplevm::binfmt::SectionMode;
use simplevm::pp::{Chunk, PreProcessor};
use simplevm::{
    resolve::UnresolvedInstruction, Instruction, Literal12Bit, Literal7Bit, Nibble, Register,
    StackOp,
};

use crate::ast;
use crate::compile::block::Block;
use crate::compile::context::{Context, FunctionDefinition};
use crate::compile::error::CompilerError;
use crate::compile::resolve::{Symbol, Type};

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
            Register::Zero,
            Register::SP,
            StackOp::PushPC,
        )),
        UnresolvedInstruction::Instruction(Instruction::Add(
            Register::BP,
            Register::SP,
            Register::Zero,
        )),
        UnresolvedInstruction::Jump("main".to_string()),
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
    for (k, t) in global_map.iter() {
        ctx.define_global(k, t.clone());
    }
    // codegen pass
    // TODO: just removed some global calc
    // offset + (global_page_size as u32) + ctx.init.iter().map(|x| x.size()).sum::<u32>();
    for p in program {
        match p {
            ast::TopLevel::FunctionDefinition {
                name, body, args, ..
            } => {
                let block =
                    compile_body(&mut ctx, body, &name.0, args).map_err(|x| (ctx.clone(), x))?;
                let local_count_sym = format!("__internal_{name}_local_count");
                ctx.define(&Symbol::new(&local_count_sym), block.local_offset as u32);
                ctx.functions.push((name.0.clone(), block));
            }
            ast::TopLevel::InlineAsm { name, body, args } => {
                let mut pp = PreProcessor::default();
                pp.create_section("_test", 0, SectionMode::RW);
                pp.set_active_section("_test");
                pp.handle(&body)
                    .map_err(|x| (ctx.clone(), CompilerError::InlineAsm(x.to_string())))?;
                let sections = pp
                    .get_unresolved_instructions()
                    .map_err(|e| (ctx.clone(), CompilerError::InlineAsm(e.to_string())))?;
                let section = sections.get("_test").unwrap();

                let mut block = Block::default();
                for chunk in &section.chunks {
                    if let Chunk::Lines(lines) = chunk {
                        block.instructions.extend(lines.clone());
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
                        Register::SP,
                        Register::BP,
                        Register::Zero,
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
                block.instructions.push(UnresolvedInstruction::Instruction(
                    Instruction::JumpRegister(Register::Zero, Register::C),
                ));

                // TODO: copy this for function defs rather than lookup in compile_body
                for (name, arg_type) in args {
                    block.define_arg(
                        &name.0,
                        &Type::from_ast(&ctx, &arg_type).map_err(|e| (ctx.clone(), e))?,
                    );
                }
                ctx.functions.push((name.0.clone(), block));
            }
            ast::TopLevel::GlobalVariable { .. } => {}
            ast::TopLevel::TypeDefinition { .. } => {}
        }
    }

    let mut offsets: HashMap<String, u32> = HashMap::new();

    let mut program_offset = ctx.get_code_section_start();
    // function offset allocation pass
    for (name, block) in ctx.functions.iter_mut() {
        // TODO: this is a hack
        if name != "_init" {
            while program_offset % 16 != 0 {
                program_offset += 1;
            }
        }
        let block_size: u32 = block.instructions.iter().map(|x| x.size()).sum();
        offsets.insert(name.to_string(), program_offset);
        block.offset = program_offset;
        program_offset += block_size;
    }
    for (_, block) in ctx.functions.clone() {
        block.register_labels(&mut ctx, block.offset);
    }
    for (name, offset) in offsets {
        ctx.define(&Symbol::new(&name), offset);
    }

    Ok(ctx)
}
