use std::cell::RefCell;
use std::rc::Rc;
use std::str::FromStr;

use simplevm::pp;
use simplevm::pp::PreProcessor;
use simplevm::{
    Instruction, InstructionParseError, Literal12Bit, Literal7Bit, Nibble, Register, StackOp,
    TestOp,
};

use crate::ast;
use crate::compile::block::{Block, BlockScope, BlockVariable, LoopLabels};
use crate::compile::context::{Context, FunctionDefinition};
use crate::compile::error::CompilerError;
use crate::compile::resolve::{type_of, Symbol, Type, UnresolvedInstruction};
use crate::compile::util::*;

fn compile_block(
    ctx: &mut Context,
    mut scope: BlockScope,
    statements: Vec<ast::Statement>,
) -> Result<Vec<UnresolvedInstruction>, CompilerError> {
    let mut out = Vec::new();
    for s in statements {
        match s {
            ast::Statement::Break => {
                if let Some(LoopLabels { ref bottom, .. }) = scope.loop_labels {
                    out.push(UnresolvedInstruction::Imm(Register::PC, bottom.clone()));
                } else {
                    return Err(CompilerError::BreakNotInLoop);
                }
            }
            ast::Statement::Continue => {
                if let Some(LoopLabels { ref top, .. }) = scope.loop_labels {
                    out.push(UnresolvedInstruction::Imm(Register::PC, top.clone()));
                } else {
                    return Err(CompilerError::ContinueNotInLoop);
                }
            }
            ast::Statement::While { cond, body } => {
                let block_identifier = gensym(rand::thread_rng());
                let label_test = Symbol::new(&(block_identifier.to_string() + "_while_lbl_test"));
                let label_out = Symbol::new(&(block_identifier + "_while_lbl_out"));
                out.push(UnresolvedInstruction::Label(label_test.clone()));
                let mut compiled_cond = compile_expression(ctx, &mut scope, cond)?;
                out.append(&mut compiled_cond);
                out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                    Register::C,
                    Register::SP,
                    StackOp::Pop,
                )));
                out.push(UnresolvedInstruction::Instruction(Instruction::Test(
                    Register::C,
                    Register::Zero,
                    TestOp::EitherNonZero,
                )));
                out.push(UnresolvedInstruction::Instruction(Instruction::AddIf(
                    Register::PC,
                    Register::PC,
                    Nibble::new_checked(2).unwrap(),
                )));
                out.push(UnresolvedInstruction::Imm(Register::PC, label_out.clone()));
                let child_scope = scope.child_in_loop(label_test.clone(), label_out.clone());
                out.append(&mut compile_block(ctx, child_scope, body)?);
                out.push(UnresolvedInstruction::Imm(Register::PC, label_test.clone()));
                out.push(UnresolvedInstruction::Label(label_out));
            }
            ast::Statement::If {
                cond,
                body,
                else_body,
            } => {
                let block_identifier = gensym(rand::thread_rng());
                let label_true = Symbol::new(&(block_identifier.to_string() + "_if_lbl_true"));
                let label_out = Symbol::new(&(block_identifier + "_if_lbl_out"));
                let mut compiled_cond = compile_expression(ctx, &mut scope, cond)?;
                out.append(&mut compiled_cond);
                // test if condition is FALSY
                out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                    Register::C,
                    Register::SP,
                    StackOp::Pop,
                )));
                out.push(UnresolvedInstruction::Instruction(Instruction::Test(
                    Register::C,
                    Register::Zero,
                    TestOp::BothZero,
                )));
                out.push(UnresolvedInstruction::Instruction(Instruction::AddIf(
                    Register::PC,
                    Register::PC,
                    Nibble::new_checked(2).unwrap(),
                )));
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
                    return Err(CompilerError::VariableAlreadyDefined(id.0.to_string()));
                }

                // type check
                let expr_type = type_of(ctx, &scope, &expr);
                let var_type = if let Some(tt) = t {
                    let var_type = Type::from_ast(ctx, &tt)?;
                    if var_type.is_struct() {
                        todo!("cannot declare struct value");
                    }
                    if !var_type.can_assign_from(&expr_type) {
                        return Err(CompilerError::TypeAssign {
                            from: expr_type,
                            to: var_type,
                        });
                    }
                    var_type
                } else {
                    expr_type
                };

                let local_index = scope.define_local(&id.0, &var_type);
                // put expression on top of stack
                let mut compiled_expr = compile_expression(ctx, &mut scope, *expr)?;
                out.append(&mut compiled_expr);
                out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                    Register::C,
                    Register::SP,
                    StackOp::Pop,
                )));
                out.push(UnresolvedInstruction::Instruction(Instruction::Add(
                    Register::BP,
                    Register::Zero,
                    Register::B,
                )));
                out.push(UnresolvedInstruction::Instruction(Instruction::AddImm(
                    Register::B,
                    Literal7Bit::new_checked(local_index as u8 * 2).unwrap(),
                )));
                out.push(UnresolvedInstruction::Instruction(Instruction::StoreWord(
                    Register::B,
                    Register::Zero,
                    Register::C,
                )));
            }
            ast::Statement::Declare(id, t, None) => {
                if scope.get(ctx, &id.0).is_some() {
                    return Err(CompilerError::VariableAlreadyDefined(id.0.to_string()));
                }
                if let Some(tt) = t {
                    let declared_type = Type::from_ast(ctx, &tt)?;
                    if declared_type.is_struct() {
                        todo!("struct value unsupported");
                    }
                    scope.define_local(&id.0, &declared_type);
                } else {
                    return Err(CompilerError::InvalidUntypedVariableDeclration(
                        id.0.to_string(),
                    ));
                }
            }
            ast::Statement::Assign(id, expr) => {
                if let Some(bv) = scope.get(ctx, &id.0) {
                    match bv {
                        BlockVariable::Local(index, _) => {
                            let mut compiled_expr = compile_expression(ctx, &mut scope, *expr)?;
                            out.append(&mut compiled_expr);
                            assign_from_stack_to_local(&mut out, index as u8);
                        }
                        BlockVariable::Arg(index, tt) => {
                            let expr_type = type_of(ctx, &scope, expr.as_ref());
                            if !tt.can_assign_from(&expr_type) {
                                return Err(CompilerError::TypeAssign {
                                    from: expr_type,
                                    to: tt,
                                });
                            }
                            let mut compiled_expr = compile_expression(ctx, &mut scope, *expr)?;
                            out.append(&mut compiled_expr);
                            assign_from_stack_to_arg(&mut out, index as u8);
                        }
                        BlockVariable::Global(addr, tt) => {
                            // type check
                            let expr_type = type_of(ctx, &scope, &expr);
                            if !tt.can_assign_from(&expr_type) {
                                return Err(CompilerError::TypeAssign {
                                    from: expr_type,
                                    to: tt,
                                });
                            }

                            let mut compiled_expr = compile_expression(ctx, &mut scope, *expr)?;
                            out.append(&mut compiled_expr);
                            out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                                Register::C,
                                Register::SP,
                                StackOp::Pop,
                            )));
                            out.extend(load_address_to(addr, Register::B, Register::M));
                            out.push(UnresolvedInstruction::Instruction(Instruction::StoreWord(
                                Register::B,
                                Register::Zero,
                                Register::C,
                            )));
                        }
                        _ => todo!("unimplemented {bv:?}"),
                    }
                } else {
                    return Err(CompilerError::VariableUndefined(id.0.to_string()));
                }
            }
            ast::Statement::AssignDeref { lhs, rhs } => {
                let compiled_addr = compile_expression(ctx, &mut scope, lhs)?;
                let compiled_value = compile_expression(ctx, &mut scope, rhs)?;
                out.extend(compiled_addr);
                out.extend(compiled_value);
                out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                    Register::B,
                    Register::SP,
                    StackOp::Pop,
                )));
                out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                    Register::C,
                    Register::SP,
                    StackOp::Pop,
                )));
                out.push(UnresolvedInstruction::Instruction(Instruction::StoreWord(
                    Register::C,
                    Register::Zero,
                    Register::B,
                )));
            }
            ast::Statement::AssignStructField { fields, rhs } => {
                let head = fields.first().expect("this is a parser issue");
                let field = fields.get(1).expect("must have 2");
                let rhs_type = type_of(ctx, &scope, &rhs);
                let mut compiled_expr = compile_expression(ctx, &mut scope, rhs)?;
                out.append(&mut compiled_expr);
                let bv = scope
                    .get(ctx, &head.0)
                    .ok_or(CompilerError::VariableUndefined(head.0.to_string()))?;
                let var_type = match &bv {
                    BlockVariable::Local(_, ty) => ty,
                    BlockVariable::Arg(_, ty) => ty,
                    BlockVariable::Global(_, ty) => ty,
                    BlockVariable::Const(_) => &Type::Int,
                };
                let struct_fields = match var_type.clone() {
                    Type::Struct(sf) => sf,
                    Type::Pointer(t) => {
                        if let Type::Struct(sf) = *t {
                            sf
                        } else {
                            return Err(CompilerError::NonStructFieldReference(
                                head.to_string(),
                                var_type.clone(),
                            ));
                        }
                    }
                    _ => {
                        panic!("{head} {var_type} {bv:?}");
                        // return Err(CompilerError::NonStructFieldReference(head.to_string(), var_type.clone())),
                    }
                };
                let (field_type, field_offset) = struct_fields
                    .get(&field.0)
                    .ok_or(CompilerError::VariableUndefined(field.0.to_string()))?;
                if !field_type.can_assign_from(&rhs_type) {
                    return Err(CompilerError::TypeAssign {
                        from: rhs_type,
                        to: field_type.clone(),
                    });
                }

                // 0. load address of thing based on var type -> C
                match &bv {
                    BlockVariable::Local(index, _) => {
                        load_local_addr_to(&mut out, *index as u8, Register::C)
                    }
                    BlockVariable::Arg(index, _) => {
                        load_arg_addr_to(&mut out, *index as u8, Register::C)
                    }
                    _ => todo!("{bv:?} not supported in struct field assignment"),
                };

                // 1. add field offset
                if var_type.is_pointer() {
                    out.push(UnresolvedInstruction::Instruction(Instruction::LoadWord(
                        Register::C,
                        Register::C,
                        Register::Zero,
                    )));
                }
                out.push(UnresolvedInstruction::Instruction(Instruction::AddImm(
                    Register::C,
                    Literal7Bit::new_checked(*field_offset as u8).unwrap(),
                )));
                // 2. pop value to write from stack
                out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                    Register::B,
                    Register::SP,
                    StackOp::Pop,
                )));
                // 3. write value
                write_value(&mut out, rhs_type, Register::B, Register::C);
            }
            ast::Statement::Return(expr) => {
                let mut compiled_expr = compile_expression(ctx, &mut scope, expr)?;
                out.append(&mut compiled_expr);
                // return in the A register
                out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                    Register::A,
                    Register::SP,
                    StackOp::Pop,
                )));
            }
            ast::Statement::Expression(expr) => {
                let mut compiled_expr = compile_expression(ctx, &mut scope, expr)?;
                out.append(&mut compiled_expr);
                // forget what we just did
                out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                    Register::Zero,
                    Register::SP,
                    StackOp::Pop,
                )));
            }
        }
    }
    Ok(out)
}

fn compile_body(
    ctx: &mut Context,
    statements: Vec<ast::Statement>,
    name: &str,
    offset: u32,
    args: Vec<(ast::Identifier, ast::Type)>,
) -> Result<Block, CompilerError> {
    let mut block = Block {
        offset,
        ..Block::default()
    };
    block
        .instructions
        .push(UnresolvedInstruction::Label(Symbol::new(name)));
    for (name, arg_type) in &args {
        block.define_arg(&name.0, &Type::from_ast(ctx, &arg_type)?);
    }
    // function setup
    let local_count_sym = format!("__internal_{name}_local_count");
    block.instructions.push(UnresolvedInstruction::AddImm(
        Register::SP,
        Symbol::new(&local_count_sym),
    ));
    let cell = Rc::new(RefCell::new(block));
    let mut compiled = compile_block(ctx, BlockScope::new(cell.clone()), statements)?;
    {
        let mut block = cell.take();
        block.instructions.append(&mut compiled);
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
            Instruction::AddImmSigned(Register::SP, Literal7Bit::from_signed(offset).unwrap()),
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
        Ok(block)
    }
}

fn compile_expression(
    ctx: &Context,
    scope: &mut BlockScope,
    expr: ast::Expression,
) -> Result<Vec<UnresolvedInstruction>, CompilerError> {
    match expr {
        ast::Expression::Bracketed(e) => compile_expression(ctx, scope, *e),
        ast::Expression::LiteralInt(i) => {
            if i <= 0xfff {
                Ok(vec![
                    UnresolvedInstruction::Instruction(Instruction::Imm(
                        Register::C,
                        Literal12Bit::new_checked(i as u16).unwrap(),
                    )),
                    UnresolvedInstruction::Instruction(Instruction::Stack(
                        Register::C,
                        Register::SP,
                        StackOp::Push,
                    )),
                ])
            } else if i <= 0xffff && (i & 0xf) == 0 {
                Ok(vec![
                    UnresolvedInstruction::Instruction(Instruction::Imm(
                        Register::C,
                        Literal12Bit::new_checked((i >> 4) as u16).unwrap(),
                    )),
                    UnresolvedInstruction::Instruction(Instruction::ShiftLeft(
                        Register::C,
                        Register::C,
                        Nibble::new_checked(4).unwrap(),
                    )),
                    UnresolvedInstruction::Instruction(Instruction::Stack(
                        Register::C,
                        Register::SP,
                        StackOp::Push,
                    )),
                ])
            } else if i <= 0xffff {
                Ok(vec![
                    UnresolvedInstruction::Instruction(Instruction::Imm(
                        Register::C,
                        Literal12Bit::new_checked((i >> 4) as u16).unwrap(),
                    )),
                    UnresolvedInstruction::Instruction(Instruction::ShiftLeft(
                        Register::C,
                        Register::C,
                        Nibble::new_checked(4).unwrap(),
                    )),
                    UnresolvedInstruction::Instruction(Instruction::AddImm(
                        Register::C,
                        Literal7Bit::new_checked((i & 0xf) as u8).unwrap(),
                    )),
                    UnresolvedInstruction::Instruction(Instruction::Stack(
                        Register::C,
                        Register::SP,
                        StackOp::Push,
                    )),
                ])
            } else {
                todo!("number too big: {i}");
            }
        }
        ast::Expression::LiteralChar(c) => Ok(vec![
            UnresolvedInstruction::Instruction(Instruction::Imm(
                Register::C,
                Literal12Bit::new_checked(c as u16).unwrap(),
            )),
            UnresolvedInstruction::Instruction(Instruction::Stack(
                Register::C,
                Register::SP,
                StackOp::Push,
            )),
        ]),
        ast::Expression::Deref(e) => {
            let inner_type = type_of(ctx, scope, &e);
            if !inner_type.is_pointer() {
                println!("{:?}", scope);
                return Err(CompilerError::DerefInvalidType(inner_type));
            }
            let mut out = Vec::new();
            out.extend(compile_expression(ctx, scope, *e)?);
            out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                Register::C,
                Register::SP,
                StackOp::Pop,
            )));
            if inner_type.size_bytes() == 1 {
                out.push(UnresolvedInstruction::Instruction(Instruction::LoadByte(
                    Register::C,
                    Register::C,
                    Register::Zero,
                )));
            } else if inner_type.size_bytes() == 2 {
                out.push(UnresolvedInstruction::Instruction(Instruction::LoadWord(
                    Register::C,
                    Register::C,
                    Register::Zero,
                )));
            } else {
                todo!("i don't know how to handle this case");
            }
            out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                Register::C,
                Register::SP,
                StackOp::Push,
            )));
            Ok(out)
        }
        ast::Expression::AddressOf(s) => {
            if let Some(v) = scope.get(ctx, &s.0) {
                match v {
                    BlockVariable::Local(i, _) => {
                        let mut out = Vec::new();
                        load_local_addr_to(&mut out, i as u8, Register::C);
                        out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                            Register::C,
                            Register::SP,
                            StackOp::Push,
                        )));
                        Ok(out)
                    }
                    BlockVariable::Arg(i, _) => {
                        let mut out = Vec::new();
                        load_arg_addr_to(&mut out, i as u8, Register::C);
                        out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                            Register::C,
                            Register::SP,
                            StackOp::Push,
                        )));
                        Ok(out)
                    }
                    BlockVariable::Global(addr, _) => {
                        let mut out = Vec::new();
                        out.extend(load_address_to(addr, Register::C, Register::Zero));
                        out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                            Register::C,
                            Register::SP,
                            StackOp::Push,
                        )));
                        Ok(out)
                    }
                    _ => todo!("address of var no implemented: {v:?}"),
                }
            } else {
                Err(CompilerError::VariableUndefined(s.to_string()))
            }
        }
        ast::Expression::Variable(s) => {
            if let Some(v) = scope.get(ctx, &s) {
                match v {
                    BlockVariable::Local(i, _) => Ok(vec![
                        UnresolvedInstruction::Instruction(Instruction::Add(
                            Register::BP,
                            Register::Zero,
                            Register::C,
                        )),
                        UnresolvedInstruction::Instruction(Instruction::AddImm(
                            Register::C,
                            Literal7Bit::new_checked(i as u8 * 2).unwrap(),
                        )),
                        UnresolvedInstruction::Instruction(Instruction::LoadWord(
                            Register::C,
                            Register::C,
                            Register::Zero,
                        )),
                        UnresolvedInstruction::Instruction(Instruction::Stack(
                            Register::C,
                            Register::SP,
                            StackOp::Push,
                        )),
                    ]),
                    BlockVariable::Arg(i, _) => Ok(vec![
                        UnresolvedInstruction::Instruction(Instruction::LoadStackOffset(
                            Register::C,
                            Register::BP,
                            Nibble::new_checked(i as u8 + 3).unwrap(),
                        )),
                        UnresolvedInstruction::Instruction(Instruction::Stack(
                            Register::C,
                            Register::SP,
                            StackOp::Push,
                        )),
                    ]),
                    BlockVariable::Global(addr, _) => {
                        let mut out = Vec::new();
                        out.extend(load_address_to(addr, Register::B, Register::M));
                        out.push(UnresolvedInstruction::Instruction(Instruction::LoadWord(
                            Register::C,
                            Register::B,
                            Register::Zero,
                        )));
                        out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                            Register::C,
                            Register::SP,
                            StackOp::Push,
                        )));
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
                UnresolvedInstruction::Imm(Register::PC, Symbol::new(&id.0)),
                // functions return in register A, so push this
                UnresolvedInstruction::Instruction(Instruction::Stack(
                    Register::A,
                    Register::SP,
                    StackOp::Push,
                )),
            ]);
            Ok(out)
        }
        ast::Expression::BinOp(e0, e1, op) => {
            let mut out = Vec::new();
            out.append(&mut compile_expression(ctx, scope, *e1)?);
            // expression 1 is on top of stack
            out.append(&mut compile_expression(ctx, scope, *e0)?);
            // stack = [rv0, rv1]
            match op {
                ast::BinOp::Add => out.push(UnresolvedInstruction::Instruction(
                    Instruction::Stack(Register::Zero, Register::SP, StackOp::Add),
                )),
                ast::BinOp::Subtract => out.push(UnresolvedInstruction::Instruction(
                    Instruction::Stack(Register::Zero, Register::SP, StackOp::Sub),
                )),
                ast::BinOp::Multiply => out.extend([
                    UnresolvedInstruction::Instruction(Instruction::Stack(
                        Register::B,
                        Register::SP,
                        StackOp::Pop,
                    )),
                    UnresolvedInstruction::Instruction(Instruction::Stack(
                        Register::C,
                        Register::SP,
                        StackOp::Pop,
                    )),
                    UnresolvedInstruction::Instruction(Instruction::Mul(
                        Register::C,
                        Register::B,
                        Register::C,
                    )),
                    UnresolvedInstruction::Instruction(Instruction::Stack(
                        Register::C,
                        Register::SP,
                        StackOp::Push,
                    )),
                ]),
                ast::BinOp::LessThanEqual => {
                    binop_compare(&mut out, Register::B, Register::C, TestOp::Lte)
                }
                ast::BinOp::GreaterThan => {
                    binop_compare(&mut out, Register::B, Register::C, TestOp::Gt)
                }
                ast::BinOp::LessThan => {
                    binop_compare(&mut out, Register::B, Register::C, TestOp::Lt)
                }

                _ => panic!("unimplemented binop {op}"),
            }
            Ok(out)
        }
    }
}

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
                ctx.define(&Symbol::new(&local_count_sym), block.local_count as u32 * 2);
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

fn load_address_to(
    addr: usize,
    target_register: Register,
    _page_register: Register,
) -> Vec<UnresolvedInstruction> {
    let mut out = Vec::new();
    if addr <= 0xfff {
        out.push(UnresolvedInstruction::Instruction(Instruction::Imm(
            target_register,
            Literal12Bit::new_checked(addr as u16).unwrap(),
        )));
    } else if addr <= 0xffff {
        out.push(UnresolvedInstruction::Instruction(Instruction::Imm(
            target_register,
            Literal12Bit::new_checked((addr & 0xfff0) as u16 >> 4).unwrap(),
        )));
        out.push(UnresolvedInstruction::Instruction(Instruction::ShiftLeft(
            target_register,
            target_register,
            Nibble::new_checked(4).unwrap(),
        )));
        out.push(UnresolvedInstruction::Instruction(Instruction::AddImm(
            target_register,
            Literal7Bit::new_checked((addr & 0xf) as u8).unwrap(),
        )));
    } else {
        todo!("paged addressing not implemented");
    }
    out
}

fn binop_compare(out: &mut Vec<UnresolvedInstruction>, a: Register, b: Register, op: TestOp) {
    out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
        a,
        Register::SP,
        StackOp::Pop,
    )));
    out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
        b,
        Register::SP,
        StackOp::Pop,
    )));
    out.push(UnresolvedInstruction::Instruction(Instruction::Test(
        a, b, op,
    )));
    out.push(UnresolvedInstruction::Instruction(Instruction::Add(
        Register::Zero,
        Register::Zero,
        Register::C,
    )));
    out.push(UnresolvedInstruction::Instruction(Instruction::AddIf(
        Register::C,
        Register::Zero,
        Nibble::new_checked(1).unwrap(),
    )));
    out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
        Register::C,
        Register::SP,
        StackOp::Push,
    )));
}

fn assign_from_stack_to_local(out: &mut Vec<UnresolvedInstruction>, index: u8) {
    out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
        Register::C,
        Register::SP,
        StackOp::Pop,
    )));
    out.push(UnresolvedInstruction::Instruction(Instruction::Add(
        Register::BP,
        Register::Zero,
        Register::B,
    )));
    out.push(UnresolvedInstruction::Instruction(Instruction::AddImm(
        Register::B,
        Literal7Bit::new_checked(index as u8 * 2).unwrap(),
    )));
    out.push(UnresolvedInstruction::Instruction(Instruction::StoreWord(
        Register::B,
        Register::Zero,
        Register::C,
    )));
}

fn load_local_addr_to(out: &mut Vec<UnresolvedInstruction>, index: u8, reg: Register) {
    out.push(UnresolvedInstruction::Instruction(Instruction::Add(
        Register::BP,
        Register::Zero,
        reg,
    )));
    out.push(UnresolvedInstruction::Instruction(Instruction::AddImm(
        reg,
        Literal7Bit::new_checked(index as u8 * 2).unwrap(),
    )));
}

fn assign_from_stack_to_arg(out: &mut Vec<UnresolvedInstruction>, index: u8) {
    out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
        Register::C,
        Register::SP,
        StackOp::Pop,
    )));
    out.push(UnresolvedInstruction::Instruction(Instruction::Add(
        Register::BP,
        Register::Zero,
        Register::B,
    )));
    out.push(UnresolvedInstruction::Instruction(
        Instruction::AddImmSigned(
            Register::B,
            Literal7Bit::from_signed(-2 * (index as i8 + 3)).unwrap(),
        ),
    ));
    out.push(UnresolvedInstruction::Instruction(Instruction::StoreWord(
        Register::B,
        Register::Zero,
        Register::C,
    )));
}

fn load_arg_addr_to(out: &mut Vec<UnresolvedInstruction>, index: u8, reg: Register) {
    out.push(UnresolvedInstruction::Instruction(Instruction::Add(
        Register::BP,
        Register::Zero,
        reg,
    )));
    out.push(UnresolvedInstruction::Instruction(
        Instruction::AddImmSigned(
            reg,
            Literal7Bit::from_signed(-2 * (index as i8 + 3)).unwrap(),
        ),
    ));
}

fn write_value(
    out: &mut Vec<UnresolvedInstruction>,
    ty: Type,
    reg_value: Register,
    reg_addr: Register,
) {
    match ty.size_bytes() {
        0 => (),
        1 => out.push(UnresolvedInstruction::Instruction(Instruction::StoreByte(
            reg_addr,
            Register::Zero,
            reg_value,
        ))),
        2 => out.push(UnresolvedInstruction::Instruction(Instruction::StoreWord(
            reg_addr,
            Register::Zero,
            reg_value,
        ))),
        n => panic!("uh oh we can't assign {n} bytes by value"),
    }
}
