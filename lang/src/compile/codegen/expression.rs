use crate::compile::codegen::util::*;

use simplevm::{
    resolve::UnresolvedInstruction, Instruction, Literal12Bit, Literal7Bit, Nibble, Register,
    StackOp, TestOp,
};

use crate::ast;
use crate::compile::block::{BlockScope, BlockVariable};
use crate::compile::context::Context;
use crate::compile::error::CompilerError;
use crate::compile::resolve::{type_of, Type};
// use crate::compile::util::*;

pub fn compile_expression(
    ctx: &mut Context,
    scope: &mut BlockScope,
    expr: &ast::Expression,
) -> Result<Vec<UnresolvedInstruction>, CompilerError> {
    match expr {
        ast::Expression::Bracketed(e) => compile_expression(ctx, scope, e),
        ast::Expression::LiteralInt(i) => {
            if *i <= 0xfff {
                Ok(vec![
                    UnresolvedInstruction::Instruction(Instruction::Imm(
                        Register::C,
                        Literal12Bit::new_checked(*i as u16).unwrap(),
                    )),
                    UnresolvedInstruction::Instruction(Instruction::Stack(
                        Register::C,
                        Register::SP,
                        StackOp::Push,
                    )),
                ])
            } else if *i <= 0xffff && (*i & 0xf) == 0 {
                Ok(vec![
                    UnresolvedInstruction::Instruction(Instruction::Imm(
                        Register::C,
                        Literal12Bit::new_checked((*i >> 4) as u16).unwrap(),
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
            } else if *i <= 0xffff {
                Ok(vec![
                    UnresolvedInstruction::Instruction(Instruction::Imm(
                        Register::C,
                        Literal12Bit::new_checked((*i >> 4) as u16).unwrap(),
                    )),
                    UnresolvedInstruction::Instruction(Instruction::ShiftLeft(
                        Register::C,
                        Register::C,
                        Nibble::new_checked(4).unwrap(),
                    )),
                    UnresolvedInstruction::Instruction(Instruction::AddImm(
                        Register::C,
                        Literal7Bit::new_checked((*i & 0xf) as u8).unwrap(),
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
                Literal12Bit::new_checked(*c as u16).unwrap(),
            )),
            UnresolvedInstruction::Instruction(Instruction::Stack(
                Register::C,
                Register::SP,
                StackOp::Push,
            )),
        ]),
        ast::Expression::LiteralString(s) => {
            let mut const_data = Vec::<u8>::new();
            // TODO: danger utf8 + str len assuming ascii
            let str_len = s.len();
            const_data.push((str_len & 0xff) as u8);
            const_data.push(((str_len & 0xff00) >> 8) as u8);
            let s_bytes = s.clone().into_bytes();
            const_data.extend(s_bytes);
            let addr = ctx.push_static_data(const_data) + 2;
            let mut out = Vec::new();
            if addr > 0xfff {
                todo!("address too big: {addr}");
            }
            out.extend(load_address_to(addr as usize, Register::C, Register::M));
            out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                Register::C,
                Register::SP,
                StackOp::Push,
            )));
            Ok(out)
        }
        ast::Expression::BuiltinSizeof(t) => {
            let tt = Type::from_ast(ctx, t)?;
            let size = tt.size_bytes();
            Ok(vec![
                UnresolvedInstruction::Instruction(Instruction::Imm(
                    Register::C,
                    Literal12Bit::new_checked(size as u16).unwrap(),
                )),
                UnresolvedInstruction::Instruction(Instruction::Stack(
                    Register::C,
                    Register::SP,
                    StackOp::Push,
                )),
            ])
        }
        ast::Expression::Deref(e) => {
            let inner_type = type_of(ctx, scope, e);
            if !inner_type.is_pointer() {
                println!("{:?}", scope);
                return Err(CompilerError::DerefInvalidType(inner_type));
            }
            let mut out = Vec::new();
            out.extend(compile_expression(ctx, scope, e)?);
            out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                Register::C,
                Register::SP,
                StackOp::Pop,
            )));
            let pointed_type = match inner_type {
                Type::Pointer(p) => p,
                _ => panic!("we already asserted this was a pointer"),
            };
            if pointed_type.size_bytes() == 1 {
                out.push(UnresolvedInstruction::Instruction(Instruction::LoadByte(
                    Register::C,
                    Register::C,
                    Register::Zero,
                )));
            } else if pointed_type.size_bytes() == 2 {
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
        ast::Expression::AddressOf(fields) => {
            let mut out = Vec::new();
            if fields.is_empty() {
                panic!("unreachable");
            }
            let head = fields.first().expect("parser issue");
            let head_var = scope
                .get(ctx, &head.0)
                .ok_or(CompilerError::VariableUndefined(head.0.to_string()))?;

            let var_type = match &head_var {
                BlockVariable::Local(_, ty) => ty,
                BlockVariable::Arg(_, ty) => ty,
                BlockVariable::Global(_, ty) => ty,
                BlockVariable::Const(_) => &Type::Int,
            };

            get_stack_field_offset(&mut out, fields, var_type, &head_var, Register::C)?;
            out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                Register::C,
                Register::SP,
                StackOp::Push,
            )));
            Ok(out)
        }
        ast::Expression::FunctionCall(id, args) => {
            let def = ctx
                .function_defs
                .get(&id.0)
                .ok_or(CompilerError::UnknownFunction(id.0.to_string()))?;
            if def.args.len() != args.len() {
                return Err(CompilerError::IncorrectFunctionArgCount {
                    name: id.0.to_string(),
                    expected: def.args.len(),
                    got: args.len(),
                });
            }
            let mut out = Vec::new();
            // TODO: check # args correct
            for a in args.iter().rev() {
                out.append(&mut compile_expression(ctx, scope, a)?);
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
                    Register::BP,
                    Register::SP,
                    Register::Zero,
                )),
                UnresolvedInstruction::Imm(Register::PC, id.0.to_string()),
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
            out.append(&mut compile_expression(ctx, scope, e1)?);
            out.append(&mut compile_expression(ctx, scope, e0)?);
            // stack = [rv0, rv1]
            let e0_type = type_of(ctx, scope, e0);
            match op {
                ast::BinOp::Add => {
                    if let Type::Pointer(t) = e0_type {
                        let size = t.size_bytes();
                        // println!("pointer arith: += *sizeof({t}) (== {size})");
                        out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                            Register::C,
                            Register::SP,
                            StackOp::Swap,
                        )));
                        out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                            Register::C,
                            Register::SP,
                            StackOp::Pop,
                        )));
                        out.push(UnresolvedInstruction::Instruction(Instruction::Imm(
                            Register::B,
                            Literal12Bit::new_checked(size as u16).unwrap(),
                        )));
                        out.push(UnresolvedInstruction::Instruction(Instruction::Mul(
                            Register::C,
                            Register::C,
                            Register::B,
                        )));
                        out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                            Register::C,
                            Register::SP,
                            StackOp::Push,
                        )));
                    }
                    out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                        Register::Zero,
                        Register::SP,
                        StackOp::Add,
                    )));
                }
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
                        Register::C,
                        Register::B,
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
        ast::Expression::Variable(fields) => {
            let mut out = Vec::new();
            let expr_type = type_of(ctx, scope, &ast::Expression::Variable(fields.to_vec()));
            if fields.is_empty() {
                panic!("unreachable");
            }
            let head = fields.first().expect("parser issue");
            let head_var = scope
                .get(ctx, &head.0)
                .ok_or(CompilerError::VariableUndefined(head.0.to_string()))?;

            let var_type = match &head_var {
                BlockVariable::Local(_, ty) => ty,
                BlockVariable::Arg(_, ty) => ty,
                BlockVariable::Global(_, ty) => ty,
                BlockVariable::Const(_) => &Type::Int,
            };

            // get addr of field
            get_stack_field_offset(&mut out, fields, var_type, &head_var, Register::C)?;

            // deref
            if expr_type.size_bytes() == 1 {
                out.push(UnresolvedInstruction::Instruction(Instruction::LoadByte(
                    Register::C,
                    Register::C,
                    Register::Zero,
                )));
            } else if expr_type.size_bytes() == 2 {
                out.push(UnresolvedInstruction::Instruction(Instruction::LoadWord(
                    Register::C,
                    Register::C,
                    Register::Zero,
                )));
            } else {
                return Err(CompilerError::ValueTooLargeForStack(expr_type.clone()));
            }

            // push
            out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                Register::C,
                Register::SP,
                StackOp::Push,
            )));
            Ok(out)
        }
        ast::Expression::ArrayDeref { lhs, index } => {
            let lhs_type = type_of(ctx, scope, lhs);
            if !lhs_type.is_pointer() {
                return Err(CompilerError::DerefInvalidType(lhs_type));
            }
            let index_type = type_of(ctx, scope, index);
            if !index_type.is_numeric() {
                return Err(CompilerError::InvalidIndexType(index_type));
            }
            let new_expr = ast::Expression::Deref(Box::new(ast::Expression::BinOp(
                lhs.clone(),
                index.clone(),
                ast::BinOp::Add,
            )));
            compile_expression(ctx, scope, &new_expr)
        }
    }
}
