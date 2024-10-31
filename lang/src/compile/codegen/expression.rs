use std::collections::HashMap;
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

#[derive(Debug)]
pub enum RegisterStateError {
    AttemptedDowngrade(Register),
    InvalidRegister(Register),
}

#[derive(Debug, Clone)]
pub struct State {
    registers: HashMap<Register, RegisterState>,
}

impl State {
    pub fn new() -> Self {
        Self {
            registers: HashMap::from([
                           (Register::A, RegisterState::default()),
                           (Register::B, RegisterState::default()),
                           // we must have at least 1 temporary register
                           (Register::C, RegisterState::Temporary),
                        ])
        }
    }

    fn invalidate(&mut self, r: Register) {
        self.registers.insert(r, RegisterState::Free);
    }

    fn get_free(&self) -> Option<Register> {
        for (reg, state) in &self.registers {
            if *state == RegisterState::Free {
                return Some(*reg)
            }
        };
        None
    }

    fn get_temp(&self) -> Option<Register> {
        for (reg, state) in &self.registers {
            if *state == RegisterState::Temporary {
                return Some(*reg)
            }
        };
        None
    }

    fn set_literal(&mut self, r: Register, i: i32) -> Result<(), RegisterStateError> {
        if let Some(state) = self.registers.get(&r) {
            match state {
                RegisterState::Free => {
                    self.registers.insert(r, RegisterState::Literal(i));
                    Ok(())
                }
                RegisterState::Literal(_) => {
                    self.registers.insert(r, RegisterState::Literal(i));
                    Ok(())
                }
                RegisterState::Temporary => Ok(()),
                RegisterState::Variable(_) => Err(RegisterStateError::AttemptedDowngrade(r)),
                RegisterState::Intermediate => Err(RegisterStateError::AttemptedDowngrade(r)),
            }
        } else {
            Err(RegisterStateError::InvalidRegister(r))
        }
    }

    fn set_intermediate(&mut self, r: Register) {
        self.registers.insert(r, RegisterState::Intermediate);
    }

    fn get_variable_register(&self, s: &str) -> Option<Register> {
        for (reg, state) in &self.registers {
            if let RegisterState::Variable(var) = state {
                if var == s {
                    return Some(*reg);
                }
            };
        };
        None
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
enum RegisterState {
    #[default]
    Free,
    Literal(i32),
    Variable(String),
    Intermediate,
    Temporary,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionDestination {
    Register(Register),
    Stack,
}

pub struct ExprRes {
    pub destination: ExpressionDestination,
    pub state: State,
    pub instructions: Vec<UnresolvedInstruction>,
}

impl ExprRes {
    fn from_instructions(instructions: Vec<UnresolvedInstruction>, state: State, destination: ExpressionDestination) -> Self {
        Self {
            instructions,
            state,
            destination,
        }
    }

    fn from_instructions_stack(instructions: Vec<UnresolvedInstruction>, state: State) -> Self {
        Self {
            instructions,
            state,
            destination: ExpressionDestination::Stack,
        }
    }
}

fn compile_expression_literal_int(
    state: State,
    i: i32,
    reg: Register,
    target: ExpressionDestination,
) -> Result<ExprRes,CompilerError> {
    let mut out = Vec::new();
    if i <= 0xfff {
        out.push(UnresolvedInstruction::Instruction(Instruction::Imm(
            reg,
            Literal12Bit::new_checked(i as u16).unwrap(),
        )));
    } else if i <= 0xffff && (i & 0xf) == 0 {
        out.extend(vec![
            UnresolvedInstruction::Instruction(Instruction::Imm(
                reg,
                Literal12Bit::new_checked((i >> 4) as u16).unwrap(),
            )),
            UnresolvedInstruction::Instruction(Instruction::ShiftLeft(
                reg,
                reg,
                Nibble::new_checked(4).unwrap(),
            )),
        ]);
    } else if i <= 0xffff {
        out.extend(vec![
            UnresolvedInstruction::Instruction(Instruction::Imm(
                reg,
                Literal12Bit::new_checked((i >> 4) as u16).unwrap(),
            )),
            UnresolvedInstruction::Instruction(Instruction::ShiftLeft(
                reg,
                reg,
                Nibble::new_checked(4).unwrap(),
            )),
            UnresolvedInstruction::Instruction(Instruction::AddImm(
                reg,
                Literal7Bit::new_checked((i & 0xf) as u8).unwrap(),
            )),
        ]);
    } else {
        todo!("number too big: {i}");
    };
    if target == ExpressionDestination::Stack {
        out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
            reg,
            Register::SP,
            StackOp::Push,
        )));
    }
    Ok(ExprRes::from_instructions(out, state, target))
}

pub fn compile_expression(
    ctx: &mut Context,
    scope: &mut BlockScope,
    expr: &ast::Expression,
    state: State,
) -> Result<ExprRes, CompilerError> {
    match expr {
        ast::Expression::Bracketed(e) => compile_expression(ctx, scope, e, state),
        ast::Expression::LiteralInt(i) => {
            let (target, reg) = if let Some(r) = state.get_free() {
                (ExpressionDestination::Register(r), r)
            } else {
                (ExpressionDestination::Stack, state.get_temp().unwrap())
            };
            let mut res = compile_expression_literal_int(state, *i, reg, target)?;
            res.state.set_literal(reg, *i).map_err(|e| CompilerError::RegisterState(e))?;
            Ok(res)
        }
        ast::Expression::LiteralChar(c) => Ok(ExprRes::from_instructions(vec![
            UnresolvedInstruction::Instruction(Instruction::Imm(
                Register::C,
                Literal12Bit::new_checked(*c as u16).unwrap(),
            )),
            UnresolvedInstruction::Instruction(Instruction::Stack(
                Register::C,
                Register::SP,
                StackOp::Push,
            )),
        ], state, ExpressionDestination::Stack)),
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
            Ok(ExprRes::from_instructions_stack(out, state))
        }
        ast::Expression::BuiltinSizeof(t) => {
            let tt = Type::from_ast(ctx, t)?;
            let size = tt.size_bytes();
            Ok(ExprRes::from_instructions_stack(vec![
                UnresolvedInstruction::Instruction(Instruction::Imm(
                    Register::C,
                    Literal12Bit::new_checked(size as u16).unwrap(),
                )),
                UnresolvedInstruction::Instruction(Instruction::Stack(
                    Register::C,
                    Register::SP,
                    StackOp::Push,
                )),
            ], state))
        }
        ast::Expression::Deref(e) => {
            let inner_type = type_of(ctx, scope, e);
            if !inner_type.is_pointer() {
                println!("{:?}", scope);
                return Err(CompilerError::DerefInvalidType(inner_type));
            }
            let mut out = Vec::new();
            let res = compile_expression(ctx, scope, e, state)?;
            out.extend(res.instructions);
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
            Ok(ExprRes::from_instructions_stack(out, res.state))
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
            Ok(ExprRes::from_instructions_stack(out, state))
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
            let mut state = state;
            // TODO: check # args correct
            for a in args.iter().rev() {
                let res = compile_expression(ctx, scope, a, state.clone())?;
                out.extend(res.instructions);
                state = res.state;
            }
            out.append(&mut vec![
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
                UnresolvedInstruction::Jump(id.0.to_string()),
                // functions return in register A, so push this
                UnresolvedInstruction::Instruction(Instruction::Stack(
                    Register::A,
                    Register::SP,
                    StackOp::Push,
                )),
            ]);
            Ok(ExprRes::from_instructions_stack(out, state))
        }
        ast::Expression::BinOp(e0, e1, op) => {
            let res_rhs = compile_expression(ctx, scope, e1, state.clone())?;
            let res_lhs = compile_expression(ctx, scope, e0, res_rhs.state.clone())?;
            let mut out = Vec::new();
            out.extend(res_rhs.instructions.clone());
            out.extend(res_lhs.instructions.clone());
            // stack = [rv0, rv1]
            let lhs_type = type_of(ctx, scope, e0);
            let rhs_type = type_of(ctx, scope, e1);
            match op {
                ast::BinOp::Add => binop_arith(true, out, &res_rhs, &res_lhs, &lhs_type, &rhs_type, state.clone()),
                ast::BinOp::Subtract => binop_arith(false, out, &res_rhs, &res_lhs, &lhs_type, &rhs_type, state.clone()),
                ast::BinOp::Multiply => binop_mul(out, &res_rhs, &res_lhs, &lhs_type, &rhs_type, state.clone()),
                ast::BinOp::LessThanEqual => {
                    // binop_compare(Register::B, Register::C, TestOp::Lte)
                    todo!("needs 2 temp registers");
                }
                ast::BinOp::GreaterThan => {
                    //  binop_compare(Register::B, Register::C, TestOp::Gt)
                    todo!("needs 2 temp registers");
                }
                ast::BinOp::LessThan => {
                    // binop_compare(Register::B, Register::C, TestOp::Lt)
                    todo!("needs 2 temp registers");
                }
                _ => panic!("unimplemented binop {op}"),
            }
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
            Ok(ExprRes::from_instructions_stack(out, state))
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
            compile_expression(ctx, scope, &new_expr, state)
        }
    }
}

fn binop_arith(is_add: bool, mut out: Vec<UnresolvedInstruction>, rhs: &ExprRes, lhs: &ExprRes, lhs_type: &Type, _rhs_type: &Type, mut state: State) -> Result<ExprRes, CompilerError> {
    let ops_on_stack = rhs.destination == ExpressionDestination::Stack && lhs.destination == ExpressionDestination::Stack;
    if ops_on_stack {
        if let Type::Pointer(t) = lhs_type {
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
        if is_add {
            out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                Register::Zero,
                Register::SP,
                StackOp::Add,
            )));
        } else {
            out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                Register::Zero,
                Register::SP,
                StackOp::Sub,
            )));
        }
        Ok(ExprRes::from_instructions_stack(out, state))
    } else {
        let r0 = if let ExpressionDestination::Register(r) = rhs.destination {
            r
        } else {
            let r = state.get_temp().unwrap();
            out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                r, Register::SP, StackOp::Pop,
            )));
            r
        };
        let r1 = if let ExpressionDestination::Register(r) = lhs.destination {
            r
        } else {
            let r = state.get_temp().unwrap();
            out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                r, Register::SP, StackOp::Pop,
            )));
            r
        };
        let rt = state.get_temp().unwrap();
        if let Type::Pointer(t) = lhs_type {
            let size = t.size_bytes();
            out.push(UnresolvedInstruction::Instruction(Instruction::Imm(
                rt,
                Literal12Bit::new_checked(size as u16).unwrap(),
            )));
            out.push(UnresolvedInstruction::Instruction(Instruction::Mul(
                r0, r0, rt,
            )));
            state.invalidate(r0);
        };
        if is_add {
            out.push(UnresolvedInstruction::Instruction(Instruction::Add(
                        r0, r0, r1)));
        } else {
            out.push(UnresolvedInstruction::Instruction(Instruction::Sub(
                        r0, r0, r1)));
        }
        state.set_intermediate(r0);
        Ok(ExprRes::from_instructions(out, state, ExpressionDestination::Register(r0)))
    }
}


fn binop_mul(mut out: Vec<UnresolvedInstruction>, rhs: &ExprRes, lhs: &ExprRes, lhs_type: &Type, rhs_typ: &Type, mut state: State) -> Result<ExprRes, CompilerError> {
    let mut out = Vec::new();
    let ops_on_stack = rhs.destination == ExpressionDestination::Stack && lhs.destination == ExpressionDestination::Stack;
    if ops_on_stack {
        todo!("need 2 temp registers");
    } else {
        let r0 = if let ExpressionDestination::Register(r) = rhs.destination {
            r
        } else {
            let r = state.get_temp().unwrap();
            out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                r, Register::SP, StackOp::Pop,
            )));
            r
        };
        let r1 = if let ExpressionDestination::Register(r) = lhs.destination {
            r
        } else {
            let r = state.get_temp().unwrap();
            out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                r, Register::SP, StackOp::Pop,
            )));
            r
        };
        let rt = state.get_temp().unwrap();
        out.push(UnresolvedInstruction::Instruction(Instruction::Mul(
                    r0, r0, r1)));
        state.set_intermediate(r0);
        Ok(ExprRes::from_instructions(out, state, ExpressionDestination::Register(r0)))
    }
}


