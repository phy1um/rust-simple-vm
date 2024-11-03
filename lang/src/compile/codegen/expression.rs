use crate::compile::codegen::util::*;
use log::{debug, trace};
use std::collections::BTreeMap;
use std::fmt;

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
    registers: BTreeMap<Register, RegisterState>,
}

impl State {
    pub(crate) fn new() -> Self {
        Self {
            registers: BTreeMap::from([
                (Register::A, RegisterState::default()),
                (Register::B, RegisterState::default()),
                // we must have at least 1 temporary register
                (Register::C, RegisterState::Temporary),
                (Register::D, RegisterState::Temporary),
            ]),
        }
    }

    pub(crate) fn invalidate_all(&mut self) {
        for reg in self.registers.clone().keys() {
            self.invalidate(*reg)
        }
    }

    pub(crate) fn invalidate(&mut self, r: Register) {
        match self.registers.get(&r) {
            Some(RegisterState::Temporary) => {}
            _ => {
                self.registers.insert(r, RegisterState::Free);
            }
        }
    }

    pub(crate) fn get_free(&self) -> Option<Register> {
        for (reg, state) in &self.registers {
            if *state == RegisterState::Free {
                return Some(*reg);
            }
        }
        None
    }

    pub(crate) fn get_temp(&self) -> Option<Register> {
        for (reg, state) in &self.registers {
            if *state == RegisterState::Temporary {
                return Some(*reg);
            }
        }
        None
    }

    pub(crate) fn get_temp_pair(&self) -> Option<(Register, Register)> {
        let first = self.get_temp()?;
        for (reg, state) in &self.registers {
            if *state == RegisterState::Temporary && *reg != first {
                return Some((first, *reg));
            }
        }
        trace!("no temp vars: {self}");
        None
    }

    pub(crate) fn set_literal(&mut self, r: Register, i: i32) -> Result<(), RegisterStateError> {
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

    pub(crate) fn set_intermediate(&mut self, r: Register) {
        // TODO: check for temp?
        if Some(RegisterState::Temporary) == self.registers.get(&r).cloned() {
            panic!("cannot set temporary to intermediate!");
        }
        self.registers.insert(r, RegisterState::Intermediate);
    }

    pub(crate) fn clear_intermediates(&mut self) {
        for (_reg, state) in self.registers.iter_mut() {
            if *state == RegisterState::Intermediate {
                *state = RegisterState::Free;
            }
        }
    }

    pub(crate) fn set_variable_register(
        &mut self,
        name: &str,
        r: Register,
    ) -> Result<(), RegisterStateError> {
        trace!("try set variable {name} in register {r}: {self}");
        match self.registers.get(&r) {
            Some(RegisterState::Free) => {
                self.registers
                    .insert(r, RegisterState::Variable(name.to_string()));
                Ok(())
            }
            Some(RegisterState::Literal(_)) => {
                self.registers
                    .insert(r, RegisterState::Variable(name.to_string()));
                Ok(())
            }
            Some(RegisterState::Temporary) => {
                trace!("register {r} is temp, no set {name}");
                Ok(())
            }
            Some(RegisterState::Variable(other)) => {
                trace!("set register state {r}: variable {other} => {name}");
                if name != other {
                    self.registers
                        .insert(r, RegisterState::Variable(name.to_string()));
                };
                Ok(())
            }
            Some(RegisterState::Intermediate) => Err(RegisterStateError::AttemptedDowngrade(r)),
            _ => Err(RegisterStateError::InvalidRegister(r)),
        }
    }

    pub(crate) fn get_variable_register(&self, s: &str) -> Option<Register> {
        for (reg, state) in &self.registers {
            if let RegisterState::Variable(var) = state {
                if var == s {
                    return Some(*reg);
                }
            };
        }
        None
    }

    pub(crate) fn reserve_temporaries(&mut self, n: u32) {
        ()
    }

    fn set_temp(&mut self, r: Register, state: bool) {
        self.registers.insert(
            r,
            if state {
                RegisterState::Temporary
            } else {
                RegisterState::Free
            },
        );
    }
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.registers
                .iter()
                .map(|(reg, kind)| format!("{reg}: {kind}"))
                .collect::<Vec<_>>()
                .join(" || ")
        )
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
    // TODO: FrameOffset(u8) ?
}

impl fmt::Display for RegisterState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Free => write!(f, "free"),
            Self::Literal(i) => write!(f, "lit({i})"),
            Self::Variable(s) => write!(f, "var({s})"),
            Self::Intermediate => write!(f, "intermediate"),
            Self::Temporary => write!(f, "temp"),
        }
    }
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
    pub(crate) fn from_instructions(
        instructions: Vec<UnresolvedInstruction>,
        state: State,
        destination: ExpressionDestination,
    ) -> Self {
        Self {
            instructions,
            state,
            destination,
        }
    }

    pub(crate) fn from_instructions_stack(
        instructions: Vec<UnresolvedInstruction>,
        state: State,
    ) -> Self {
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
) -> Result<ExprRes, CompilerError> {
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
    mut state: State,
) -> Result<ExprRes, CompilerError> {
    trace!("compile expression: {expr}");
    trace!("expr state: {state}");
    match expr {
        ast::Expression::Bracketed(e) => compile_expression(ctx, scope, e, state),
        ast::Expression::LiteralInt(i) => {
            let (target, reg) = if let Some(r) = state.get_free() {
                (ExpressionDestination::Register(r), r)
            } else {
                (ExpressionDestination::Stack, state.get_temp().unwrap())
            };
            let mut res = compile_expression_literal_int(state, *i, reg, target)?;
            res.state
                .set_literal(reg, *i)
                .map_err(CompilerError::RegisterState)?;
            Ok(res)
        }
        ast::Expression::LiteralChar(c) => Ok(ExprRes::from_instructions(
            vec![
                UnresolvedInstruction::Instruction(Instruction::Imm(
                    Register::C,
                    Literal12Bit::new_checked(*c as u16).unwrap(),
                )),
                UnresolvedInstruction::Instruction(Instruction::Stack(
                    Register::C,
                    Register::SP,
                    StackOp::Push,
                )),
            ],
            state,
            ExpressionDestination::Stack,
        )),
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
            let tmp = state.get_temp().unwrap();
            out.extend(load_address_to(addr as usize, tmp, Register::M));
            out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                tmp,
                Register::SP,
                StackOp::Push,
            )));
            Ok(ExprRes::from_instructions_stack(out, state))
        }
        ast::Expression::BuiltinSizeof(t) => {
            let tt = Type::from_ast(ctx, t)?;
            let size = tt.size_bytes();
            Ok(ExprRes::from_instructions_stack(
                vec![
                    UnresolvedInstruction::Instruction(Instruction::Imm(
                        Register::C,
                        Literal12Bit::new_checked(size as u16).unwrap(),
                    )),
                    UnresolvedInstruction::Instruction(Instruction::Stack(
                        Register::C,
                        Register::SP,
                        StackOp::Push,
                    )),
                ],
                state,
            ))
        }
        ast::Expression::Deref(e) => {
            let inner_type = type_of(ctx, scope, e);
            if !inner_type.is_pointer() {
                trace!("deref pointer: {:?}", scope);
                return Err(CompilerError::DerefInvalidType(inner_type));
            }
            let mut out = Vec::new();
            let res = compile_expression(ctx, scope, e, state.clone())?;
            out.extend(res.instructions);
            let temp_reg = if let ExpressionDestination::Register(r) = res.destination {
                r
            } else {
                let t0 = res.state.get_free().unwrap();
                out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                    t0,
                    Register::SP,
                    StackOp::Pop,
                )));
                t0
            };
            let pointed_type = match inner_type {
                Type::Pointer(p) => p,
                _ => panic!("we already asserted this was a pointer"),
            };
            if pointed_type.size_bytes() == 1 {
                out.push(UnresolvedInstruction::Instruction(Instruction::LoadByte(
                    temp_reg,
                    temp_reg,
                    Register::Zero,
                )));
            } else if pointed_type.size_bytes() == 2 {
                out.push(UnresolvedInstruction::Instruction(Instruction::LoadWord(
                    temp_reg,
                    temp_reg,
                    Register::Zero,
                )));
            } else {
                todo!("i don't know how to handle this case");
            }
            out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                temp_reg,
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
            out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                Register::A,
                Register::SP,
                StackOp::Push,
            )));

            // TODO: check # args correct
            for a in args.iter().rev() {
                let res = compile_expression(ctx, scope, a, state.clone())?;
                out.extend(res.instructions);
                if let ExpressionDestination::Register(r) = res.destination {
                    out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                        r,
                        Register::SP,
                        StackOp::Push,
                    )));
                }
                state = res.state;
            }
            let a_state = state.registers.get(&Register::A).unwrap().clone();
            state.invalidate_all();
            out.extend(vec![
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
                UnresolvedInstruction::Instruction(Instruction::Stack(
                    Register::A,
                    Register::SP,
                    StackOp::Push,
                )),
                UnresolvedInstruction::Instruction(Instruction::Stack(
                    Register::Zero,
                    Register::SP,
                    StackOp::Swap,
                )),
                UnresolvedInstruction::Instruction(Instruction::Stack(
                    Register::A,
                    Register::SP,
                    StackOp::Pop,
                )),
            ]);
            state.registers.insert(Register::A, a_state);
            Ok(ExprRes::from_instructions_stack(out, state))
        }
        ast::Expression::BinOp(e0, e1, op) => {
            let res_lhs = compile_expression(ctx, scope, e0, state.clone())?;
            let res_rhs = compile_expression(ctx, scope, e1, res_lhs.state.clone())?;
            trace!(
                "binop: lhs {e0} => {:?} | rhs {e1} => {:?}",
                res_lhs.destination,
                res_rhs.destination
            );
            let mut out = Vec::new();
            out.extend(res_lhs.instructions.clone());
            out.extend(res_rhs.instructions.clone());
            // stack = [rv0, rv1]
            let lhs_type = type_of(ctx, scope, e0);
            let rhs_type = type_of(ctx, scope, e1);
            let state = res_rhs.state.clone();
            match op {
                ast::BinOp::Add => {
                    binop_arith(true, out, &res_rhs, &res_lhs, &lhs_type, &rhs_type, state)
                }
                ast::BinOp::Subtract => {
                    binop_arith(false, out, &res_rhs, &res_lhs, &lhs_type, &rhs_type, state)
                }
                ast::BinOp::Multiply => {
                    binop_mul(out, &res_rhs, &res_lhs, &lhs_type, &rhs_type, state)
                }
                ast::BinOp::LessThanEqual => {
                    binop_compare(out, &res_rhs, &res_lhs, TestOp::Lte, state)
                }
                ast::BinOp::GreaterThan => {
                    binop_compare(out, &res_rhs, &res_lhs, TestOp::Gt, state)
                }
                ast::BinOp::LessThan => binop_compare(out, &res_rhs, &res_lhs, TestOp::Lt, state),
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

            // TODO: constants :D

            let name = fields
                .iter()
                .map(|x| x.0.clone())
                .collect::<Vec<_>>()
                .join(".");
            if let Some(reg) = state.get_variable_register(&name) {
                debug!("reusing var {name} in {reg}");
                return Ok(ExprRes::from_instructions(
                    out,
                    state,
                    ExpressionDestination::Register(reg),
                ));
            };

            // get addr of field
            let (reg, is_temp) = if let Some(r) = state.get_free() {
                (r, false)
            } else {
                (state.get_temp().unwrap(), true)
            };
            get_stack_field_offset(&mut out, fields, var_type, &head_var, reg)?;

            // deref
            if expr_type.size_bytes() == 1 {
                out.push(UnresolvedInstruction::Instruction(Instruction::LoadByte(
                    reg,
                    reg,
                    Register::Zero,
                )));
            } else if expr_type.size_bytes() == 2 {
                out.push(UnresolvedInstruction::Instruction(Instruction::LoadWord(
                    reg,
                    reg,
                    Register::Zero,
                )));
            } else {
                return Err(CompilerError::ValueTooLargeForStack(expr_type.clone()));
            }

            if is_temp {
                debug!("var load {name}: stack (push {reg})");
                // push
                out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                    reg,
                    Register::SP,
                    StackOp::Push,
                )));
                Ok(ExprRes::from_instructions_stack(out, state))
            } else {
                debug!("var load {name}: in {reg}");
                state
                    .set_variable_register(&name, reg)
                    .map_err(CompilerError::RegisterState)?;
                Ok(ExprRes::from_instructions(
                    out,
                    state,
                    ExpressionDestination::Register(reg),
                ))
            }
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

fn binop_arith(
    is_add: bool,
    mut out: Vec<UnresolvedInstruction>,
    rhs: &ExprRes,
    lhs: &ExprRes,
    lhs_type: &Type,
    _rhs_type: &Type,
    mut state: State,
) -> Result<ExprRes, CompilerError> {
    let ops_on_stack = rhs.destination == ExpressionDestination::Stack
        && lhs.destination == ExpressionDestination::Stack;
    let ops_in_reg = matches!(rhs.destination, ExpressionDestination::Register(_))
        && matches!(lhs.destination, ExpressionDestination::Register(_));
    if ops_on_stack {
        if let Type::Pointer(t) = lhs_type {
            let size = t.size_bytes();
            state.reserve_temporaries(2);
            let (t0, t1) = state.get_temp_pair().unwrap();
            out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                Register::Zero,
                Register::SP,
                StackOp::Swap,
            )));
            out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                t0,
                Register::SP,
                StackOp::Pop,
            )));
            out.push(UnresolvedInstruction::Instruction(Instruction::Imm(
                t1,
                Literal12Bit::new_checked(size as u16).unwrap(),
            )));
            out.push(UnresolvedInstruction::Instruction(Instruction::Mul(
                t0, t0, t1,
            )));
            out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                t0,
                Register::SP,
                StackOp::Push,
            )));
            state.reserve_temporaries(1);
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
        let (r0, r1, rt, reg_out) = if ops_in_reg {
            let r0 = if let ExpressionDestination::Register(r) = lhs.destination {
                r
            } else {
                panic!("unreachable");
            };
            let r1 = if let ExpressionDestination::Register(r) = rhs.destination {
                r
            } else {
                panic!("unreachable");
            };
            (r0, r1, state.get_temp().unwrap(), r0)
        } else {
            state.reserve_temporaries(2);
            let (t0, rt) = state.get_temp_pair().unwrap();
            let mut reg_out_opt: Option<Register> = None;
            let r1 = if let ExpressionDestination::Register(r) = rhs.destination {
                reg_out_opt = Some(r);
                r
            } else {
                out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                    t0,
                    Register::SP,
                    StackOp::Pop,
                )));
                t0
            };
            let r0 = if let ExpressionDestination::Register(r) = lhs.destination {
                reg_out_opt = Some(r);
                r
            } else {
                out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                    t0,
                    Register::SP,
                    StackOp::Pop,
                )));
                t0
            };

            let reg_out = reg_out_opt.unwrap();
            (r0, r1, rt, reg_out)
        };
        trace!("arith binop registers: r0={r0}, r1={r1}, rtmp={rt}, rout={reg_out}");

        if let Type::Pointer(t) = lhs_type {
            let size = t.size_bytes();
            out.push(UnresolvedInstruction::Instruction(Instruction::Imm(
                rt,
                Literal12Bit::new_checked(size as u16).unwrap(),
            )));
            out.push(UnresolvedInstruction::Instruction(Instruction::Mul(
                rt, r1, rt,
            )));
            if is_add {
                out.push(UnresolvedInstruction::Instruction(Instruction::Add(
                    reg_out, r0, rt,
                )));
            } else {
                out.push(UnresolvedInstruction::Instruction(Instruction::Sub(
                    reg_out, r0, rt,
                )));
            }
        } else if is_add {
            out.push(UnresolvedInstruction::Instruction(Instruction::Add(
                reg_out, r0, r1,
            )));
        } else {
            out.push(UnresolvedInstruction::Instruction(Instruction::Sub(
                reg_out, r0, r1,
            )));
        };
        state.set_intermediate(reg_out);
        debug!("arith op: using {reg_out} as intermediate");
        Ok(ExprRes::from_instructions(
            out,
            state,
            ExpressionDestination::Register(reg_out),
        ))
    }
}

fn binop_mul(
    mut out: Vec<UnresolvedInstruction>,
    rhs: &ExprRes,
    lhs: &ExprRes,
    _lhs_type: &Type,
    _rhs_type: &Type,
    mut state: State,
) -> Result<ExprRes, CompilerError> {
    let ops_on_stack = rhs.destination == ExpressionDestination::Stack
        && lhs.destination == ExpressionDestination::Stack;
    if ops_on_stack {
        state.reserve_temporaries(2);
        let (t0, t1) = state.get_temp_pair().unwrap();
        out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
            t0,
            Register::SP,
            StackOp::Pop,
        )));
        out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
            t1,
            Register::SP,
            StackOp::Pop,
        )));
        state.reserve_temporaries(1);
        if let Some(rt) = state.get_free() {
            out.push(UnresolvedInstruction::Instruction(Instruction::Mul(
                rt, t0, t1,
            )));
            state.set_intermediate(rt);
            Ok(ExprRes::from_instructions(
                out,
                state,
                ExpressionDestination::Register(rt),
            ))
        } else {
            out.push(UnresolvedInstruction::Instruction(Instruction::Mul(
                t0, t0, t1,
            )));
            out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                t0,
                Register::SP,
                StackOp::Push,
            )));
            Ok(ExprRes::from_instructions(
                out,
                state,
                ExpressionDestination::Stack,
            ))
        }
    } else {
        let mut reg_opt_out = None;
        let mut reg_opt_other = None;
        if let ExpressionDestination::Register(r) = rhs.destination {
            reg_opt_out = Some(r);
            r
        } else {
            let r = state.get_temp().unwrap();
            out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                r,
                Register::SP,
                StackOp::Pop,
            )));
            reg_opt_other = Some(r);
            r
        };
        if let ExpressionDestination::Register(r) = lhs.destination {
            if reg_opt_out.is_some() {
                reg_opt_other = reg_opt_out;
            }
            reg_opt_out = Some(r);
            r
        } else {
            let r = state.get_temp().unwrap();
            out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                r,
                Register::SP,
                StackOp::Pop,
            )));
            reg_opt_other = Some(r);
            r
        };
        let reg_out = reg_opt_out.unwrap();
        let other = reg_opt_other.unwrap();
        out.push(UnresolvedInstruction::Instruction(Instruction::Mul(
            reg_out, reg_out, other,
        )));
        state.set_intermediate(reg_out);
        Ok(ExprRes::from_instructions(
            out,
            state,
            ExpressionDestination::Register(reg_out),
        ))
    }
}
