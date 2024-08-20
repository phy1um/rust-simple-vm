use simplevm::{
    resolve::UnresolvedInstruction, Instruction, Literal12Bit, Literal7Bit, Nibble, Register,
    StackOp, TestOp,
};

use crate::ast;
use crate::compile::block::BlockVariable;
use crate::compile::error::CompilerError;
use crate::compile::resolve::Type;

pub fn load_address_to(
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

pub fn binop_compare(out: &mut Vec<UnresolvedInstruction>, a: Register, b: Register, op: TestOp) {
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
        Register::C,
        Register::Zero,
        Register::Zero,
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

pub fn assign_from_stack_to_local(out: &mut Vec<UnresolvedInstruction>, ty: &Type, offset: u8) {
    out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
        Register::C,
        Register::SP,
        StackOp::Pop,
    )));
    out.push(UnresolvedInstruction::Instruction(Instruction::Add(
        Register::B,
        Register::BP,
        Register::Zero,
    )));
    out.push(UnresolvedInstruction::Instruction(Instruction::AddImm(
        Register::B,
        Literal7Bit::new_checked(offset).unwrap(),
    )));
    write_value(out, ty, Register::C, Register::B);
}

pub fn load_local_addr_to(out: &mut Vec<UnresolvedInstruction>, offset: u8, reg: Register) {
    out.push(UnresolvedInstruction::Instruction(Instruction::Add(
        reg,
        Register::BP,
        Register::Zero,
    )));
    out.push(UnresolvedInstruction::Instruction(Instruction::AddImm(
        reg,
        Literal7Bit::new_checked(offset).unwrap(),
    )));
}

pub fn assign_from_stack_to_arg(out: &mut Vec<UnresolvedInstruction>, index: u8) {
    out.push(UnresolvedInstruction::Instruction(Instruction::Stack(
        Register::C,
        Register::SP,
        StackOp::Pop,
    )));
    out.push(UnresolvedInstruction::Instruction(Instruction::Add(
        Register::B,
        Register::BP,
        Register::Zero,
    )));
    out.push(UnresolvedInstruction::Instruction(
        Instruction::AddImmSigned(
            Register::B,
            Literal7Bit::from_signed(-2 * (index as i8 + 3)).unwrap(),
        ),
    ));
    out.push(UnresolvedInstruction::Instruction(Instruction::StoreWord(
        Register::C,
        Register::B,
        Register::Zero,
    )));
}

pub fn load_arg_addr_to(out: &mut Vec<UnresolvedInstruction>, index: u8, reg: Register) {
    out.push(UnresolvedInstruction::Instruction(Instruction::Add(
        reg,
        Register::BP,
        Register::Zero,
    )));
    out.push(UnresolvedInstruction::Instruction(
        Instruction::AddImmSigned(
            reg,
            Literal7Bit::from_signed(-2 * (index as i8 + 3)).unwrap(),
        ),
    ));
}

pub fn write_value(
    out: &mut Vec<UnresolvedInstruction>,
    ty: &Type,
    reg_value: Register,
    reg_addr: Register,
) {
    match ty.size_bytes() {
        0 => (),
        1 => out.push(UnresolvedInstruction::Instruction(Instruction::StoreByte(
            reg_value,
            reg_addr,
            Register::Zero,
        ))),
        2 => out.push(UnresolvedInstruction::Instruction(Instruction::StoreWord(
            reg_value,
            reg_addr,
            Register::Zero,
        ))),
        n => panic!("uh oh we can't assign {n} bytes by value"),
    }
}

// ASSUME len fields >= 1
pub fn get_stack_field_offset(
    out: &mut Vec<UnresolvedInstruction>,
    fields: &[ast::Identifier],
    var_type: &Type,
    head_var: &BlockVariable,
    target_register: Register,
) -> Result<(), CompilerError> {
    let head_name = fields.first().unwrap();

    // check for special case of 1 field, as then we don't want to deref
    // eg let *int b := 2; return b; <-- this needs to return 2
    if var_type.is_pointer() && fields.len() > 1 {
        match head_var {
            BlockVariable::Local(offset, _) => {
                load_local_addr_to(out, *offset as u8, target_register);
                out.push(UnresolvedInstruction::Instruction(Instruction::LoadWord(
                    target_register,
                    target_register,
                    Register::Zero,
                )));
            }
            BlockVariable::Arg(offset, _) => {
                load_arg_addr_to(out, *offset as u8, target_register);
                out.push(UnresolvedInstruction::Instruction(Instruction::LoadWord(
                    target_register,
                    target_register,
                    Register::Zero,
                )));
            }
            BlockVariable::Global(addr, _) => {
                out.extend(load_address_to(*addr, target_register, Register::M))
            }
            BlockVariable::Const(_) => {
                return Err(CompilerError::NonStructFieldReference(
                    head_name.to_string(),
                    Type::Int,
                ));
            }
        }
    } else {
        match head_var {
            BlockVariable::Local(offset, _) => {
                load_local_addr_to(out, *offset as u8, target_register)
            }
            BlockVariable::Arg(offset, _) => load_arg_addr_to(out, *offset as u8, target_register),
            // TODO: probably not valid or reachable
            BlockVariable::Global(addr, _) => {
                out.extend(load_address_to(*addr, target_register, Register::M))
            }
            BlockVariable::Const(_) => {
                return Err(CompilerError::NonStructFieldReference(
                    head_name.to_string(),
                    Type::Int,
                ));
            }
        }
    };

    if fields.len() == 1 {
        return Ok(());
    }

    let mut struct_fields = match var_type.clone() {
        Type::Struct(sf) => sf,
        Type::Pointer(t) => {
            if let Type::Struct(sf) = *t {
                sf
            } else {
                return Err(CompilerError::NonStructFieldReference(
                    head_name.to_string(),
                    var_type.clone(),
                ));
            }
        }
        _ => {
            return Err(CompilerError::NonStructFieldReference(
                head_name.to_string(),
                var_type.clone(),
            ));
        }
    };

    let mut current_type = var_type.clone();
    for (i, field) in fields[1..].iter().enumerate() {
        let (field_type, field_offset) = {
            let (field_type, field_offset) =
                struct_fields
                    .get(&field.0)
                    .ok_or(CompilerError::StructFieldDoesNotExist(
                        field.to_string(),
                        current_type.clone(),
                    ))?;
            (field_type.clone(), *field_offset)
        };

        out.push(UnresolvedInstruction::Instruction(Instruction::AddImm(
            target_register,
            Literal7Bit::new_checked(field_offset as u8).unwrap(),
        )));
        if field_type.is_pointer() && i < fields.len() - 2 {
            out.push(UnresolvedInstruction::Instruction(Instruction::LoadWord(
                target_register,
                target_register,
                Register::Zero,
            )));
        }

        current_type = field_type.clone();

        if i < fields.len() - 2 {
            struct_fields = match field_type {
                Type::Struct(sf) => sf,
                Type::Pointer(t) => {
                    if let Type::Struct(sf) = *t {
                        sf
                    } else {
                        return Err(CompilerError::NonStructFieldReference(
                            field.0.to_string(),
                            var_type.clone(),
                        ));
                    }
                }
                _ => {
                    return Err(CompilerError::NonStructFieldReference(
                        field.to_string(),
                        field_type.clone(),
                    ));
                }
            };
        };
    }
    Ok(())
}
