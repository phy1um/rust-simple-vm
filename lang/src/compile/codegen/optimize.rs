use crate::compile::block::Block;
use log::trace;
use simplevm::{resolve::UnresolvedInstruction, Instruction, Register};

pub(crate) fn optimize_function(block: &mut Block) {
    remove_noops(block);
}

pub(crate) fn remove_noops(block: &mut Block) {
    let mut new_instructions = Vec::new();
    for instruction in &block.instructions {
        if let Some(i) = match instruction {
            UnresolvedInstruction::Instruction(Instruction::AddImm(_, lit)) => {
                if lit.value == 0 {
                    None
                } else {
                    Some(instruction)
                }
            }
            UnresolvedInstruction::Instruction(Instruction::AddImmSigned(_, lit)) => {
                if lit.value == 0 {
                    None
                } else {
                    Some(instruction)
                }
            }
            UnresolvedInstruction::Instruction(Instruction::Add(tgt, a, b)) => {
                if (tgt == a && *b == Register::Zero) || (tgt == b && *a == Register::Zero) {
                    None
                } else {
                    Some(instruction)
                }
            }
            UnresolvedInstruction::Instruction(Instruction::Sub(tgt, a, b)) => {
                if (tgt == a && *b == Register::Zero) || (tgt == b && *a == Register::Zero) {
                    None
                } else {
                    Some(instruction)
                }
            }
            UnresolvedInstruction::Instruction(Instruction::ShiftLeft(tgt, a, n)) => {
                if (tgt == a && n.value == 0) {
                    None
                } else {
                    Some(instruction)
                }
            }
            UnresolvedInstruction::Instruction(Instruction::ShiftRightLogical(tgt, a, n)) => {
                if (tgt == a && n.value == 0) {
                    None
                } else {
                    Some(instruction)
                }
            }
            UnresolvedInstruction::Instruction(Instruction::ShiftRightArithmetic(tgt, a, n)) => {
                if (tgt == a && n.value == 0) {
                    None
                } else {
                    Some(instruction)
                }
            }
            x => Some(x),
        } {
            new_instructions.push(i.clone());
        }
    }
    block.instructions = new_instructions;
}
