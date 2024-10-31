use crate::compile::block::Block;
use log::trace;
use simplevm::{resolve::UnresolvedInstruction, Instruction};

pub(crate) fn optimize_function(block: &mut Block) {
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
            x => Some(x),
        } {
            new_instructions.push(i.clone());
        }
    }
    block.instructions = new_instructions;
}
