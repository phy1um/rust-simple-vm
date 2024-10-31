use crate::compile::block::Block;
use simplevm::{resolve::UnresolvedInstruction, Instruction, Register, StackOp};

pub(crate) fn optimize_function(block: &mut Block) {
    remove_noops(block);
    stack_shuffle(block);
}

fn stack_shuffle(block: &mut Block) {
    let mut new_instructions = Vec::new();
    let mut last_push: Option<(Register, Register)> = None;
    for instruction in &block.instructions {
        if let UnresolvedInstruction::Instruction(Instruction::Stack(tgt, sp, op)) = instruction {
            if *op == StackOp::Push {
                if let Some((pt, psp)) = last_push {
                    new_instructions.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                        pt,
                        psp,
                        StackOp::Push,
                    )));
                }
                last_push = Some((*tgt, *sp));
            } else if *op == StackOp::Pop {
                if let Some((prev_tgt, prev_sp)) = last_push {
                    if prev_tgt != *tgt || prev_sp != *sp {
                        new_instructions.push(UnresolvedInstruction::Instruction(
                            Instruction::Stack(prev_tgt, prev_sp, StackOp::Push),
                        ));
                        new_instructions.push(instruction.clone());
                    }
                    last_push = None;
                } else {
                    last_push = None;
                    new_instructions.push(instruction.clone());
                }
            } else {
                if let Some((pt, psp)) = last_push {
                    new_instructions.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                        pt,
                        psp,
                        StackOp::Push,
                    )));
                }
                last_push = None;
                new_instructions.push(instruction.clone());
            }
        } else {
            if let Some((pt, psp)) = last_push {
                new_instructions.push(UnresolvedInstruction::Instruction(Instruction::Stack(
                    pt,
                    psp,
                    StackOp::Push,
                )));
            }
            last_push = None;
            new_instructions.push(instruction.clone());
        }
    }
    block.instructions = new_instructions;
}

fn remove_noops(block: &mut Block) {
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
                if tgt == a && n.value == 0 {
                    None
                } else {
                    Some(instruction)
                }
            }
            UnresolvedInstruction::Instruction(Instruction::ShiftRightLogical(tgt, a, n)) => {
                if tgt == a && n.value == 0 {
                    None
                } else {
                    Some(instruction)
                }
            }
            UnresolvedInstruction::Instruction(Instruction::ShiftRightArithmetic(tgt, a, n)) => {
                if tgt == a && n.value == 0 {
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
