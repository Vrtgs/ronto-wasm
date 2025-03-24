use crate::expression::definitions::TypedInstruction;
use crate::expression::{ActiveCompilation, CompiledInstruction, JumpType};
use crate::Index;
use std::iter;

fn optimize_nop(compiler: &mut ActiveCompilation) {
    let instructions = &mut compiler.instructions;
    let is_nop = |instruction: &CompiledInstruction| {
        matches!(
            instruction,
            CompiledInstruction::Typed(
                TypedInstruction::Nop
                | TypedInstruction::F32ReinterpretI32 | TypedInstruction::I32ReinterpretF32
                | TypedInstruction::F64ReinterpretI64 | TypedInstruction::I64ReinterpretF64
            )
        )
    };

    let mut prefix_nop = Vec::<u32>::with_capacity(instructions.len() + 1);
    prefix_nop.push(0);
    for instruction in &*instructions {
        let mut nop_count = prefix_nop.last().copied().unwrap();
        if is_nop(instruction) {
            nop_count += 1;
        }
        prefix_nop.push(nop_count)
    }

    instructions.retain_mut(|instr| {
        if is_nop(instr) {
            return false;
        }

        let apply_offset = |goto: &mut Index| {
            if let Some(offset) = prefix_nop.get(goto.as_usize()).copied() {
                goto.0 -= offset
            }
        };

        if let CompiledInstruction::Jump(jmp) = instr {
            match jmp {
                JumpType::Jump(jmp) | JumpType::JumpIf(jmp) => apply_offset(&mut jmp.goto),
                JumpType::JumpTable { table, fallback, .. } => {
                    table.iter_mut().chain(iter::once(fallback))
                        .for_each(|jmp| apply_offset(&mut jmp.goto))
                }
            }
        }

        true
    });
}

pub fn optimize(_compiler: &mut ActiveCompilation) {
    macro_rules! from_flags {
        ($($flag:ident)*) => {
            $(if _compiler.flags.$flag {
                $flag(_compiler);
            })*
        };
    }

    from_flags!(optimize_nop);
}