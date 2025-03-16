use crate::expression::definitions::SimpleInstruction;
use crate::expression::stage1_compile::{LiteralInstruction, UnresolvedJump};
use crate::expression::stage2_compile::{LabelType, LabeledInstruction};
use crate::expression::{ActiveCompilation, Compile, CompiledInstruction, IncompleteJump, Label};
use crate::vector::{Index, WasmVec};

impl Compile for LiteralInstruction {
    fn compile(self, compiler: &mut ActiveCompilation) -> anyhow::Result<CompiledInstruction> {
        match self {
            LiteralInstruction::Parametric(inst) => inst.compile(compiler),
            LiteralInstruction::Simple(inst) => inst.compile(compiler),
        }
    }
}

pub(super) fn compile(
    expression: WasmVec<LabeledInstruction>,
    compiler: &mut ActiveCompilation,
) -> anyhow::Result<()> {
    // let mut labels_count = Index(0);

    for instruction in expression {
        match instruction {
            LabeledInstruction::Label {
                input,
                output,
                ty,
                start,
                end,
            } => {
                // labels_count.0 += 1;

                let goto = match ty {
                    LabelType::Loop => start,
                    LabelType::Block => end,
                };

                // FIXME this shouldn't require a nop just to slide on
                compiler.add_instruction(SimpleInstruction::Nop)?;

                compiler.add_label(Label {
                    goto: Index(goto.0),
                    return_address: Index::from_usize(compiler.values.len() - input.len()),
                    input,
                    output,
                    label_type: ty,
                })?
            }
            LabeledInstruction::End => {
                // FIXME this shouldn't require a nop just to slide on
                compiler.add_instruction(SimpleInstruction::Nop)?;
                let label = compiler.pop_label()?;
                if compiler.take_unreachable() {
                    compiler.values.truncate(label.return_address.as_usize());
                    compiler.values.extend_from_slice(&label.output)
                }
            }
            LabeledInstruction::JumpCf(jmp) => {
                let jump = match jmp {
                    UnresolvedJump::Branch(label) => {
                        IncompleteJump::Jump(compiler.resolve_label(label)?)
                    }
                    UnresolvedJump::BranchIf(label) => {
                        IncompleteJump::JumpIf(compiler.resolve_label(label)?)
                    }
                    UnresolvedJump::BranchTable(labels, fallback) => {
                        let fallback = compiler.resolve_label(fallback)?;
                        let table = labels.try_map(|label| compiler.resolve_label(label))?;
                        IncompleteJump::JumpTable(table, fallback)
                    }
                    UnresolvedJump::Return => IncompleteJump::Jump(compiler.resolve_return()),
                };
                compiler.add_instruction(jump)?
            }
            LabeledInstruction::Literal(lit) => compiler.add_instruction(lit)?,
        }
    }

    Ok(())
}
