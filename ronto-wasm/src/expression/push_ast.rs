use crate::expression::definitions::TypedInstruction;
use crate::expression::desugar::{LabelType, TypeVec, UnresolvedJump};
use crate::expression::label_resolution::{ResolvedInstruction, StructuredControlFlow};
use crate::expression::{ActiveCompilation, IncompleteJump, Label};
use crate::runtime::values_len;
use crate::vector::{Index, WasmVec};

pub(super) fn push_ast(
    expression: Vec<ResolvedInstruction<'_>>,
    compiler: &mut ActiveCompilation,
) -> anyhow::Result<()> {
    for instruction in expression {
        if compiler.flags.dead_code_elimination
            && (compiler.hit_unreachable() && !matches!(instruction, ResolvedInstruction::End))
        {
            continue;
        }

        match instruction {
            ResolvedInstruction::StructureCf(StructuredControlFlow {
                start,
                end,
                block_type,
                label_type,
            }) => {
                let goto = match label_type {
                    LabelType::Loop => start,
                    LabelType::Block => end,
                };

                // add nop to slide on
                compiler.add_instruction(TypedInstruction::Nop)?;

                let into_vec = |type_info| match type_info {
                    TypeVec::Empty => const { WasmVec::new() },
                    TypeVec::Single(val) => WasmVec::from_trusted_box(Box::new([val])),
                    TypeVec::Borrowed(borrowed) => WasmVec::from_trusted_box(borrowed.into()),
                    TypeVec::Owned(vec) => vec,
                };

                let input = into_vec(block_type.input);
                compiler.add_label(Label {
                    goto: Index(goto.0),
                    return_address: Index(compiler.values_len.0 - values_len(&input)?.0),
                    type_check_address: Index::from_usize(compiler.values.len() - input.len()),
                    input,
                    output: into_vec(block_type.output),
                    label_type,
                })?
            }
            ResolvedInstruction::End => {
                // add nop to slide on
                compiler.add_instruction(TypedInstruction::Nop)?;
                let label = compiler.pop_label()?;
                if compiler.take_unreachable() {
                    compiler
                        .values
                        .truncate(label.type_check_address.as_usize());
                    compiler.values.extend_from_slice(&label.output);
                    compiler.values_len.0 = label.return_address.0 + values_len(&label.output)?.0;
                }
            }
            ResolvedInstruction::JumpCf(jmp) => {
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
            ResolvedInstruction::Simple(lit) => compiler.add_instruction(lit)?,
        }
    }

    Ok(())
}
