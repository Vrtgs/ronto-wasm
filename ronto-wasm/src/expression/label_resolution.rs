use crate::expression::ActiveCompilation;
use crate::expression::definitions::SimpleInstruction;
use crate::expression::desugar::{
    CompiledBlockType, DesugaredInstruction, LabelType, UnresolvedJump,
};
use crate::vector::Index;
use anyhow::bail;

pub(crate) struct IncompleteCf<'env> {
    block_type: CompiledBlockType<'env>,
    start: Index,
    ty: LabelType,
}

pub(crate) struct StructuredControlFlow<'env> {
    pub(crate) start: Index,
    pub(crate) end: Index,
    pub(crate) block_type: CompiledBlockType<'env>,
    pub(crate) label_type: LabelType,
}

enum ControlFlow<'env> {
    Incomplete,
    Complete(StructuredControlFlow<'env>),
}

enum IncompleteResolveInstruction<'env> {
    StructureCf(ControlFlow<'env>),
    JumpCf(UnresolvedJump),
    Simple(SimpleInstruction),
    EndCf,
}

pub(crate) enum ResolvedInstruction<'env> {
    StructureCf(StructuredControlFlow<'env>),
    End,
    JumpCf(UnresolvedJump),
    Simple(SimpleInstruction),
}

impl<'a> IncompleteResolveInstruction<'a> {
    fn complete(self) -> ResolvedInstruction<'a> {
        match self {
            IncompleteResolveInstruction::StructureCf(ControlFlow::Complete(cf)) => {
                ResolvedInstruction::StructureCf(cf)
            }
            IncompleteResolveInstruction::StructureCf(ControlFlow::Incomplete) => {
                unreachable!("this is a bug, we only ever parse expressions with balanced closings")
            }
            IncompleteResolveInstruction::EndCf => ResolvedInstruction::End,
            IncompleteResolveInstruction::JumpCf(jmp) => ResolvedInstruction::JumpCf(jmp),
            IncompleteResolveInstruction::Simple(lit) => ResolvedInstruction::Simple(lit),
        }
    }
}

pub(crate) fn resolve<'env>(
    expression: Vec<DesugaredInstruction<'env>>,
    _: &mut ActiveCompilation<'_, 'env>,
) -> anyhow::Result<Vec<ResolvedInstruction<'env>>> {
    let mut instructions = vec![];
    let mut control_flow: Vec<IncompleteCf> = vec![];

    for instruction in expression.into_iter() {
        macro_rules! get_i {
            () => {
                match Index::try_from_usize(instructions.len()) {
                    Some(i) => i,
                    None => bail!("expression too long to compile"),
                }
            };
        }

        match instruction {
            DesugaredInstruction::Label(ty, bt) => {
                let cf = IncompleteCf {
                    block_type: bt,
                    start: get_i!(),
                    ty,
                };
                instructions.push(IncompleteResolveInstruction::StructureCf(
                    ControlFlow::Incomplete,
                ));
                control_flow.push(cf);
            }
            DesugaredInstruction::EndCf => {
                let end = get_i!();
                let cf = control_flow
                    .pop()
                    .expect("there should be something on the control flow stack");

                let complete_cf = StructuredControlFlow {
                    start: cf.start,
                    end,
                    block_type: cf.block_type,
                    label_type: cf.ty,
                };
                *instructions.get_mut(cf.start.as_usize()).unwrap() =
                    IncompleteResolveInstruction::StructureCf(ControlFlow::Complete(complete_cf));
                instructions.push(IncompleteResolveInstruction::EndCf)
            }
            DesugaredInstruction::JumpCf(jmp) => {
                instructions.push(IncompleteResolveInstruction::JumpCf(jmp))
            }
            DesugaredInstruction::Simple(simple) => {
                instructions.push(IncompleteResolveInstruction::Simple(simple))
            }
        }
    }

    Ok(instructions
        .into_iter()
        .map(IncompleteResolveInstruction::complete)
        .collect())
}
