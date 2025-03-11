use crate::expression::definitions::{BlockType, ControlFlowInstruction, Instruction, ParametricInstruction, SimpleInstruction};
use crate::expression::{ActiveCompilation, Expression};
use crate::invalid_data;
use crate::parser::LabelIndex;
use crate::vector::{Index, WasmVec};

#[derive(Copy, Clone)]
pub(crate) enum IncompleteCfType {
    If,
    IfElse { else_start: Index },
    Loop,
    Block,
}

#[derive(Copy, Clone)]
pub(crate) struct IncompleteCf {
    block_type: BlockType,
    start: Index,
    ty: IncompleteCfType,
}

#[derive(Debug, Copy, Clone)]
pub(crate) enum StructuredControlFlow {
    If { start: Index, end: Index },
    IfElse { start: Index, else_start: Index, end: Index, block_type: BlockType },
    Loop { start: Index, end: Index, block_type: BlockType },
    Block { start: Index, end: Index, block_type: BlockType },
}

#[derive(Debug)]
pub(crate) enum UnresolvedJump {
    Branch(LabelIndex),
    BranchIf(LabelIndex),
    BranchTable(WasmVec<LabelIndex>, LabelIndex),
    Return,
}

#[derive(Debug)]
pub(crate) enum LiteralInstruction {
    Parametric(ParametricInstruction),
    Simple(SimpleInstruction),
}

#[derive(Copy, Clone)]
enum ControlFlow {
    Incomplete(IncompleteCf),
    Complete(StructuredControlFlow),
}

enum IncompleteResolveInstruction {
    StructureCf(ControlFlow),
    EndCf,
    JumpCf(UnresolvedJump),
    Literal(LiteralInstruction),
}

#[derive(Debug)]
pub(crate) enum ResolvedInstruction {
    StructureCf(StructuredControlFlow),
    EndCf,
    JumpCf(UnresolvedJump),
    Literal(LiteralInstruction),
}

impl IncompleteResolveInstruction {
    fn complete(self) -> ResolvedInstruction {
        match self {
            IncompleteResolveInstruction::StructureCf(ControlFlow::Complete(cf)) => ResolvedInstruction::StructureCf(cf),
            IncompleteResolveInstruction::StructureCf(ControlFlow::Incomplete(_)) => {
                unreachable!("this is a bug, we only ever parse expressions with balanced closings")
            }
            IncompleteResolveInstruction::EndCf => ResolvedInstruction::EndCf,
            IncompleteResolveInstruction::JumpCf(jmp) => ResolvedInstruction::JumpCf(jmp),
            IncompleteResolveInstruction::Literal(lit) => ResolvedInstruction::Literal(lit),
        }
    }
}

pub(crate) fn compile(expression: Expression, active_compilation: &mut ActiveCompilation) -> anyhow::Result<WasmVec<ResolvedInstruction>> {
    let mut instructions = vec![];
    let mut control_flow: Vec<IncompleteCf> = vec![];

    for instruction in expression.instructions.into_iter() {
        let i = Index::from_usize(instructions.len());

        macro_rules! push_cf {
            ($ty:ident($bt:expr)) => {{
                let cf = IncompleteCf {
                    block_type: $bt,
                    start: i,
                    ty: IncompleteCfType::$ty
                };
                instructions.push(IncompleteResolveInstruction::StructureCf(ControlFlow::Incomplete(cf)));
                control_flow.push(cf);
            }};
        }

        macro_rules! push_jump {
            ($ty:ident $( ($($tt:tt)*) )?) => {{
                instructions.push(IncompleteResolveInstruction::JumpCf(UnresolvedJump::$ty $( ($($tt)*) )?));
            }};
        }

        match instruction {
            Instruction::ControlFlow(instr) => {
                match instr {
                    ControlFlowInstruction::Block(bt) => push_cf!(Block(bt)),
                    ControlFlowInstruction::Loop(bt) => push_cf!(Loop(bt)),
                    ControlFlowInstruction::If(bt) => push_cf!(If(bt)),
                    ControlFlowInstruction::Else => {
                        let Some(IncompleteCf { ty: bt @ IncompleteCfType::If, .. }) = control_flow.last_mut() else {
                            unreachable!("there has to be an if as the last control flow; otherwise the expression would fail to decode")
                        };
                        *bt = IncompleteCfType::IfElse { else_start: i }
                    }
                    ControlFlowInstruction::End => {
                        let end = i;
                        let cf = control_flow.pop().expect("there should be something on the control flow stack");
                        let complete_cf = match cf.ty {
                            IncompleteCfType::If => {
                                let is_unit_type = cf.block_type != BlockType::Empty
                                    || cf.block_type.resolve(active_compilation.context.types)? != (&[], &[]);
                                if is_unit_type {
                                    return Err(invalid_data("if control flow can't return a type"));
                                }
                                StructuredControlFlow::If { start: cf.start, end }
                            }
                            IncompleteCfType::IfElse { else_start } => {
                                StructuredControlFlow::IfElse {
                                    start: cf.start,
                                    else_start,
                                    end,
                                    block_type: cf.block_type,
                                }
                            }
                            IncompleteCfType::Loop => StructuredControlFlow::Loop {
                                start: cf.start,
                                end,
                                block_type: cf.block_type,
                            },
                            IncompleteCfType::Block => StructuredControlFlow::Block {
                                start: cf.start,
                                end,
                                block_type: cf.block_type,
                            }
                        };
                        *instructions.get_mut(cf.start.as_usize()).unwrap() = IncompleteResolveInstruction::StructureCf(ControlFlow::Complete(
                            complete_cf
                        ));
                        instructions.push(IncompleteResolveInstruction::EndCf)
                    }
                    ControlFlowInstruction::Branch(label) => push_jump!(Branch(label)),
                    ControlFlowInstruction::BranchIf(label) => push_jump!(BranchIf(label)),
                    ControlFlowInstruction::BranchTable(labels, fallback) => push_jump!(BranchTable(labels, fallback)),
                    ControlFlowInstruction::Return => push_jump!(Return)
                }
            }
            Instruction::Parametric(instr) => instructions.push(IncompleteResolveInstruction::Literal(LiteralInstruction::Parametric(instr))),
            Instruction::Simple(instr) => instructions.push(IncompleteResolveInstruction::Literal(LiteralInstruction::Simple(instr))),
        }
    }

    Ok(WasmVec::from_trusted_box(instructions.into_iter().map(IncompleteResolveInstruction::complete).collect()))
}