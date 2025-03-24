use crate::expression::definitions::{BlockType, ControlFlowInstruction, Instruction, SimpleInstruction, TypedInstruction};
use crate::expression::{ActiveCompilation, Expression};
use crate::parser::{LabelIndex, ValueType};
use crate::vector::{vector_from_vec, WasmVec};
use crate::Index;
use anyhow::{ensure, Context};
use std::iter;

#[derive(Debug, Copy, Clone)]
pub(crate) enum LabelType {
    Block,
    Loop,
}

#[derive(Debug)]
pub(crate) enum UnresolvedJump {
    Branch(LabelIndex),
    BranchIf(LabelIndex),
    BranchTable(WasmVec<LabelIndex>, LabelIndex),
    Return,
}


pub(crate) enum TypeVec<'a> {
    Empty,
    Single(ValueType),
    Borrowed(&'a [ValueType]),
    Owned(WasmVec<ValueType>),
}

pub(crate) struct CompiledBlockType<'a> {
    pub(crate) input: TypeVec<'a>,
    pub(crate) output: TypeVec<'a>,
}

pub(crate) enum DesugaredInstruction<'a> {
    Label(LabelType, CompiledBlockType<'a>),
    EndCf,
    JumpCf(UnresolvedJump),
    Simple(SimpleInstruction),
}

pub(crate) enum IncompleteCfType<'a> {
    If,
    IfElse {
        if_block: Vec<DesugaredInstruction<'a>>,
    },
    Block,
    Loop,
}

pub(crate) struct IncompleteCf<'a> {
    block_type: BlockType,
    ty: IncompleteCfType<'a>,
    instructions: Vec<DesugaredInstruction<'a>>,
}

impl<'a> IncompleteCf<'a> {
    fn output(
        self,
        instructions: &mut Vec<DesugaredInstruction<'a>>,
        compiler: &mut ActiveCompilation<'_, 'a>,
    ) -> anyhow::Result<()> {
        const IF_BLOCK_TYPE: CompiledBlockType<'static> = CompiledBlockType {
            input: TypeVec::Single(ValueType::I32),
            output: TypeVec::Empty,
        };

        match self.ty {
            IncompleteCfType::If => {
                // if desugar:
                // block (param i32)
                // i32.eqz
                // br_if 0
                // /*if-so*/
                // end
                ensure!(
                    self.block_type.resolve(compiler.context.types)? == (&[], &[]),
                    "invalid if block return type"
                );
                instructions.extend([
                    DesugaredInstruction::Label(LabelType::Block, IF_BLOCK_TYPE),
                    DesugaredInstruction::Simple(SimpleInstruction::Typed(TypedInstruction::I32EqZ)),
                    DesugaredInstruction::JumpCf(UnresolvedJump::BranchIf(LabelIndex::ZERO)),
                ]);
                instructions.extend(self.instructions);
                instructions.push(DesugaredInstruction::EndCf)
            }
            IncompleteCfType::IfElse { if_block } => {
                // if else desugar:
                // block (param i32 /*rest*/) (result /*rest*/)
                // block (param i32)
                // br_if 0
                // /*if-not*/ but increment all br instructions by 1
                // br 1
                // end
                // /*if-so*/
                // end

                let if_so = if_block;
                let mut if_not = self.instructions;

                let inc = |idx: &mut LabelIndex| {
                    idx.0.0.checked_add(1).context("instructions too nested")
                        .map(|new_idx| idx.0.0 = new_idx)
                };

                for instr in &mut if_not {
                    if let DesugaredInstruction::JumpCf(jmp) = instr {
                        match jmp {
                            UnresolvedJump::Branch(idx)
                            | UnresolvedJump::BranchIf(idx) => inc(idx)?,
                            UnresolvedJump::BranchTable(idxs, idx) => {
                                idxs.iter_mut().chain(iter::once(idx)).try_for_each(inc)?
                            }
                            UnresolvedJump::Return => {}
                        }
                    }
                }

                let (input, output) = self.block_type.resolve(compiler.context.types)?;

                let input = match input {
                    [] => TypeVec::Single(ValueType::I32),
                    _ => {
                        let vec = input.iter().copied()
                            .chain(iter::once(ValueType::I32))
                            .collect::<Vec<_>>();
                        TypeVec::Owned(vector_from_vec(vec)?)
                    }
                };

                instructions.extend([
                    DesugaredInstruction::Label(
                        LabelType::Block,
                        CompiledBlockType {
                            input,
                            output: TypeVec::Borrowed(output),
                        },
                    ),
                    DesugaredInstruction::Label(LabelType::Block, IF_BLOCK_TYPE),
                ]);
                instructions.push(DesugaredInstruction::JumpCf(UnresolvedJump::BranchIf(
                    LabelIndex::ZERO,
                )));
                instructions.extend(if_not);
                instructions.push(DesugaredInstruction::JumpCf(UnresolvedJump::Branch(
                    LabelIndex(Index(1)),
                )));
                instructions.push(DesugaredInstruction::EndCf);
                instructions.extend(if_so);
                instructions.push(DesugaredInstruction::EndCf)
            }
            ty @ (IncompleteCfType::Block | IncompleteCfType::Loop) => {
                let label = match ty {
                    IncompleteCfType::Block => LabelType::Block,
                    IncompleteCfType::Loop => LabelType::Loop,
                    _ => unreachable!(),
                };

                let (input, output) = self.block_type.resolve(compiler.context.types)?;
                let ty = CompiledBlockType {
                    input: TypeVec::Borrowed(input),
                    output: TypeVec::Borrowed(output),
                };

                instructions.push(DesugaredInstruction::Label(label, ty));
                instructions.extend(self.instructions);
                instructions.push(DesugaredInstruction::EndCf)
            }
        }

        Ok(())
    }
}

pub(crate) fn desugar<'env>(
    expression: Expression,
    compiler: &mut ActiveCompilation<'_, 'env>,
) -> anyhow::Result<Vec<DesugaredInstruction<'env>>> {
    let mut underlying_instructions = vec![];

    let mut control_flow: Vec<IncompleteCf<'env>> = vec![];

    for instruction in expression.instructions.into_iter() {
        macro_rules! instructions {
            () => {{
                control_flow
                    .last_mut()
                    .map(|cf| &mut cf.instructions)
                    .unwrap_or(&mut underlying_instructions)
            }};
        }

        macro_rules! push_jump {
            ($ty:ident $( ($($tt:tt)*) )?) => {{
                instructions!().push(DesugaredInstruction::JumpCf(UnresolvedJump::$ty $( ($($tt)*) )?));
            }};
        }

        macro_rules! push_cf {
            ($ty:ident($bt:expr)) => {{
                control_flow.push(IncompleteCf {
                    block_type: $bt,
                    ty: IncompleteCfType::$ty,
                    instructions: vec![],
                })
            }};
        }

        match instruction {
            Instruction::ControlFlow(instr) => match instr {
                ControlFlowInstruction::Block(bt) => push_cf!(Block(bt)),
                ControlFlowInstruction::Loop(bt) => push_cf!(Loop(bt)),
                ControlFlowInstruction::If(bt) => push_cf!(If(bt)),
                ControlFlowInstruction::Else => {
                    let Some(IncompleteCf {
                                 ty: cf_ty @ IncompleteCfType::If,
                                 instructions: if_block,
                                 ..
                             }) = control_flow.last_mut()
                    else {
                        unreachable!(
                            "there has to be an if as the last control flow, otherwise the expression would fail to decode; this is a bug"
                        )
                    };
                    *cf_ty = IncompleteCfType::IfElse { if_block: std::mem::take(if_block) };
                }
                ControlFlowInstruction::End => {
                    let cf = control_flow.pop()
                        .expect("there has to be some kind of control flow, otherwise the expression would fail to decode; this is a bug");
                    cf.output(instructions!(), compiler)?
                }
                ControlFlowInstruction::Branch(label) => push_jump!(Branch(label)),
                ControlFlowInstruction::BranchIf(label) => push_jump!(BranchIf(label)),
                ControlFlowInstruction::BranchTable(labels, fallback) => {
                    push_jump!(BranchTable(labels, fallback))
                }
                ControlFlowInstruction::Return => push_jump!(Return),
            },
            Instruction::Simple(instr) => instructions!().push(DesugaredInstruction::Simple(instr)),
        }
    }

    assert!(control_flow.is_empty(), "unbalanced control flow; this is a bug");

    Ok(underlying_instructions)
}
