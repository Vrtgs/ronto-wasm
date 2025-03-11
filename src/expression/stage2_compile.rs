use crate::expression::definitions::SimpleInstruction;
use crate::expression::stage1_compile::{LiteralInstruction, ResolvedInstruction, StructuredControlFlow, UnresolvedJump};
use crate::expression::ActiveCompilation;
use crate::parser::{LabelIndex, NumericType, ValueType};
use crate::vector::{Index, WasmVec};

#[derive(Debug, Copy, Clone)]
pub(super) enum LabelType {
    Loop,
    Block,
}

#[derive(Debug)]
pub(super) enum LabeledInstruction {
    Label {
        input: WasmVec<ValueType>,
        output: WasmVec<ValueType>,
        ty: LabelType,
        start: Index,
        end: Index,
    },
    End,
    JumpCf(UnresolvedJump),
    Literal(LiteralInstruction),
}

pub(super) fn compile(expression: WasmVec<ResolvedInstruction>, compiler: &mut ActiveCompilation) -> anyhow::Result<WasmVec<LabeledInstruction>> {
    let mut instructions = vec![];
    let mut extra_offset = 0;
    for instruction in expression {
        macro_rules! make_label {
            ($ty:ident; $start:expr, $end:expr, $block_type:expr) => {{
                let (input, output) = $block_type.resolve(compiler.context.types)?;
                let to_owned = |tys: &[ValueType]| WasmVec::from_trusted_box(tys.into());
                make_label!($ty; $start, $end; (to_owned(input)) -> to_owned(output))
            }};
            ($ty:ident; $start:expr, $end:expr; ($input:expr) -> $output:expr) => {{
                LabeledInstruction::Label {
                    input: $input,
                    output: $output,
                    ty: LabelType::$ty,
                    start: Index($start.0 + extra_offset),
                    end: Index($end.0 + extra_offset),
                }
            }};
        }
        match instruction {
            ResolvedInstruction::StructureCf(cf) => {
                match cf {
                    StructuredControlFlow::IfElse { .. } => todo!(),
                    StructuredControlFlow::If { start, end } => {
                        let i32_input = WasmVec::from_trusted_box(Box::new([
                            ValueType::NumericType(NumericType::I32)
                        ]));

                        extra_offset += 2;
                        
                        instructions.push(make_label!(Block; start, end; (i32_input) -> (WasmVec::new())));
                        instructions.push(LabeledInstruction::Literal(
                            LiteralInstruction::Simple(SimpleInstruction::I32EqZ)
                        ));
                        instructions.push(LabeledInstruction::JumpCf(
                            UnresolvedJump::BranchIf(LabelIndex(Index(0)))
                        ));
                    }
                    StructuredControlFlow::Loop { start, end, block_type } => {
                        instructions.push(make_label!(Loop; start, end, block_type))
                    }
                    StructuredControlFlow::Block { start, end, block_type } => {
                        instructions.push(make_label!(Block; start, end, block_type))
                    }
                }
            }
            ResolvedInstruction::EndCf => instructions.push(LabeledInstruction::End),
            ResolvedInstruction::JumpCf(jump) => {
                instructions.push(LabeledInstruction::JumpCf(jump))
            }
            ResolvedInstruction::Literal(lit) => {
                instructions.push(LabeledInstruction::Literal(lit))
            }
        }
    }

    Ok(WasmVec::from_trusted_box(instructions.into_boxed_slice()))
}