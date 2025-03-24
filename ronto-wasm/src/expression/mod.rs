mod definitions;
mod typed_instruction_code;

mod untyped_instruction_code;

use crate::expression::definitions::{
    ControlFlowInstruction, Instruction, SimpleInstruction, TypedInstruction, UntypedInstruction,
};
use crate::expression::desugar::LabelType;
use crate::expression::untyped_instruction_code::UntypedInstructionCode;
use crate::parser::{
    DataIndex, Decode, ExternIndex, FunctionDefinition, FunctionIndex, GlobalIndex, GlobalType,
    LabelIndex, LocalIndex, MemoryIndex, ReferenceType, TableIndex, TableType, TypeIndex, TypeInfo,
    ValueType,
};
use crate::read_tape::ReadTape;
use crate::runtime::memory_buffer::MemoryFault;
use crate::runtime::parameter::sealed::SealedInput;
use crate::runtime::{
    CompilerFlags, ReferenceValue, Store, Trap, Value, ValueStack, WasmContext, WordStore,
    values_len,
};
use crate::vector::{Index, WasmVec, vector_from_vec};
use crate::{Stack, VirtualMachine, runtime};
use anyhow::{Context, bail, ensure};
use std::cmp::PartialEq;
use std::io::Read;

impl From<MemoryFault> for Trap {
    #[cold]
    fn from(_: MemoryFault) -> Self {
        Trap::memory_fault()
    }
}

pub type ExecutionResult<T = ()> = Result<T, Trap>;

#[derive(Debug, PartialEq, Clone)]
pub struct Expression {
    instructions: WasmVec<Instruction>,
}

impl Expression {
    pub fn function_call(idx: FunctionIndex) -> Self {
        Self {
            instructions: WasmVec::from_trusted_box(Box::new([Instruction::Simple(
                SimpleInstruction::Typed(TypedInstruction::RefFunc(idx)),
            )])),
        }
    }

    pub fn const_eval(&self, vm: &Store) -> Option<Value> {
        let [Instruction::Simple(instruction)] = &*self.instructions else {
            return None;
        };

        Some(match *instruction {
            SimpleInstruction::Typed(ref typed) => match *typed {
                TypedInstruction::I32Const(val) => Value::I32(val as u32),
                TypedInstruction::I64Const(val) => Value::I64(val as u64),
                TypedInstruction::F32Const(val) => Value::F32(val),
                TypedInstruction::F64Const(val) => Value::F64(val),
                TypedInstruction::V128Const(val) => Value::V128(val),
                TypedInstruction::RefNull(ReferenceType::Extern) => {
                    Value::Ref(ReferenceValue::Extern(ExternIndex::NULL))
                }
                TypedInstruction::RefNull(ReferenceType::Function) => {
                    Value::Ref(ReferenceValue::Function(FunctionIndex::NULL))
                }
                TypedInstruction::RefFunc(func) => Value::Ref(ReferenceValue::Function(func)),
                _ => {
                    debug_assert!(!typed.const_available());
                    return None;
                }
            },
            // TODO: only allow imported globals
            SimpleInstruction::Untyped(UntypedInstruction::GlobalGet(glob)) => {
                vm.load_global(glob)?
            }
            _ => return None,
        })
    }
}

macro_rules! declare_untyped {
    ($($name:ident $(($($data:tt)*))?),*) => {
        paste::paste!{
            #[derive(Debug, Eq, PartialEq, Copy, Clone)]
            pub enum CompiledUntypedInstruction {
                $([<$name 32>] $(($($data)*))?,
                [<$name 64>] $(($($data)*))?,
                [<$name 128>] $(($($data)*))?),*
            }

            impl CompiledUntypedInstruction {
                fn execute(self, context: &mut WasmContext) -> ExecutionResult {
                    #[allow(unused_parens)]
                    #[allow(non_snake_case)]
                    {paste::paste!{match self {
                        $(
                        CompiledUntypedInstruction::[<$name 32>] $(($($data)*))? => {
                            <untyped_instruction_code::$name as UntypedInstructionCode<($($($data)*)?)>>::call::<u32>(($($($data)*)?), context)
                        },
                        CompiledUntypedInstruction::[<$name 64>] $(($($data)*))? => {
                            <untyped_instruction_code::$name as UntypedInstructionCode<($($($data)*)?)>>::call::<u64>(($($($data)*)?), context)
                        },
                        CompiledUntypedInstruction::[<$name 128>] $(($($data)*))? => {
                            <untyped_instruction_code::$name as UntypedInstructionCode<($($($data)*)?)>>::call::<u128>(($($($data)*)?), context)
                        },
                        )*
                    }}}
                }
            }
        }
    };
}

type Local = LocalIndex;
type Global = GlobalIndex;

declare_untyped! {
    Select,
    Drop,
    LocalGet(Local),
    LocalSet(Local),
    LocalTee(Local),
    GlobalGet(Global),
    GlobalSet(Global)
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct Jump {
    // let stack_start = return_address;
    // let stack_end = stack.len() - return_amount;
    // context.stack.drain(stack_start..stack_end);
    goto: Index,
    return_address: Index,
    return_amount: Index,
}

impl Jump {
    #[inline(always)]
    fn jump(self, bp: Index, ip: &mut Index, context: &mut WasmContext) {
        let stack_start = Index(bp.0 + self.return_address.0).as_usize();
        let stack_end =
            Index(Index::from_usize(context.stack.len()).0 - self.return_amount.0).as_usize();
        context.stack.0.drain(stack_start..stack_end);
        *ip = self.goto
    }
}

#[derive(Debug, PartialEq, Clone)]
struct JumpTableEntry {
    goto: Index,
    return_address: Index,
}

#[derive(Debug, PartialEq, Clone)]
enum JumpType {
    Jump(Jump),
    JumpIf(Jump),
    JumpTable {
        table: WasmVec<JumpTableEntry>,
        fallback: JumpTableEntry,
        return_amount: Index,
    },
}

enum IncompleteJump {
    Jump(Label),
    JumpIf(Label),
    JumpTable(WasmVec<Label>, Label),
}

#[derive(Debug, PartialEq, Clone)]
enum CompiledInstruction {
    Jump(JumpType),
    Untyped(CompiledUntypedInstruction),
    Typed(TypedInstruction),
}

#[derive(Debug, PartialEq, Clone)]
pub struct WasmFunction {
    instructions: WasmVec<CompiledInstruction>,
    pre_compiled_locals: WasmVec<runtime::Word>,
    parameters_len: Index,
}

pub(crate) struct WasmCompilationContext<'a> {
    pub(crate) globals: &'a WasmVec<GlobalType>,
    pub(crate) mem_count: Index,
    pub(crate) data_count: Index,
    pub(crate) function_signatures: &'a WasmVec<TypeIndex>,
    pub(crate) types: &'a WasmVec<TypeInfo>,
    pub(crate) tables: &'a WasmVec<TableType>,
}

#[derive(Debug, Clone)]
pub struct Label {
    goto: Index,
    return_address: Index,
    type_check_address: Index,
    input: WasmVec<ValueType>,
    output: WasmVec<ValueType>,
    label_type: LabelType,
}

impl Label {
    fn takes(&self) -> &[ValueType] {
        match self.label_type {
            LabelType::Loop => &self.input,
            LabelType::Block => &self.output,
        }
    }
}

pub(crate) struct ActiveCompilation<'a, 'env> {
    flags: &'env CompilerFlags,
    function_index: FunctionIndex,
    context: &'a WasmCompilationContext<'env>,
    expected_output: &'env WasmVec<ValueType>,
    instructions: Vec<CompiledInstruction>,
    locals: WasmVec<(ValueType, LocalIndex)>,
    values: Vec<ValueType>,
    values_len: Index,
    labels: Vec<Label>,
    hit_unreachable: bool,
}

trait Compile: Sized {
    fn compile(self, compiler: &mut ActiveCompilation) -> anyhow::Result<CompiledInstruction>;
}

impl Compile for IncompleteJump {
    fn compile(self, compiler: &mut ActiveCompilation) -> anyhow::Result<CompiledInstruction> {
        let jump = match self {
            IncompleteJump::Jump(label) => {
                let takes = label.takes();
                compiler.ensure_slice_top(takes)?;
                compiler.set_unreachable();
                JumpType::Jump(Jump {
                    goto: label.goto,
                    return_address: label.return_address,
                    return_amount: values_len(takes)?,
                })
            }
            IncompleteJump::JumpIf(label) => {
                let Some(ValueType::I32) = compiler.pop() else {
                    bail!("br_if failed to compile; no condition on top of the stack")
                };
                let takes = label.takes();

                compiler.ensure_slice_top(takes)?;
                JumpType::JumpIf(Jump {
                    goto: label.goto,
                    return_address: label.return_address,
                    return_amount: values_len(takes)?,
                })
            }
            IncompleteJump::JumpTable(table, fallback) => {
                let takes = fallback.takes();
                let table = table.try_map(|label| {
                    ensure!(label.takes() == takes, "mismatched_types");

                    Ok(JumpTableEntry {
                        goto: label.goto,
                        return_address: label.return_address,
                    })
                })?;

                compiler.ensure_slice_top(takes)?;
                compiler.set_unreachable();
                JumpType::JumpTable {
                    table,
                    fallback: JumpTableEntry {
                        goto: fallback.goto,
                        return_address: fallback.return_address,
                    },
                    return_amount: values_len(takes)?,
                }
            }
        };

        Ok(CompiledInstruction::Jump(jump))
    }
}

impl Compile for UntypedInstruction {
    fn compile(self, _compiler: &mut ActiveCompilation) -> anyhow::Result<CompiledInstruction> {
        macro_rules! compile {
            ($($name: ident $(($($data:ident),*))?),*) => {
                match self {
                    UntypedInstruction::Select => untyped_instruction_code::Select::compile((), _compiler),
                    UntypedInstruction::SelectT(opt) => untyped_instruction_code::Select::compile(opt, _compiler),
                    $(UntypedInstruction::$name $(($($data),*))? => {
                        untyped_instruction_code::$name::compile(($($($data),*)?), _compiler)
                    }),*
                }
            };
        }
        Ok(CompiledInstruction::Untyped(compile! {
            Drop,
            LocalGet (local),
            LocalSet (local),
            LocalTee (local),
            GlobalGet (global),
            GlobalSet (global)
        }?))
    }
}

impl Compile for TypedInstruction {
    fn compile(self, compiler: &mut ActiveCompilation) -> anyhow::Result<CompiledInstruction> {
        ensure!(
            self.simulate(compiler),
            "invalid instruction {}",
            self.name()
        );
        Ok(CompiledInstruction::Typed(self))
    }
}

impl Compile for SimpleInstruction {
    fn compile(self, compiler: &mut ActiveCompilation) -> anyhow::Result<CompiledInstruction> {
        match self {
            SimpleInstruction::Untyped(untyped) => untyped.compile(compiler),
            SimpleInstruction::Typed(typed) => typed.compile(compiler),
        }
    }
}

impl ActiveCompilation<'_, '_> {
    pub(crate) fn pop_type_input(&mut self, r#type: TypeIndex) -> bool {
        self.context
            .types
            .get(r#type.0)
            .is_some_and(|ty| self.pop_slice(&ty.parameters).is_ok())
    }

    pub(crate) fn push_type_output(&mut self, r#type: TypeIndex) -> bool {
        self.context
            .types
            .get(r#type.0)
            .map(|ty| self.push_slice(&ty.result))
            .is_some()
    }

    pub(crate) fn simulate_call_indirect(&mut self, ty: TypeIndex) -> bool {
        self.pop_type_input(ty) && self.push_type_output(ty)
    }

    pub(crate) fn simulate_call(&mut self, function: FunctionIndex) -> bool {
        if let Some(&ty) = self.context.function_signatures.get(function.0) {
            return self.simulate_call_indirect(ty);
        }
        false
    }

    pub(crate) fn take_unreachable(&mut self) -> bool {
        std::mem::replace(&mut self.hit_unreachable, false)
    }

    pub(crate) fn set_unreachable(&mut self) {
        self.hit_unreachable = true;
    }

    pub(crate) fn hit_unreachable(&self) -> bool {
        self.hit_unreachable
    }

    pub(crate) fn peek(&mut self) -> Option<&ValueType> {
        self.values.last()
    }

    pub(crate) fn pop(&mut self) -> Option<ValueType> {
        self.values.pop().inspect(|val| {
            self.values_len.0 -= val.word_size();
        })
    }

    pub(crate) fn pop_n<const N: usize>(&mut self) -> Option<[ValueType; N]> {
        self.values.pop_n().inspect(|vals| {
            const { assert!(Index::MAX.as_usize() >= N) }
            self.values_len.0 -= values_len(vals).unwrap().0;
        })
    }

    pub(crate) fn push(&mut self, value: ValueType) {
        self.values_len.0 += value.word_size();
        self.values.push(value)
    }

    pub(crate) fn push_slice(&mut self, data: &[ValueType]) {
        self.values_len.0 += values_len(data).unwrap().0;
        self.values.extend_from_slice(data)
    }

    pub(crate) fn is_slice_top(&self, data: &[ValueType]) -> bool {
        let stack = &self.values;
        data.len() <= stack.len() && stack[stack.len() - data.len()..] == *data
    }

    pub(crate) fn ensure_slice_top(&self, data: &[ValueType]) -> anyhow::Result<()> {
        ensure!(self.is_slice_top(data), "invalid slice top");
        Ok(())
    }

    pub(crate) fn pop_slice(&mut self, data: &[ValueType]) -> anyhow::Result<()> {
        let stack = &mut self.values;
        if data.len() > stack.len() {
            self.values_len.0 = 0;
            stack.clear();
            bail!("not enough elements on the stack")
        }

        self.values_len.0 -= values_len(data)?.0;
        ensure!(
            stack.drain(stack.len() - data.len()..).as_slice() == data,
            "type mismatched"
        );

        Ok(())
    }

    pub(crate) fn get_global(&self, global: GlobalIndex) -> Option<GlobalType> {
        self.context.globals.get(global.0).copied()
    }

    pub(crate) fn get_local(&self, local: LocalIndex) -> Option<(ValueType, LocalIndex)> {
        self.locals.get(local.0).copied()
    }

    pub(crate) fn get_table(&self, local: TableIndex) -> Option<TableType> {
        self.context.tables.get(local.0).copied()
    }

    pub(crate) fn has_memory(&self, memory: MemoryIndex) -> bool {
        self.context.mem_count > memory.0
    }

    pub(crate) fn has_data(&self, data: DataIndex) -> bool {
        self.context.data_count > data.0
    }
}

impl ActiveCompilation<'_, '_> {
    fn resolve_return(&self) -> Label {
        self.labels
            .first()
            .cloned()
            .expect("there should always be at least one label for function return")
    }

    fn resolve_label(&self, idx: LabelIndex) -> anyhow::Result<Label> {
        self.labels
            .iter()
            .nth_back(idx.0.as_usize())
            .cloned()
            .with_context(|| format!("couldn't resolve label {}", idx.0.0))
    }

    fn add_label(&mut self, label: Label) -> anyhow::Result<()> {
        ensure!(self.is_slice_top(&label.input), "invalid label input");
        self.labels.push(label);
        Ok(())
    }

    fn pop_label(&mut self) -> anyhow::Result<Label> {
        if self.labels.len() <= 1 {
            assert_ne!(self.labels.len(), 0);
            bail!("can't pop the function label")
        }

        let label = self.labels.pop().unwrap();
        let valid_label_return = self.hit_unreachable()
            || (Some(self.values.len())
                == label
                    .type_check_address
                    .as_usize()
                    .checked_add(label.output.len())
                && self.is_slice_top(&label.output));

        ensure!(valid_label_return, "invalid label output");
        Ok(label)
    }

    fn add_instruction(&mut self, instr: impl Compile) -> anyhow::Result<()> {
        let instr = instr.compile(self)?;
        self.instructions.push(instr);
        Ok(())
    }

    fn compile(mut self) -> anyhow::Result<WasmFunction> {
        if !self.hit_unreachable() {
            self.pop_slice(self.expected_output)?;
            ensure!(
                self.values.is_empty(),
                "too many values on the stack before function return"
            );
        }

        optimize_instructions::optimize(&mut self);
        let instructions =
            vector_from_vec(std::mem::take(&mut self.instructions)).context("function too long")?;

        let type_id = self
            .context
            .function_signatures
            .get(self.function_index.0)
            .unwrap();
        let r#type = self.context.types.get(type_id.0).unwrap();
        let parameters_len = values_len(&r#type.parameters)?;

        let mut locals = ValueStack::new();

        self.locals
            .iter()
            .for_each(|&(ty, _)| match Value::new(ty) {
                Value::I32(val) => val.push_words(&mut locals.0),
                Value::I64(val) => val.push_words(&mut locals.0),
                Value::F32(val) => val.push_words(&mut locals.0),
                Value::F64(val) => val.push_words(&mut locals.0),
                Value::V128(val) => val.push_words(&mut locals.0),
                Value::Ref(ReferenceValue::Function(func)) => func.push_words(&mut locals.0),
                Value::Ref(ReferenceValue::Extern(extern_idx)) => {
                    extern_idx.push_words(&mut locals.0)
                }
            });

        Ok(WasmFunction {
            parameters_len,
            pre_compiled_locals: vector_from_vec(locals.0)?,
            instructions,
        })
    }
}

impl<'a> WasmCompilationContext<'a> {
    fn start_compilation(
        &self,
        flags: &'a CompilerFlags,
        (function, index): (FunctionDefinition, FunctionIndex),
        ty: TypeIndex,
    ) -> anyhow::Result<(Expression, ActiveCompilation<'_, 'a>)> {
        let function_signature = self.types.get(ty.0).context("invalid type index")?;

        let locals = WasmVec::try_from(
            function_signature
                .parameters
                .iter()
                .copied()
                .chain(function.locals)
                .collect::<Box<[_]>>(),
        )
        .context("too many parameters and locals in function")?;

        Ok((
            function.body,
            ActiveCompilation {
                flags,
                function_index: index,
                context: self,
                expected_output: &function_signature.result,
                instructions: vec![],
                locals: WasmVec::from_trusted_box(
                    locals
                        .iter()
                        .scan(0, |state, new| {
                            let prev = *state;
                            *state += new.word_size();
                            Some((*new, LocalIndex(Index(prev))))
                        })
                        .collect::<Box<[_]>>(),
                ),
                values: vec![],
                values_len: Index(0),
                labels: vec![Label {
                    goto: Index(u32::MAX),
                    return_address: Index(0),
                    type_check_address: Index(0),
                    label_type: LabelType::Block,
                    input: function_signature.parameters.clone(),
                    output: function_signature.result.clone(),
                }],
                hit_unreachable: false,
            },
        ))
    }
}

mod desugar;
mod label_resolution;
mod optimize_instructions;
mod push_ast;

impl WasmFunction {
    pub fn new(
        options: &CompilerFlags,
        definition: (FunctionDefinition, FunctionIndex),
        ty: TypeIndex,
        compiler: &mut WasmCompilationContext,
    ) -> anyhow::Result<Self> {
        let (expr, mut active_compilation) = compiler.start_compilation(options, definition, ty)?;

        let desugared = desugar::desugar(expr, &mut active_compilation)?;
        let labeled = label_resolution::resolve(desugared, &mut active_compilation)?;
        push_ast::push_ast(labeled, &mut active_compilation)?;

        active_compilation.compile()
    }

    pub(crate) fn eval(
        &self,
        virtual_machine: &VirtualMachine,
        stack: &mut ValueStack,
        call_depth: usize,
    ) -> ExecutionResult {
        let new_stack_len = stack.len() - self.parameters_len.as_usize();
        let mut locals =
            Vec::with_capacity(self.parameters_len.as_usize() + self.pre_compiled_locals.len());
        locals.extend_from_slice(&stack.0[new_stack_len..]);
        locals.extend_from_slice(&self.pre_compiled_locals);
        stack.0.truncate(new_stack_len);

        let context = &mut WasmContext {
            virtual_machine,
            locals: WasmVec::from_trusted_box(locals.into_boxed_slice()),
            stack,
            call_depth,
        };

        let bp = Index::from_usize(context.stack.len());
        let mut ip = Index::ZERO;

        macro_rules! jump {
            ($jmp: expr) => {{
                $jmp.jump(bp, &mut ip, context);
                continue;
            }};
        }

        while let Some(instruction) = self.instructions.get(ip) {
            // if !cfg!(test) {
            //     println!("Stack: {:?}", context.stack);
            //     println!("Locals: {:?}", context.locals);
            //     println!("Next instruction: {:?}", instruction);
            //     std::io::stdin().lines().next().transpose().unwrap();
            // }

            match instruction {
                CompiledInstruction::Jump(jump) => match jump {
                    JumpType::Jump(jmp) => jump!(jmp),
                    JumpType::JumpIf(jmp) => {
                        let cond = u32::get(context.stack);
                        if cond != 0 {
                            jump!(jmp)
                        }
                    }
                    &JumpType::JumpTable {
                        ref table,
                        ref fallback,
                        return_amount,
                    } => {
                        let idx = Index::get(context.stack);
                        let jmp = table.get(idx).unwrap_or(fallback);
                        jump!(Jump {
                            goto: jmp.goto,
                            return_address: jmp.return_address,
                            return_amount
                        })
                    }
                },
                CompiledInstruction::Untyped(instr) => instr.execute(context)?,
                CompiledInstruction::Typed(instr) => instr.execute(context)?,
            }

            let Some(next) = ip.0.checked_add(1) else {
                break;
            };
            ip.0 = next
        }

        debug_assert!(bp <= Index::from_usize(context.stack.len()));

        Ok(())
    }
}

impl Decode for Expression {
    fn decode(file: &mut ReadTape<impl Read>) -> anyhow::Result<Self> {
        #[derive(Debug)]
        enum IncompleteControlFlow {
            Block,
            Loop,
            If,
            IfElse,
        }

        let mut instructions = vec![];
        let mut control_flow = vec![];

        loop {
            let instruction = Instruction::decode(file)?;

            match instruction {
                Instruction::ControlFlow(ControlFlowInstruction::End) => {
                    if control_flow.pop().is_none() {
                        break;
                    }
                }
                Instruction::ControlFlow(ControlFlowInstruction::Block(_)) => {
                    control_flow.push(IncompleteControlFlow::Block)
                }
                Instruction::ControlFlow(ControlFlowInstruction::Loop(_)) => {
                    control_flow.push(IncompleteControlFlow::Loop)
                }
                Instruction::ControlFlow(ControlFlowInstruction::If(_)) => {
                    control_flow.push(IncompleteControlFlow::If)
                }
                Instruction::ControlFlow(ControlFlowInstruction::Else) => {
                    let Some(r#if @ IncompleteControlFlow::If) = control_flow.last_mut() else {
                        bail!("unexpected else clause");
                    };
                    *r#if = IncompleteControlFlow::IfElse
                }
                _ => {}
            }

            instructions.push(instruction);
        }

        ensure!(
            control_flow.is_empty(),
            "unescaped control flow: {:?}",
            control_flow
        );

        Ok(Self {
            instructions: vector_from_vec(instructions)?,
        })
    }
}
