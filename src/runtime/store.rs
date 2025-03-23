use crate::expression::{Expression, FunctionBody, WasmCompilationContext};
use crate::parser::InitMode;
use crate::parser::{
    Data, Element, ExportDescription, FunctionIndex, GlobalIndex, ImportDescription, TypeInfo,
    WasmBinary,
};
use crate::runtime::memory_buffer::MemoryBuffer;
use crate::runtime::{values_len, Body, Constructor, FunctionInner, GlobalValueInner, Import, Linker, ReferenceValue, Table, Value, ValueInner, ValueStack, WasmFunction, WordStore};
use crate::vector::{vector_from_vec, WasmVec};
use crate::{invalid_data, Index};
use anyhow::{ensure, Context};
use std::collections::HashMap;

pub struct Store {
    pub(crate) types: WasmVec<TypeInfo>,
    pub(crate) functions: WasmVec<FunctionInner>,
    pub(crate) tables: WasmVec<Table>,
    pub(crate) memory: WasmVec<MemoryBuffer>,
    pub(super) globals: WasmVec<GlobalValueInner>,
    pub(crate) data: WasmVec<Data>,
    pub(crate) element: WasmVec<Element>,
    pub(crate) start: Option<FunctionIndex>,
    pub(crate) exports: HashMap<Box<str>, ExportDescription>,
}

impl Store {
    pub(crate) fn load_global(&self, local: GlobalIndex) -> Option<Value> {
        self.globals.get(local.0).map(|glob| glob.load())
    }

    pub(crate) fn store_global(&self, local: GlobalIndex, value: Value) -> Result<(), ()> {
        self.globals
            .get(local.0)
            .ok_or(())
            .and_then(|glob| glob.store(value))
    }
}

impl Store {
    #[allow(clippy::too_many_arguments)]
    fn create(
        types: WasmVec<TypeInfo>,
        functions: impl IntoIterator<Item=FunctionInner>,
        tables: impl IntoIterator<Item=Table>,
        memory: impl IntoIterator<Item=MemoryBuffer>,
        exports: HashMap<Box<str>, ExportDescription>,
        global_stubs: impl IntoIterator<Item=GlobalValueInner>,
        data: impl IntoIterator<Item=Data>,
        start: Option<FunctionIndex>,
        element: impl IntoIterator<Item=Element>,
        constructors: impl IntoIterator<Item=Constructor>,
    ) -> anyhow::Result<Self> {
        fn collect_wasm_vec<T>(it: impl IntoIterator<Item=T>) -> anyhow::Result<WasmVec<T>> {
            WasmVec::try_from(it.into_iter().collect::<Box<[_]>>())
                .map_err(|_| invalid_data("too many functions in store"))
        }

        let functions = collect_wasm_vec(functions)?;

        let mut this = Store {
            types,
            functions,
            tables: collect_wasm_vec(tables)?,
            memory: collect_wasm_vec(memory)?,
            exports,
            globals: collect_wasm_vec(global_stubs)?,
            data: collect_wasm_vec(data)?,
            start,
            element: collect_wasm_vec(element)?,
        };

        for constructor in constructors {
            constructor(&mut this)?
        }

        Ok(this)
    }

    pub fn with_linker(binary: WasmBinary, linker: &Linker) -> anyhow::Result<Self> {
        let sections = binary.sections;

        let types = sections.r#type.map(|sec| sec.functions).unwrap_or_default();

        let wasm_function_types = sections
            .function
            .map(|fun| fun.signatures)
            .unwrap_or_default();
        let wasm_defined_functions = sections.code.map(|fun| fun.definitions).unwrap_or_default();
        if wasm_function_types.len() != wasm_defined_functions.len() {
            return Err(invalid_data("mismatched function signatures and functions"));
        }

        let import_stubs = sections.import.map(|fun| fun.imports).unwrap_or_default();

        let (imported_functions, imported_globals, _) = import_stubs
            .into_iter()
            .map(|imp| {
                let import = linker.get(&imp.module, &imp.name).ok_or_else(|| {
                    invalid_data(format!(
                        "unresolved import [{}]::[{}]",
                        imp.module, imp.name
                    ))
                })?;

                Ok(match (imp.description, import.clone()) {
                    (ImportDescription::Function(r#type), Import::Function(body, signature)) => {
                        let import_type = types
                            .get(r#type.0)
                            .context("invalid import type index")?;

                        let type_checks = (signature.input_check)(&import_type.parameters)
                            && (signature.output_check)(&import_type.result);

                        ensure!(type_checks,
                            "invalid function [{}]::[{}] signature, expected {import_type}",
                                imp.module, imp.name
                        );


                        let func = FunctionInner {
                            r#type,
                            body: Body::Import(body),
                        };
                        (Some(func), None, None)
                    }
                    (ImportDescription::Global(_), Import::Global(value)) => {
                        (None, Some(GlobalValueInner::Immutable(value)), None)
                    }
                    (ImportDescription::Memory(imposed_limit), Import::Memory(buffer)) => {
                        if imposed_limit.min > buffer.min() && imposed_limit.max > buffer.max() {
                            return Err(invalid_data("invalid memory buffer signature"));
                        }
                        (None, None, Some(buffer))
                    }
                    _ => return Err(invalid_data("mismatched import type")),
                })
            })
            .collect::<anyhow::Result<(Vec<_>, Vec<_>, Vec<_>)>>()?;

        let wasm_defined_globals = sections.global.map(|glob| glob.globals).unwrap_or_default();

        let globals_signatures = vector_from_vec(
            imported_globals
                .iter()
                .flatten()
                .map(|glob| glob.r#type())
                .chain(wasm_defined_globals.iter().map(|glob| glob.r#type))
                .collect(),
        )?;

        let function_signatures = vector_from_vec(
            imported_functions
                .iter()
                .flatten()
                .map(|func| func.r#type)
                .chain(wasm_function_types.iter().copied())
                .collect(),
        )?;

        let tables = sections
            .table
            .as_ref()
            .map(|sec| &sec.tables)
            .unwrap_or(const { &WasmVec::new() });

        let mut compiler = WasmCompilationContext {
            globals: &globals_signatures,
            mem_count: sections.memory.as_ref().map(|mem| mem.memories.len_idx()).unwrap_or(Index(0)),
            data_count: sections.data.as_ref().map(|mem| mem.data.len_idx()).unwrap_or(Index(0)),
            function_signatures: &function_signatures,
            types: &types,
            tables,
        };

        let functions = imported_functions
            .into_iter()
            .flatten()
            .map(anyhow::Ok)
            .chain(
                wasm_defined_functions
                    .into_iter()
                    .zip(wasm_function_types)
                    .map(|(def, r#type)| {
                        let parameters = &compiler.types.get(r#type.0)
                            .context("invalid function type")?
                            .parameters;

                        let parameters_len = values_len(parameters)?;

                        let mut locals = ValueStack::new();

                        def.locals.iter().for_each(|&ty| {
                            match Value::new(ty) {
                                Value::I32(val) => val.push_words(&mut locals.0),
                                Value::I64(val) => val.push_words(&mut locals.0),
                                Value::F32(val) => val.push_words(&mut locals.0),
                                Value::F64(val) => val.push_words(&mut locals.0),
                                Value::V128(val) => val.push_words(&mut locals.0),
                                Value::Ref(ReferenceValue::Function(func)) => func.push_words(&mut locals.0),
                                Value::Ref(ReferenceValue::Extern(extern_idx)) => extern_idx.push_words(&mut locals.0),
                            }
                        });

                        Ok(FunctionInner {
                            r#type,
                            body: Body::WasmDefined(WasmFunction {
                                parameters_len,
                                locals: vector_from_vec(locals.0).context("too many locals")?,
                                body: FunctionBody::new(def, r#type, &mut compiler)?,
                            }),
                        })
                    }),
            );

        let functions = vector_from_vec(functions.collect::<anyhow::Result<_>>()?)?;

        let (global_constructors, global_stubs) = imported_globals
            .into_iter()
            .flatten()
            .map(|val| (None, val))
            .chain(
                wasm_defined_globals
                    .into_iter()
                    .map(|glob| (Some(glob.expression), GlobalValueInner::new(glob.r#type))),
            )
            .unzip::<_, _, Vec<_>, Vec<_>>();

        let global_constructors = global_constructors
            .into_iter()
            .enumerate()
            .filter_map(|(i, expr)| expr.map(|expr| (i, expr)));

        let global_setter = |this: &mut Store| {
            for (i, global_constructor) in global_constructors {
                global_constructor
                    .const_eval(this)
                    .ok_or_else(|| invalid_data("global constructor not available in const"))
                    .and_then(|new_value| {
                        (*this.globals)[i].store_mut(new_value).map_err(|_| {
                            invalid_data("Global initialization failed, invalid return")
                        })
                    })?
            }

            Ok(())
        };

        let _const_eval_offset = |this: &Store, offset: &Expression| {
            offset
                .const_eval(this)
                .ok_or_else(|| invalid_data("active element offset not available in const"))
                .and_then(|val| {
                    <Index as ValueInner>::from(val)
                        .ok_or_else(|| invalid_data("invalid offset type"))
                })
        };

        macro_rules! init_active_segment {
            ($this: expr, $name:ident, |$index:ident, $offset:ident| $expr: expr) => {{
                for $name in $this.$name.iter() {
                    let InitMode::Active {
                        index: $index,
                        offset: ref $offset,
                    } = $name.mode
                    else {
                        continue;
                    };

                    let $offset = _const_eval_offset($this, $offset)?;

                    $expr
                }
                Ok(())
            }};
        }

        let memory_setter = move |this: &mut Store| {
            init_active_segment!(this, data, |memory_index, offset| {
                let buff = this
                    .memory
                    .get(memory_index)
                    .ok_or_else(|| invalid_data("invalid memory index offset"))?;

                buff.init(offset, &data.init).map_err(|_| {
                    invalid_data("invalid memory operation performed by active segment")
                })?;
            })
        };

        let table_setter = move |this: &mut Store| {
            init_active_segment!(this, element, |table_index, offset| {
                let values = element
                    .init
                    .iter()
                    .map(|expr| match expr.const_eval(this) {
                        Some(Value::Ref(ReferenceValue::Function(func))) => Some(func),
                        _ => None,
                    })
                    .collect::<Option<Vec<_>>>()
                    .ok_or_else(|| invalid_data("invalid element initialization"))?;

                let table = this
                    .tables
                    .get_mut(table_index)
                    .and_then(|table| match table {
                        Table::FunctionTable(table) => Some(&mut **table),
                        _ => None,
                    })
                    .ok_or_else(|| invalid_data("invalid table index"))?;

                let table = table
                    .get_mut(offset.as_usize()..)
                    .ok_or_else(|| invalid_data("invalid offset"))?;
                let len = values.len().min(table.len());
                let table = &mut table[..len];
                table.copy_from_slice(&values)
            })
        };

        let oom_to_error = |_| invalid_data("wasm module requires too much memory");

        Self::create(
            types,
            functions,
            sections
                .table
                .map(|sec| sec.tables.try_map(Table::try_from))
                .transpose()
                .map_err(oom_to_error)?
                .unwrap_or_default(),
            sections
                .memory
                .map(|sec| sec.memories.try_map(MemoryBuffer::new))
                .transpose()
                .map_err(oom_to_error)?
                .unwrap_or_default(),
            sections
                .export
                .map(|sec| {
                    sec.exports
                        .into_iter()
                        .map(|exp| (exp.name, exp.description))
                        .collect()
                })
                .unwrap_or_default(),
            global_stubs,
            sections.data.map(|data| data.data).unwrap_or_default(),
            sections.start,
            sections
                .element
                .map(|element| element.elements)
                .unwrap_or_default(),
            [
                Box::new(global_setter) as Constructor,
                Box::new(memory_setter) as Constructor,
                Box::new(table_setter) as Constructor,
            ],
        )
    }

    pub fn new(binary: WasmBinary) -> anyhow::Result<Self> {
        Self::with_linker(binary, &Linker::new())
    }
}
