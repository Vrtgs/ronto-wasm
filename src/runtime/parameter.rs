#![allow(private_interfaces)]

use crate::Stack;
use crate::expression::ActiveCompilation;
use crate::parser::ValueType;
use crate::runtime::parameter::sealed::{SealedInput, SealedOutput};
use crate::runtime::{Value, ValueInner};
use std::convert::Infallible;
use std::fmt::{Debug, Display, Formatter};
use std::iter;
use std::marker::PhantomData;

trait Parameter: Sized + 'static {
    const TYPE: &'static [ValueType];

    fn from_stack(stack: &mut Vec<Value>) -> Option<Self>;

    fn into_values(self) -> impl IntoIterator<Item = Value>;
}

pub(crate) mod sealed {
    use crate::expression::ActiveCompilation;
    use crate::parser::ValueType;
    use crate::runtime::Value;
    use std::fmt::Formatter;

    pub trait ArgumentFmt: Sized + 'static {
        fn fmt_args(f: &mut Formatter) -> std::fmt::Result;
    }

    pub trait SealedInput: ArgumentFmt {
        fn get_from_compiler(compiler: &mut ActiveCompilation) -> bool;
        fn get_checked(stack: &mut Vec<Value>) -> Option<Self>;
        fn get(stack: &mut Vec<Value>) -> Self {
            Self::get_checked(stack)
                .expect("validation should make sure we never get a value from the stack if it doesn't exist, or has a mismatched type")
        }
        fn into_input(self) -> Vec<Value>;
        fn subtype(ty: &[ValueType]) -> bool;
    }

    pub trait SealedOutput: ArgumentFmt {
        fn update_compiler(compiler: &mut ActiveCompilation);
        fn push(self, stack: &mut Vec<Value>);
        fn get_output(stack: &mut Vec<Value>) -> Option<Self>;
        fn subtype(ty: &[ValueType]) -> bool;
    }
}

pub(crate) fn fmt_ty_vec(f: &mut Formatter, ty_vec: &[ValueType]) -> std::fmt::Result {
    match ty_vec {
        [] => f.write_str("()"),
        [ty] => Display::fmt(ty, f),
        params => {
            let mut tuple = f.debug_tuple("");

            struct DisplayDebug<T>(T);

            impl<T: Display> Debug for DisplayDebug<T> {
                fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                    T::fmt(&self.0, f)
                }
            }

            for param in params {
                tuple.field(&DisplayDebug(param));
            }
            tuple.finish()
        }
    }
}

impl<T: Parameter> sealed::ArgumentFmt for T {
    fn fmt_args(f: &mut Formatter) -> std::fmt::Result {
        fmt_ty_vec(f, T::TYPE)
    }
}

impl<T: Parameter> SealedInput for T {
    fn get_from_compiler(compiler: &mut ActiveCompilation) -> bool {
        compiler.pop_slice(T::TYPE).is_ok()
    }

    fn get_checked(stack: &mut Vec<Value>) -> Option<Self> {
        T::from_stack(stack)
    }

    fn into_input(self) -> Vec<Value> {
        T::into_values(self).into_iter().collect()
    }

    fn subtype(ty: &[ValueType]) -> bool {
        ty == T::TYPE
    }
}

impl<T: Parameter> SealedOutput for T {
    fn update_compiler(compiler: &mut ActiveCompilation) {
        compiler.push_slice(T::TYPE)
    }

    fn push(self, stack: &mut Vec<Value>) {
        stack.extend(T::into_values(self))
    }

    fn get_output(stack: &mut Vec<Value>) -> Option<Self> {
        T::from_stack(stack)
    }

    fn subtype(ty: &[ValueType]) -> bool {
        ty == T::TYPE
    }
}

pub trait FunctionInput: SealedInput {}
pub trait FunctionOutput: SealedOutput {}

impl<T: SealedInput> FunctionInput for T {}
impl<T: SealedOutput> FunctionOutput for T {}

impl Parameter for () {
    const TYPE: &'static [ValueType] = &[];

    fn from_stack(_: &mut Vec<Value>) -> Option<Self> {
        Some(())
    }

    fn into_values(self) -> impl IntoIterator<Item = Value> {
        iter::empty()
    }
}

impl sealed::ArgumentFmt for Infallible {
    fn fmt_args(f: &mut Formatter) -> std::fmt::Result {
        f.write_str("never")
    }
}

impl SealedOutput for Infallible {
    fn update_compiler(compiler: &mut ActiveCompilation) {
        compiler.set_unreachable()
    }

    fn push(self, _: &mut Vec<Value>) {
        match self {}
    }

    fn get_output(_: &mut Vec<Value>) -> Option<Self> {
        unreachable!()
    }

    fn subtype(_: &[ValueType]) -> bool {
        true
    }
}

impl<T: ValueInner> Parameter for T {
    const TYPE: &'static [ValueType] = &[T::TYPE];

    fn from_stack(stack: &mut Vec<Value>) -> Option<Self> {
        stack.pop().and_then(T::from)
    }

    fn into_values(self) -> impl IntoIterator<Item = Value> {
        iter::once(self.into())
    }
}

pub fn fmt_fn_signature<T: FunctionInput, U: FunctionOutput>() -> impl Display {
    struct FmtArgs<T, U>(PhantomData<fn(T) -> U>);

    impl<T: FunctionInput, U: FunctionOutput> Display for FmtArgs<T, U> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            <T as sealed::ArgumentFmt>::fmt_args(f)?;
            f.write_str(" -> ")?;
            <U as sealed::ArgumentFmt>::fmt_args(f)
        }
    }

    FmtArgs::<T, U>(PhantomData)
}

macro_rules! impl_param_for_tuple {
    ($(($($T:literal),+ $(,)?))+) => {paste::paste! {
        $(
        impl<$([<T $T>]: ValueInner),*> Parameter for ($([<T $T>]),+,) {
            const TYPE: &'static [ValueType] = &[$([<T $T>]::TYPE),+];

            fn from_stack(stack: &mut Vec<Value>) -> Option<Self> {
                let [$( [<t $T>] ),+] = stack.pop_n()?;
                Some(($([<T $T>]::from([<t $T>])?),+,))
            }

            fn into_values(self) -> impl IntoIterator<Item=Value> {
                [$(self.$T.into()),+]
            }
        }
        )+
    }};
}

impl_param_for_tuple! {
    (0, 1)
    (0, 1, 2)
    (0, 1, 2, 3)
    (0, 1, 2, 3, 4)
    (0, 1, 2, 3, 4, 5)
    (0, 1, 2, 3, 4, 5, 6)
    (0, 1, 2, 3, 4, 5, 6, 7)
    (0, 1, 2, 3, 4, 5, 6, 7, 8)
}
