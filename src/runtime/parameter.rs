use crate::parser;
use crate::parser::ValueType;
use crate::runtime::parameter::sealed::{SealedInput, SealedOutput};
use crate::runtime::{Validator, Value, ValueInner};
use crate::Stack;
use std::convert::Infallible;
use std::fmt::{Display, Formatter};
use std::marker::PhantomData;

trait Parameter: Sized + 'static {
    const TYPE: &'static [ValueType];

    fn from_stack(stack: &mut Vec<Value>) -> Option<Self>;

    fn into_values(self) -> Vec<Value>;

    fn push(self, stack: &mut Vec<Value>);
}

pub(crate) mod sealed {
    use crate::parser::ValueType;
    use crate::runtime::{Validator, Value};
    use std::fmt::Formatter;

    pub trait ArgumentFmt: Sized + 'static {
        fn fmt_args(f: &mut Formatter) -> std::fmt::Result;
    }

    pub trait SealedInput: ArgumentFmt {
        fn validate(validator: &mut Validator) -> bool;
        fn get_checked(stack: &mut Vec<Value>) -> Option<Self>;
        fn get(stack: &mut Vec<Value>) -> Self {
            let prev = format!("{stack:?}");
            Self::get_checked(stack).unwrap_or_else(|| {
                panic!("{prev} validation should make sure we never get a value from the stack if it doesn't exist, or has a mismatched type")
            })
        }
        fn into_input(self) -> Vec<Value>;
        fn subtype(ty: &[ValueType]) -> bool;
    }

    pub trait SealedOutput: ArgumentFmt {
        fn update_validator(validator: &mut Validator);
        fn push(self, stack: &mut Vec<Value>);
        fn get_output(stack: &mut Vec<Value>) -> Option<Self>;
        fn subtype(ty: &[ValueType]) -> bool;
    }
}

impl<T: Parameter> sealed::ArgumentFmt for T {
    fn fmt_args(f: &mut Formatter) -> std::fmt::Result {
        parser::fmt_ty_vec(f, T::TYPE)
    }
}

impl<T: Parameter> SealedInput for T {
    fn validate(validator: &mut Validator) -> bool {
        validator.pop_slice(T::TYPE)
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
    fn update_validator(validator: &mut Validator) {
        validator.push_slice(T::TYPE)
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

    fn into_values(self) -> Vec<Value> {
        vec![]
    }

    fn push(self, _: &mut Vec<Value>) {}
}

impl sealed::ArgumentFmt for Infallible {
    fn fmt_args(f: &mut Formatter) -> std::fmt::Result {
        f.write_str("never")
    }
}

impl SealedOutput for Infallible {
    fn update_validator(validator: &mut Validator) {
        validator.set_unreachable()
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

    fn into_values(self) -> Vec<Value> {
        vec![self.into()]
    }

    fn push(self, stack: &mut Vec<Value>) {
        stack.push(self.into())
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

            fn into_values(self) -> Vec<Value> {
                vec![$(self.$T.into()),+]
            }

            fn push(self, stack: &mut Vec<Value>) {
                stack.push_n([$(self.$T.into()),+]);
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
