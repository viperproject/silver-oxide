#[macro_export]
macro_rules! idx {
    ($struct:ident, $prefix:tt) => {
        #[derive(Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
        // Note: we use `u32` since this the file would need to be > ~100GB to
        // overflow this with the number of terms constructed
        pub struct $struct($crate::NonMaxU32);
        impl $struct {
            pub(crate) const fn mk(value: usize) -> Self {
                assert!(value < u32::MAX as usize);
                Self($crate::NonMaxU32::new(value as u32).unwrap())
            }
        }
        impl From<usize> for $struct {
            fn from(value: usize) -> Self {
                Self::mk(value)
            }
        }
        impl From<$struct> for usize {
            fn from(value: $struct) -> Self {
                value.0.get() as usize
            }
        }
        impl core::fmt::Debug for $struct {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                write!(f, $prefix, self.0)
            }
        }
        impl $struct {
            pub const ZERO: Self = Self($crate::NonMaxU32::ZERO);
            pub const MAX: Self = Self($crate::NonMaxU32::MAX);
        }
    };
}
macro_rules! idx_display {
    ($struct:ident, $prefix:tt) => {
        idx!($struct, $prefix);
        impl core::fmt::Display for $struct {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                core::fmt::Debug::fmt(self, f)
            }
        }
    };
}

idx_display!(Local, "_{}");
idx_display!(Temporary, "t{}");
idx_display!(ExpLocal, "e{}");
idx_display!(QuantLocal, "q{}");

idx!(BasicBlock, "bb{}");
idx!(Label, "lbl{}");
idx!(Loop, "∞{}");

pub(in crate::vmir) mod def_id {
    idx!(LocalDefId, "id{}");
}

idx!(CompoundIdx, "c{}");
idx!(VariantIdx, "v{}");
idx!(FieldIdx, "f{}");


idx!(ResourceId, "r{}");
idx!(FunctionId, "f{}");
idx!(MethodId, "m{}");
idx!(AdtId, "adt{}");
idx!(AxiomId, "ax{}");

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Maybe<T>(Option<T>);

impl<T> Maybe<T> {
    pub const TRUE: Self = Self(None);

    pub fn is_true(&self) -> bool {
        self.0.is_none()
    }
}

impl<T> From<T> for Maybe<T> {
    fn from(value: T) -> Self {
        Self(Some(value))
    }
}

// impl<T: From<usize>> From<usize> for Maybe<T> {
//     fn from(value: usize) -> Self {
//         Self(Some(T::from(value)))
//     }
// }

impl<T> core::ops::Deref for Maybe<T> {
    type Target = Option<T>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
