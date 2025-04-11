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
        impl core::fmt::Display for $struct {
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

idx!(Local, "_{}");
idx!(ExpLocal, "e{}");

idx!(BasicBlock, "bb{}");
idx!(Label, "lbl{}");

idx!(LocalDefId, "id{}");

idx!(CompoundIdx, "c{}");
