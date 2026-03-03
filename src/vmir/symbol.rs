use core::fmt;

use super::ty::Interned;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol<'tcx>(pub(crate) Interned<'tcx, str>);

impl<'tcx> Symbol<'tcx> {
    pub const fn new_static(s: &'static str) -> Self {
        Symbol(Interned(s))
    }

    pub fn as_str(self) -> &'tcx str {
        self.0 .0
    }
}

// fmt

impl fmt::Debug for Symbol<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

impl fmt::Display for Symbol<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}
