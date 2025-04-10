use crate::parse::ConstKind;

use super::{ast::idx::LocalDefId, Interned};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefId {
    /// Is this a reference to a Viper builtin?
    builtin: bool,
    /// The index of the member in the program.
    index: LocalDefId,
}

impl DefId {
    pub fn is_local(self) -> bool {
        !self.builtin
    }

    pub fn as_local(self) -> Option<LocalDefId> {
        self.is_local().then_some(self.index)
    }

    pub fn expect_local(self) -> LocalDefId {
        self.as_local().expect("Expected a local def id")
    }

    pub(crate) const fn builtin(index: usize) -> Self {
        Self {
            builtin: true,
            index: LocalDefId::mk(index),
        }
    }
}

impl From<LocalDefId> for DefId {
    fn from(index: LocalDefId) -> Self {
        Self {
            builtin: false,
            index,
        }
    }
}

impl From<&LocalDefId> for DefId {
    fn from(id: &LocalDefId) -> Self {
        Self::from(*id)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol<'tcx>(pub(crate) Interned<'tcx, str>);

impl<'tcx> Symbol<'tcx> {
    pub fn as_str(self) -> &'tcx str {
        self.0 .0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Const<'tcx>(pub(crate) Interned<'tcx, ConstKind>);

impl<'tcx> Const<'tcx> {
    pub fn kind(self) -> &'tcx ConstKind {
        self.0 .0
    }
}
