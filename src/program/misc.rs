use core::fmt;

use crate::parse::{ConstHeapKind, ConstKind};

use super::{Interned, LocalDefId, TyCtxt};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol<'tcx>(pub(crate) Interned<'tcx, str>);

impl<'tcx> Symbol<'tcx> {
    pub fn as_str(self) -> &'tcx str {
        self.0 .0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Const<'tcx>(pub(crate) Interned<'tcx, ConstKind>);

impl<'tcx> Const<'tcx> {
    pub fn kind(self) -> &'tcx ConstKind {
        self.0 .0
    }

    pub fn as_bool(self) -> Option<bool> {
        match self.kind() {
            ConstKind::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_int(self) -> Option<&'tcx num::BigInt> {
        match self.kind() {
            ConstKind::Int(i) => Some(i),
            _ => None,
        }
    }

    pub fn is_wildcard(self) -> bool {
        matches!(self.kind(), ConstKind::Wildcard)
    }

    pub fn is_epsilon(self) -> bool {
        matches!(self.kind(), ConstKind::Epsilon)
    }

    pub fn is_heap(self) -> bool {
        matches!(self.kind(), ConstKind::Heap(..))
    }

    pub fn is_imprecise(self) -> bool {
        self.is_wildcard() || self.is_heap()
    }
}

// fmt

impl fmt::Debug for DefId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ctx = unsafe { TyCtxt::global_ref_unchecked() };
        if let Some(name) = ctx.item_name(*self) {
            write!(f, "{name}")
        } else {
            write!(f, "DefId({:?}, {:?})", self.builtin, self.index)
        }
    }
}

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

impl fmt::Debug for Const<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind())
    }
}

impl fmt::Display for ConstKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use num::{One, Zero};
        match self {
            ConstKind::Bool(b) => write!(f, "{b}"),
            ConstKind::Int(i) => write!(f, "{i}"),
            ConstKind::Real(r) if r.is_zero() => write!(f, "none"),
            ConstKind::Real(r) if r.is_one() => write!(f, "write"),
            ConstKind::Real(r) => write!(f, "{} / {}", r.numer(), r.denom()),
            ConstKind::Null => write!(f, "null"),
            ConstKind::Epsilon => write!(f, "epsilon"),
            ConstKind::Wildcard => write!(f, "wildcard"),
            ConstKind::Heap(ConstHeapKind::Old) => write!(f, "□"),
            ConstKind::Heap(ConstHeapKind::SelfFraming) => write!(f, "▣"),
        }
    }
}
