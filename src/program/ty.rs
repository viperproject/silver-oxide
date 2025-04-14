use core::{fmt, ops::Deref};

use super::{DefId, Interned, Interner};

#[derive(Debug, Clone, Copy)]
pub struct Types<'tcx> {
    pub bool_: Ty<'tcx>,
    pub int_: Ty<'tcx>,
    pub real_: Ty<'tcx>,
    pub ref_: Ty<'tcx>,
    pub heap_: Ty<'tcx>,
}

impl<'tcx> Types<'tcx> {
    pub(super) fn new(i: &Interner<'tcx>) -> Self {
        Self {
            bool_: i.mk_ty_from_kind(TyKind::Bool),
            int_: i.mk_ty_from_kind(TyKind::Int),
            real_: i.mk_ty_from_kind(TyKind::Real),
            ref_: i.mk_ty_from_kind(TyKind::Ref),
            heap_: i.mk_ty_from_kind(TyKind::Heap),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ty<'tcx>(pub(crate) Interned<'tcx, TyKind<'tcx>>);

impl<'tcx> Ty<'tcx> {
    pub fn kind(self) -> &'tcx TyKind<'tcx> {
        self.0 .0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyList<'tcx>(pub(crate) Interned<'tcx, [Ty<'tcx>]>);

impl<'tcx> TyList<'tcx> {
    pub fn as_slice(self) -> &'tcx [Ty<'tcx>] {
        self.0 .0
    }
}

impl<'tcx> Deref for TyList<'tcx> {
    type Target = [Ty<'tcx>];
    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum TyKind<'tcx> {
    Bool,
    Int,
    Real,
    Ref,
    Domain(DefId, TyList<'tcx>),
    /// The return ty of a `Field` or `Predicate`, the only valid operation on
    /// this ty is to deref/perm it with a heap of `Resource`. A deref returns
    /// the contained type.
    ResourceId(Ty<'tcx>),
    /// The deref value of a `Predicate`
    Compound(CompoundId),
    Heap,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct CompoundId {
    /// The `Predicate`/`Function`(/`Method`?)
    pub did: DefId,
    /// `None` for `Predicate`, `Some(false)` for `Function`, and `Some(false |
    /// true)` for `Method`. Indicates if talking about the postcondition.
    pub contract: Option<bool>,
}

#[derive(Default)]
pub struct TyWalker<'tcx> {
    stack: Vec<Ty<'tcx>>,
}

impl<'tcx> Ty<'tcx> {
    pub fn walk(self) -> TyWalker<'tcx> {
        TyWalker { stack: vec![self] }
    }
}

impl<'tcx> TyWalker<'tcx> {
    pub(super) fn add_ty(&mut self, ty: Ty<'tcx>) {
        self.stack.push(ty);
    }
}

impl<'tcx> Iterator for TyWalker<'tcx> {
    type Item = Ty<'tcx>;
    fn next(&mut self) -> Option<Self::Item> {
        let ty = self.stack.pop()?;
        match *ty.kind() {
            TyKind::Domain(_, ty_list) => {
                self.stack.extend(ty_list.as_slice());
            }
            TyKind::ResourceId(ty) => {
                self.stack.push(ty);
            }
            _ => {}
        }
        Some(ty)
    }
}

// fmt

impl fmt::Debug for Ty<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind() {
            TyKind::Bool => write!(f, "bool"),
            TyKind::Int => write!(f, "int"),
            TyKind::Real => write!(f, "real"),
            TyKind::Ref => write!(f, "ref"),
            TyKind::Domain(symbol, ty_list) => write!(f, "{symbol:?}{ty_list:?}"),
            TyKind::ResourceId(ty) => write!(f, "&{ty:?}"),
            TyKind::Compound(compound_id) => write!(f, "{compound_id:?}"),
            TyKind::Heap => write!(f, "heap"),
        }
    }
}

impl fmt::Debug for TyList<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        for (i, ty) in self.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            ty.fmt(f)?;
        }
        write!(f, "]")
    }
}

impl fmt::Debug for CompoundId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.did)?;
        if let Some(false) = self.contract {
            write!(f, ".pre")?;
        } else if let Some(true) = self.contract {
            write!(f, ".post")?;
        }
        Ok(())
    }
}
