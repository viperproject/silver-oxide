use core::{fmt, ops::{Deref, Index}};

use crate::{vmir::{middle::{AdtId, FieldIdx, VariantIdx}, ty::*, DefId, Symbol}, AsBrackets, TiVec};

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ty<'tcx>(Interned<'tcx, TyKind<'tcx>>);

impl<'tcx> Ty<'tcx> {
    pub(super) const fn new(i: Interned<'tcx, TyKind<'tcx>>) -> Self {
        Self(i)
    }

    pub fn kind(self) -> &'tcx TyKind<'tcx> {
        self.0 .0
    }

    pub fn deref(self) -> Ty<'tcx> {
        match *self.kind() {
            TyKind::Address(ty) => ty,
            _ => panic!("Expected a resource id but have {self}"),
        }
    }

    pub fn is_primitive(self) -> bool {
        matches!(*self.kind(), TyKind::Bool | TyKind::Int | TyKind::Real | TyKind::Ref)
    }

    pub fn param(self) -> Option<ParamTy<'tcx>> {
        match *self.kind() {
            TyKind::Param(p) => Some(p),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyList<'tcx>(pub(crate) Interned<'tcx, [Ty<'tcx>]>);

impl<'tcx> TyList<'tcx> {
    pub fn empty() -> Self {
        TyList(Interned(&[]))
    }

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TyKind<'tcx> {
    Bool,
    Int,
    Real,
    Ref,
    Param(ParamTy<'tcx>),
    Domain(DomainKind, TyList<'tcx>),
    /// The return ty of a `Field` or `Predicate`, the only valid operation on
    /// this ty is to deref/perm it with a heap. A deref returns
    /// the contained type.
    Address(Ty<'tcx>),

    Heap,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DomainKind {
    Adt(AdtId),
    Domain(DomainDef),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ParamTy<'tcx> {
    pub name: Symbol<'tcx>,
    pub index: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DomainDef {
    pub id: DefId,
}

// util

impl DomainKind {
    pub fn adt<'tcx>(self, tcx: &'tcx TyCtxt<'tcx>) -> Option<AdtDef<'tcx>> {
        match self {
            DomainKind::Adt(id) => Some(tcx.interner.get_adt_def(id)),
            _ => None,
        }
    }

    pub fn domain(self) -> Option<DomainDef> {
        match self {
            DomainKind::Domain(domain) => Some(domain),
            _ => None,
        }
    }
}

// walker

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
    pub(crate) fn add_ty(&mut self, ty: Ty<'tcx>) {
        self.stack.push(ty);
    }
}

impl<'tcx> Iterator for TyWalker<'tcx> {
    type Item = Ty<'tcx>;
    fn next(&mut self) -> Option<Self::Item> {
        let ty = self.stack.pop()?;
        match *ty.kind() {
            TyKind::Domain(_, args) => {
                self.stack.extend(args.as_slice());
            }
            TyKind::Address(ty) => {
                self.stack.push(ty);
            }
            _ => {}
        }
        Some(ty)
    }
}

// folder

pub trait TypeFolder<'tcx> {
    fn interner(&self) -> &Interner<'tcx>;

    /// Call this method on a `TypeFolder` to fold a type.
    fn fold_ty(&mut self, ty: Ty<'tcx>) -> Ty<'tcx> {
        self.inner_fold_ty(ty).unwrap_or(ty)
    }

    /// The implementation of folding, to keep recursing return
    /// `Some(super_fold_ty)`. If `None` is returned, the type is not recursed
    /// any further and not changed.
    fn inner_fold_ty(&mut self, ty: Ty<'tcx>) -> Option<Ty<'tcx>>;

    fn super_fold_ty(&mut self, ty: Ty<'tcx>) -> Ty<'tcx> {
        use TyKind::*;
        match *ty.kind() {
            Domain(kind, args) => {
                let args = args.iter().map(|ty| self.fold_ty(*ty)).collect();
                let args = self.interner().mk_ty_list(args);
                self.interner().mk_ty_from_kind(TyKind::Domain(kind, args))
            }
            Address(ty) => {
                let ty = self.fold_ty(ty);
                self.interner().mk_ty_from_kind(TyKind::Address(ty))
            }
            Bool | Int | Real | Ref | Heap | Param(_) => ty,
        }
    }
}

pub struct ArgFolder<'a, 'tcx> {
    pub interner: &'a Interner<'tcx>,
    pub args: TyList<'tcx>,
}

impl<'a, 'tcx> ArgFolder<'a, 'tcx> {
    pub fn new(interner: &'a Interner<'tcx>, args: TyList<'tcx>) -> Self {
        Self { interner, args }
    }
}
impl<'a, 'tcx> TypeFolder<'tcx> for ArgFolder<'a, 'tcx> {
    fn interner(&self) -> &Interner<'tcx> {
        self.interner
    }

    fn inner_fold_ty(&mut self, ty: Ty<'tcx>) -> Option<Ty<'tcx>> {
        if let TyKind::Param(p) = *ty.kind() {
            Some(self.args[p.index as usize])
        } else {
            Some(self.super_fold_ty(ty))
        }
    }
}

// fmt

impl fmt::Display for Ty<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind().fmt(f)
    }
}

impl fmt::Display for TyKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TyKind::Bool => write!(f, "bool"),
            TyKind::Int => write!(f, "int"),
            TyKind::Real => write!(f, "real"),
            TyKind::Ref => write!(f, "ref"),
            TyKind::Param(p) => write!(f, "{p}"),
            TyKind::Domain(kind, args) => write!(f, "{kind}{args}"),
            TyKind::Address(ty) => write!(f, "&{ty}"),
            TyKind::Heap => write!(f, "heap"),
        }
    }
}

impl fmt::Display for ParamTy<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}#{}", self.name, self.index)
    }
}

impl fmt::Display for DomainKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            DomainKind::Adt(adt) => write!(f, "{adt}"),
            DomainKind::Domain(did) =>
                write!(f, "{}", did.id),
        }
    }
}

impl fmt::Display for TyList<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_slice().bracketed().fmt(f)
    }
}
