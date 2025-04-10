use crate::parse::{ConstKind, Ident};

use super::*;

pub struct Interner<'tcx>(&'tcx InternerInner<'tcx>);

impl<'tcx> Interner<'tcx> {
    pub fn mk_ty_from_kind(&self, kind: TyKind<'tcx>) -> Ty<'tcx> {
        let ty = match kind {
            TyKind::Bool => &TyKind::Bool,
            TyKind::Int => &TyKind::Int,
            TyKind::Real => &TyKind::Real,
            TyKind::Ref => &TyKind::Ref,
            TyKind::Heap => &TyKind::Heap,
            _ => self.0.ty.intern(kind),
        };
        Ty(Interned(ty))
    }

    pub fn mk_symbol(&self, ident: &Ident) -> Symbol<'tcx> {
        Symbol(Interned(self.0.symbol.intern_ref(ident).0.as_str()))
    }

    pub fn mk_ty_list(&self, tys: Vec<Ty<'tcx>>) -> TyList<'tcx> {
        TyList(Interned(self.0.ty_list.intern(tys)))
    }

    pub fn mk_const(&self, const_: &ConstKind) -> Const<'tcx> {
        let const_ = match const_ {
            ConstKind::Bool(true) => &ConstKind::Bool(true),
            ConstKind::Bool(false) => &ConstKind::Bool(false),
            ConstKind::Null => &ConstKind::Null,
            ConstKind::None => &ConstKind::None,
            ConstKind::Write => &ConstKind::Write,
            ConstKind::Epsilon => &ConstKind::Epsilon,
            ConstKind::Wildcard => &ConstKind::Wildcard,
            ConstKind::Int(..) => self.0.const_.intern_ref(const_),
        };
        Const(Interned(const_))
    }
}

#[derive(Debug)]
pub struct Interned<'tcx, T: ?Sized>(pub(crate) &'tcx T);

impl<T: ?Sized> Clone for Interned<'_, T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}
impl<T: ?Sized> Copy for Interned<'_, T> {}

impl<T: ?Sized> PartialEq for Interned<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        core::ptr::eq(self.0, other.0)
    }
}
impl<T: ?Sized> Eq for Interned<'_, T> {}
impl<T: ?Sized> core::hash::Hash for Interned<'_, T> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        core::ptr::hash(self.0, state)
    }
}

impl<'tcx> Default for Interner<'tcx> {
    fn default() -> Self {
        Self(Box::leak(Default::default()))
    }
}

impl Drop for Interner<'_> {
    fn drop(&mut self) {
        let raw = self.0 as *const _ as *mut InternerInner;
        let interner = unsafe { Box::from_raw(raw) };
        drop(interner);
    }
}

#[derive(Default)]
struct InternerInner<'tcx> {
    ty: internment::Bump<TyKind<'tcx>>,
    symbol: internment::Bump<Ident>,
    ty_list: internment::Bump<Vec<Ty<'tcx>>>,
    const_: internment::Bump<ConstKind>,
}
