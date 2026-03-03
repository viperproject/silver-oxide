use ::std::borrow::Borrow;

use crate::{
    parse::{ConstHeapKind, ConstKind, Ident},
    vmir::{middle::AdtId, Symbol}, TiVec,
};

use super::*;

pub type RegisterAdtDef<'tcx> = Register<AdtId, AdtDefData<'tcx>>;

pub struct Interner<'tcx>(&'tcx InternerInner);

impl<'tcx> Interner<'tcx> {
    pub const fn mk_ty_const(kind: TyKind) -> Option<Ty<'static>> {
        static BOOL: TyKind<'static> = TyKind::Bool;
        static INT: TyKind<'static> = TyKind::Int;
        static REAL: TyKind<'static> = TyKind::Real;
        static REF: TyKind<'static> = TyKind::Ref;
        static HEAP: TyKind<'static> = TyKind::Heap;
        let ty = match kind {
            TyKind::Bool => &BOOL,
            TyKind::Int => &INT,
            TyKind::Real => &REAL,
            TyKind::Ref => &REF,
            TyKind::Heap => &HEAP,
            _ => return None,
        };
        Some(Ty::new(Interned(ty)))
    }

    pub fn mk_ty_from_kind(&self, kind: TyKind<'tcx>) -> Ty<'tcx> {
        if let Some(ty) = Self::mk_ty_const(kind) {
            return ty;
        }
        // Safety: we know that the `'tcx` lifetime will live as long as `Interner` does.
        let kind = unsafe { core::mem::transmute::<TyKind<'tcx>, TyKind<'static>>(kind) };
        Ty::new(Interned(self.0.ty.intern(kind)))
    }

    pub fn mk_symbol(&self, ident: &Ident) -> Symbol<'tcx> {
        Symbol(Interned(self.0.symbol.intern_ref(ident).0.as_str()))
    }

    pub(crate) fn mk_custom_symbol(&self, ident: &Ident, suffix: &str) -> Symbol<'tcx> {
        let val = Ident(format!("{}@{suffix}", ident.0));
        Symbol(Interned(self.0.symbol.intern(val).0.as_str()))
    }

    pub fn mk_ty_list(&self, tys: Vec<Ty<'tcx>>) -> TyList<'tcx> {
        // Safety: we know that the `'tcx` lifetime will live as long as `Interner` does.
        let tys = unsafe { core::mem::transmute::<Vec<Ty<'tcx>>, Vec<Ty<'static>>>(tys) };
        TyList(Interned(self.0.ty_list.intern(tys)))
    }

    #[must_use]
    pub fn register_adt_def(&self) -> RegisterAdtDef<'tcx> {
        let reg = self.0.adt_def.register();
        unsafe { core::mem::transmute::<Register<AdtId, AdtDefData<'static>>, RegisterAdtDef<'tcx>>(reg) }
    }

    pub fn intern_adt_def(&self, reg: RegisterAdtDef<'tcx>, value: AdtDefData<'tcx>) -> AdtId {
        let id = reg.id();
        let reg = unsafe { core::mem::transmute::<RegisterAdtDef<'tcx>, Register<AdtId, AdtDefData<'static>>>(reg) };
        let value = unsafe { core::mem::transmute::<AdtDefData<'tcx>, AdtDefData<'static>>(value) };
        self.0.adt_def.intern(reg, value);
        id
    }

    pub fn get_adt_def(&self, id: AdtId) -> AdtDef<'tcx> {
        AdtDef(Interned(&self.0.adt_def[id]))
    }

    pub fn mk_const_ref(&self, const_: &ConstKind) -> Const<'tcx> {
        Self::mk_const_inner(const_, |const_| self.0.const_.intern_ref(const_))
    }

    pub fn mk_const(&self, const_: ConstKind) -> Const<'tcx> {
        Self::mk_const_inner(const_, |const_| self.0.const_.intern(const_))
    }

    fn mk_const_inner<T: Borrow<ConstKind>>(
        const_: T,
        intern: impl FnOnce(T) -> &'tcx ConstKind,
    ) -> Const<'tcx> {
        use ConstKind::*;
        let const_ = match const_.borrow() {
            Bool(true) => &Bool(true),
            Bool(false) => &Bool(false),
            Null => &Null,
            Epsilon => &Epsilon,
            Wildcard => &Wildcard,
            Heap(ConstHeapKind::Old) => &Heap(ConstHeapKind::Old),
            Heap(ConstHeapKind::SelfFraming) => &Heap(ConstHeapKind::SelfFraming),
            Int(..) | Real(..) => intern(const_),
        };
        Const(Interned(const_))
    }
}

#[derive(Debug)]
pub struct Interned<'tcx, T: ?Sized>(pub(crate) &'tcx T);

impl<T: ?Sized> Interned<'_, T> {
    pub fn ptr(self) -> *const () {
        (self.0 as *const T).cast()
    }
}

impl<T: ?Sized> Clone for Interned<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T: ?Sized> Copy for Interned<'_, T> {}

impl<T: ?Sized> PartialEq for Interned<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr() == other.ptr()
    }
}
impl<T: ?Sized> Eq for Interned<'_, T> {}
impl<T: ?Sized> core::hash::Hash for Interned<'_, T> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.ptr().hash(state)
    }
}

impl<T: ?Sized> PartialOrd for Interned<'_, T> {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: ?Sized> Ord for Interned<'_, T> {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.ptr().cmp(&other.ptr())
    }
}

impl Default for Interner<'_> {
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
struct InternerInner {
    ty: internment::Bump<TyKind<'static>>,
    symbol: internment::Bump<Ident>,
    ty_list: internment::Bump<Vec<Ty<'static>>>,
    const_: internment::Bump<ConstKind>,
    adt_def: InternVec<AdtId, AdtDefData<'static>>,
}

pub struct InternVec<K, V> {
    data: internment::Bump<V>,
    p: core::cell::Cell<TiVec<K, Option<core::ptr::NonNull<V>>>>,
}

pub struct Register<K: core::fmt::Debug, V> {
    k: K,
    p: core::marker::PhantomData<fn(V) -> V>,
}

impl<K: Copy + From<usize> + core::fmt::Debug, V: Eq + core::hash::Hash> InternVec<K, V> where usize: From<K> {
    #[must_use]
    fn register(&self) -> Register<K, V> {
        let mut p = self.p.take();
        let k = p.push_and_get_key(None);
        self.p.set(p);
        Register { k, p: core::marker::PhantomData }
    }

    fn intern(&self, r: Register<K, V>, value: V) {
        let value = self.data.intern(value);
        let ptr = core::ptr::NonNull::from(value);
        let mut p = self.p.take();
        p[r.k] = Some(ptr);
        self.p.set(p);
        core::mem::forget(r);
    }
}

impl<K: Copy + core::fmt::Debug, V> Register<K, V> {
    pub fn id(&self) -> K {
        self.k
    }
}

impl<K, V> core::ops::Index<K> for InternVec<K, V> where usize: From<K> {
    type Output = V;
    fn index(&self, index: K) -> &Self::Output {
        let p = self.p.take();
        let ptr = p[index];
        self.p.set(p);
        unsafe { ptr.unwrap().as_ref() }
    }
}

impl<K, V> Default for InternVec<K, V> {
    fn default() -> Self {
        Self {
            data: Default::default(),
            p: Default::default(),
        }
    }
}

impl<K: core::fmt::Debug, V> Drop for Register<K, V> {
    fn drop(&mut self) {
        panic!("Register dropped without being interned: {:?}", self.k);
    }
}
