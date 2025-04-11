use ::std::{mem::ManuallyDrop, ops::{Deref, DerefMut}, sync::Mutex};

use crate::parse::{Ident, Type};

use super::{member::{Member, Members}, *};

pub struct TyCtxt<'tcx>(Box<GlobalCtxt<'tcx>>);

impl<'tcx> Deref for TyCtxt<'tcx> {
    type Target = GlobalCtxt<'tcx>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<'tcx> DerefMut for TyCtxt<'tcx> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub struct GlobalCtxt<'tcx> {
    pub interner: Interner<'tcx>,
    pub types: Types<'tcx>,
    pub std: Std<'tcx>,
    pub(super) globals: Globals<'tcx>,
    pub(super) members: Members<'tcx>,
}

impl<'tcx> TyCtxt<'tcx> {
    pub fn data<Id: Into<DefId>>(&self, id: Id) -> MemberData<'tcx> {
        let id = <Id as Into<DefId>>::into(id);
        match id.as_local() {
            Some(id) => self.globals.data[id],
            None => self.std.data(id),
        }
    }

    pub fn is_field<Id: Into<DefId>>(&self, id: Id) -> bool {
        matches!(self.data(id).kind, MemberKind::Field)
    }

    pub fn is_predicate<Id: Into<DefId>>(&self, id: Id) -> bool {
        matches!(self.data(id).kind, MemberKind::Predicate)
    }

    pub fn is_function<Id: Into<DefId>>(&self, id: Id) -> bool {
        matches!(self.data(id).kind, MemberKind::Function)
    }

    pub fn is_method<Id: Into<DefId>>(&self, id: Id) -> bool {
        matches!(self.data(id).kind, MemberKind::Method)
    }

    pub fn is_heap_dependent<Id: Into<DefId>>(&self, id: Id) -> bool {
        self.fn_sig(id).unwrap().heap_dependent
    }

    pub fn fn_sig<Id: Into<DefId>>(&self, id: Id) -> Option<&FnSig<'tcx>> {
        let id = <Id as Into<DefId>>::into(id);
        match id.as_local() {
            Some(id) => self.globals.sigs[id].as_ref(),
            None => self.std.fn_sig(id),
        }
    }

    pub fn item_name(&self, id: DefId) -> Option<Symbol<'tcx>> {
        self.data(id).sig.map(|s| s.name)
    }

    pub fn member(&self, id: LocalDefId) -> &Member<'tcx> {
        &self.members.bodies[id]
    }

    pub fn global_ref(&self, symbol: Symbol<'tcx>) -> Option<DefId> {
        self.std.global_ref(symbol).or_else(|| {
            self.globals.resolved.get(&symbol).map(|id| id.into())
        })
    }

    pub(super) fn calculate_kinds(&mut self, program: &Program) {
        self.0.globals.calculate_kinds(&self.0.interner, &program);
    }

    pub(crate) fn get_callee(&self, ident: &Ident) -> DefId {
        let ident = self.interner.mk_symbol(ident);
        self.global_ref(ident).unwrap()
    }

    pub(crate) fn const_ty(&self, const_: Const<'tcx>) -> Ty<'tcx> {
        use crate::parse::ConstKind::*;
        match const_.kind() {
            Bool(..) => self.types.bool_,
            Int(..) => self.types.int_,
            Null => self.types.ref_,
            None => self.types.real_,
            Write => self.types.real_,
            Epsilon => self.types.real_,
            Wildcard => self.types.real_,
            SelfFramingHeap => self.types.heap_,
        }
    }

    pub(crate) fn translate_type(&self, type_: &Type) -> Ty<'tcx> {
        match type_ {
            Type::Bool => self.types.bool_,
            Type::Int => self.types.int_,
            Type::Real => self.types.real_,
            Type::Ref => self.types.ref_,
            Type::Domain(ident, items) => {
                let ident = self.interner.mk_symbol(ident);
                let items = items.iter().map(|item| self.translate_type(item)).collect();
                let items = self.interner.mk_ty_list(items);
                self.interner.mk_ty_from_kind(TyKind::Domain(ident, items))
            }
        }
    }

    /// Only for use in development features (e.g. printing DefId)
    pub(crate) unsafe fn global_ref_unchecked() -> impl Deref<Target = TyCtxtCopy<'static>> {
        TCX.lock().unwrap()
    }
}

pub(crate) struct TyCtxtCopy<'tcx>(Option<ManuallyDrop<TyCtxt<'tcx>>>);
unsafe impl Send for TyCtxtCopy<'static> {}

impl<'tcx> Deref for TyCtxtCopy<'tcx> {
    type Target = TyCtxt<'tcx>;
    fn deref(&self) -> &Self::Target {
        &self.0.as_ref().unwrap()
    }
}

static TCX: Mutex<TyCtxtCopy<'static>> = Mutex::new(TyCtxtCopy(None));

impl<'tcx> Default for TyCtxt<'tcx> {
    fn default() -> Self {
        let interner = Interner::default();
        let types = Types::new(&interner);
        let std = Std::new(&interner);
        let gcx = GlobalCtxt { interner, types, std, globals: Default::default(), members: Default::default() };
        let tcx = TyCtxt(Box::new(gcx));

        let tcx_copy = unsafe {
            core::mem::transmute::<&TyCtxt<'tcx>, &TyCtxt<'static>>(&tcx)
        };
        let tcx_copy = unsafe {
            ManuallyDrop::new(core::ptr::read(tcx_copy))
        };
        TCX.lock().unwrap().0.replace(tcx_copy);

        tcx
    }
}
