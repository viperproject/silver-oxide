use crate::parse::{Declaration, Program, Type};

use super::{ast::{idx::LocalDefId, member::{Member, Members}, FnSig, Globals}, *};

pub struct TyCtxt<'tcx> {
    pub interner: Interner<'tcx>,
    pub types: Types<'tcx>,
    pub std: Std<'tcx>,
    pub(super) globals: Globals<'tcx>,
    pub(super) members: Members<'tcx>,
}

impl<'tcx> TyCtxt<'tcx> {
    pub fn kind<Id: Into<DefId>>(&self, id: Id) -> MemberKind {
        let id = <Id as Into<DefId>>::into(id);
        match id.as_local() {
            Some(id) => self.globals.data[id].kind,
            None => self.std.kind(id),
        }
    }

    pub fn is_field<Id: Into<DefId>>(&self, id: Id) -> bool {
        matches!(self.kind(id), MemberKind::Field)
    }

    pub fn is_predicate<Id: Into<DefId>>(&self, id: Id) -> bool {
        matches!(self.kind(id), MemberKind::Predicate)
    }

    pub fn is_function<Id: Into<DefId>>(&self, id: Id) -> bool {
        matches!(self.kind(id), MemberKind::Function)
    }

    pub fn is_method<Id: Into<DefId>>(&self, id: Id) -> bool {
        matches!(self.kind(id), MemberKind::Method)
    }

    pub fn fn_sig<Id: Into<DefId>>(&self, id: Id) -> Option<&FnSig<'tcx>> {
        let id = <Id as Into<DefId>>::into(id);
        match id.as_local() {
            Some(id) => self.globals.data[id].fn_sig.as_ref(),
            None => self.std.fn_sig(id),
        }
    }

    pub fn member(&self, id: LocalDefId) -> &Member<'tcx> {
        &self.members.bodies[id]
    }

    pub fn global_ref(&self, symbol: Symbol<'tcx>) -> Option<DefId> {
        self.std.global_ref(symbol).or_else(|| {
            self.globals.resolved.get(&symbol).map(|id| id.into())
        })
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
}

impl<'tcx> Default for TyCtxt<'tcx> {
    fn default() -> Self {
        let interner = Interner::default();
        let types = Types::new(&interner);
        let std = Std::new(&interner);
        Self { interner, types, std, globals: Default::default(), members: Default::default() }
    }
}
