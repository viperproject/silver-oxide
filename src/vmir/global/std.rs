use crate::{translate::global::MemberKind, vmir::{middle::FunctionId, ty::{DomainDef, DomainKind, Interner, ParamTy, Ty, TyKind}, Binder, DefId, Symbol}};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct StdId<'tcx>(Symbol<'tcx>);

static SEQ: StdId<'static> = StdId(Symbol::new_static("Seq"));
static SET: StdId<'static> = StdId(Symbol::new_static("Set"));
static MULTISET: StdId<'static> = StdId(Symbol::new_static("Multiset"));
static MAP: StdId<'static> = StdId(Symbol::new_static("Map"));

static SEQ_ID: FunctionId = FunctionId::mk(u32::MAX as usize - 1);
static SET_ID: FunctionId = FunctionId::mk(u32::MAX as usize - 2);
static MULTISET_ID: FunctionId = FunctionId::mk(u32::MAX as usize - 3);
static MAP_ID: FunctionId = FunctionId::mk(u32::MAX as usize - 4);

static SEQ_TYPE: DomainDef = DomainDef {
    id: DefId::builtin(0),
};
static SET_TYPE: DomainDef = DomainDef {
    id: DefId::builtin(1),
};
static MULTISET_TYPE: DomainDef = DomainDef {
    id: DefId::builtin(2),
};
static MAP_TYPE: DomainDef = DomainDef {
    id: DefId::builtin(3),
};

pub struct Std;

impl Std {
    pub fn resolve<'tcx>(ident: Symbol<'tcx>) -> Option<StdId<'tcx>> {
        match ident.as_str() {
            "Seq" => Some(SEQ),
            "Set" => Some(SET),
            "Multiset" => Some(MULTISET),
            "Map" => Some(MAP),
            _ => None,
        }
    }

    pub fn get_member_kind(id: StdId) -> Option<MemberKind> {
        match id.0.as_str() {
            "Seq" | "Set" | "Multiset" | "Map" => Some(MemberKind::Function),
            _ => None,
        }
    }

    pub fn get_function<'tcx>(id: StdId<'tcx>) -> Option<(FunctionId, bool, Option<Binder<&'tcx [Ty<'tcx>]>>, Binder<Ty<'tcx>>)> {
        let heap = Binder::new(Interner::mk_ty_const(TyKind::Heap).unwrap(), 0);
        match id.0.as_str() {
            "Seq" => Some((SEQ_ID, true, None, heap)),
            "Set" => Some((SET_ID, true, None, heap)),
            "Multiset" => Some((MULTISET_ID, true, None, heap)),
            "Map" => Some((MAP_ID, true, None, heap)),
            _ => None,
        }
    }

    pub fn get_type<'tcx>(id: StdId<'tcx>)  -> Option<(DomainKind, &'tcx [ParamTy<'tcx>])> {
        static T: &'static [ParamTy<'static>] = &[ParamTy { name: Symbol::new_static("T"), index: 0 }];
        static M: &'static [ParamTy<'static>] = &[ParamTy { name: Symbol::new_static("K"), index: 0 }, ParamTy { name: Symbol::new_static("V"), index: 1 }];
        let (ty, params) = match id.0.as_str() {
            "Seq" => (SEQ_TYPE, T),
            "Set" => (SET_TYPE, T),
            "Multiset" => (MULTISET_TYPE, T),
            "Map" => (MAP_TYPE, M),
            _ => return None,
        };
        Some((DomainKind::Domain(ty), params))
    }
}
