use core::ops::{Deref, DerefMut};

use crate::{parse::{Adt, Block, Contract, Declaration, Function, HeapExp, IdnDecl, MemberId, Predicate, Program, Type}, translate::global::AnyId, vmir::{middle::{AdtId, VariantIdx}, ty::{AdtDefData, DomainDef, DomainKind, FieldDef, ParamTy, Register, RegisterAdtDef, Ty, TyCtxt, TyKind, TyList, VariantDef}, DefId, DefKind, Std, StdId, Symbol}, HashMap, TiVec};

use super::resolve::ResolveT;

pub struct TypeT<'a, 'tcx> {
    inner: ResolveT<'a, 'tcx>, 
    tys: TiVec<MemberId, DomainData<'tcx>>,
    to_intern: Vec<(MemberId, bool, RegisterAdtDef<'tcx>)>,
}

pub struct TypeCtxt<'a, 'tcx> {
    pub t: &'a TypeT<'a, 'tcx>,
    pub params: HashMap<Symbol<'tcx>, Ty<'tcx>>,
}

impl<'a, 'tcx> TypeCtxt<'a, 'tcx> {
    pub fn translate(&self, type_: &Type) -> Ty<'tcx> {
        match type_ {
            Type::Bool => self.t.types.bool_,
            Type::Int => self.t.types.int_,
            Type::Real => self.t.types.real_,
            Type::Ref => self.t.types.ref_,
            Type::Domain(ident, items) => {
                // TODO: handle builtin types
                let ident = self.t.interner.mk_symbol(ident);
                if let Some(ty) = self.params.get(&ident) {
                    assert_eq!(items.len(), 0, "domain param type cannot have arguments");
                    return *ty;
                }
                let (domain, params) = self.resolve_type(ident);
                assert_eq!(params.len(), items.len(), "domain/adt param count mismatch");
                let items = items.iter().map(|item| self.translate(item)).collect();
                let items = self.t.interner.mk_ty_list(items);
                self.t.interner.mk_ty_from_kind(TyKind::Domain(domain, items))
            }
        }
    }

    fn resolve_type(&self, ident: Symbol<'tcx>) -> (DomainKind, &[ParamTy<'tcx>]) {
        let id = self.t.resolve_get(ident).expect("ident not found");
        let id = match id {
            Ok(id) => id,
            Err(id) => return Std::get_type(id).unwrap(),
        };
        let DomainData::Domain(domain, params, ..) = &self.t.tys[id] else {
            panic!("domain not found");
        };
        (*domain, params)
    }
}

#[derive(Debug)]
pub enum DomainData<'tcx> {
    None,
    Domain(DomainKind, Box<[ParamTy<'tcx>]>),
    Predicate { res_snap: Ty<'tcx>, fold_post_snap: Option<Ty<'tcx>> },
    Function { pre_snap: Option<Ty<'tcx>> },
    Method { pre_snap: Option<Ty<'tcx>>, post_snap: Option<Ty<'tcx>> },
    Field { res: Ty<'tcx> },
}

// tcx: &mut TyCtxt<'tcx>, id: MemberId, decl: &Declaration, to_intern: &mut Vec<(MemberId, bool, RegisterAdtDef<'tcx>)>
struct CollectTypeT<'a, 'tcx> {
    tcx: &'a mut TyCtxt<'tcx>,
    to_intern_adt: &'a mut Vec<(MemberId, RegisterAdtDef<'tcx>)>,
    to_intern_exp: &'a mut Vec<(MemberId, bool, RegisterAdtDef<'tcx>)>,
}

impl<'a, 'tcx> TypeT<'a, 'tcx> {
    pub fn new(mut resolved: ResolveT<'a, 'tcx>, program: &Program) -> Self {
        let (mut to_intern_exp, mut to_intern_adt) = (Vec::new(), Vec::new());
        let mut ctt = CollectTypeT { tcx: &mut *resolved, to_intern_adt: &mut to_intern_adt, to_intern_exp: &mut to_intern_exp };
        let tys = program.iter().map(|(id, decl)| {
            ctt.register_decl(id, decl)
        }).collect();
        let mut self_ = Self { inner: resolved, tys, to_intern: to_intern_exp };
        // Native adts can be interned immediately, snapshot adts require
        // desugaring (and are done later in `post_desugar_intern`).
        for (id, reg) in to_intern_adt {
            let Declaration::Adt(adt) = &program[id] else {
                unreachable!()
            };
            let adt = self_.intern_adt(id, adt);
            self_.interner.intern_adt_def(reg, adt);
        }
        for (id, decl) in program.iter() {
            self_.register_field(id, decl);
        }
        self_
    }

    pub fn type_translator(&self, params: HashMap<Symbol<'tcx>, Ty<'tcx>>) -> TypeCtxt<'_, 'tcx> {
        TypeCtxt { t: self, params }
    }

    pub fn get_ty_params(&self, domain_or_adt: AnyId) -> Option<&Box<[ParamTy<'tcx>]>> {
        let id = match domain_or_adt {
            Ok(id) => id,
            Err(_) => todo!(),
        };
        let DomainData::Domain(_, params, ..) = &self.tys[id] else {
            return None;
        };
        Some(params)
    }

    pub fn get_params(&self, domain_or_adt: AnyId) -> Option<HashMap<Symbol<'tcx>, Ty<'tcx>>> {
        let params = self.get_ty_params(domain_or_adt)?;
        Some(params.iter().copied().map(|param| {
            let ty = self.interner.mk_ty_from_kind(TyKind::Param(param));
            (param.name, ty)
        }).collect())
    }

    pub fn get_domain_or_adt(&self, id: MemberId) -> Option<DomainKind> {
        let DomainData::Domain(domain, _, ..) = &self.tys[id] else {
            return None;
        };
        Some(*domain)
    }

    pub fn get_adt(&self, id: AnyId) -> Option<(AdtId, &'tcx AdtDefData<'tcx>)> {
        let id = match id {
            Ok(id) => id,
            Err(_) => todo!(),
        };
        let DomainData::Domain(DomainKind::Adt(adt), _) = &self.tys[id] else {
            return None;
        };
        Some((*adt, self.interner.get_adt_def(*adt).data()))
    }

    /// Get the `adt/domain` type of a predicate, or the type of a field. Does
    /// not include the address wrapper.
    pub fn predicate_field_ty(&self, pred: MemberId) -> Ty<'tcx> {
        match self.tys[pred] {
            DomainData::Field { res } => res,
            DomainData::Predicate { res_snap, .. } => res_snap,
            ref other => unreachable!("{other:?}"),
        }
    }

    pub fn predicate_fold_ty(&self, pred: MemberId) -> Option<Ty<'tcx>> {
        match self.tys[pred] {
            DomainData::Predicate { fold_post_snap, .. } => fold_post_snap,
            _ => unreachable!(),
        }
    }

    pub fn function_pre_ty(&self, fn_id: MemberId) -> Option<Ty<'tcx>> {
        match self.tys[fn_id] {
            DomainData::Function { pre_snap } => pre_snap,
            _ => unreachable!(),
        }
    }

    pub fn method_sig_ty(&self, fn_id: MemberId) -> (Option<Ty<'tcx>>, Option<Ty<'tcx>>) {
        match self.tys[fn_id] {
            DomainData::Method { pre_snap, post_snap } => (pre_snap, post_snap),
            ref other => unreachable!("{other:?}"),
        }
    }

    pub fn post_desugar_intern(&mut self, program: &Program) {
        let to_intern = core::mem::take(&mut self.to_intern);
        for (id, post, reg) in to_intern {
            let adt = self.intern_decl(id, post, &program[id]);
            self.interner.intern_adt_def(reg, adt);
        }
    }

    fn register_field(&mut self, id: MemberId, decl: &Declaration) {
        match decl {
            Declaration::Field(field) => {
                let tycx = self.type_translator(Default::default());
                let res = tycx.translate(&field.ty());
                assert!(matches!(self.tys[id], DomainData::None));
                self.tys[id] = DomainData::Field { res };
            }
            _ => (),
        }
    }

    fn intern_decl(&mut self, _id: MemberId, post: bool, decl: &Declaration) -> AdtDefData<'tcx> {
        let (symbol, variants) = match decl {
            Declaration::Predicate(Predicate { signature, body: Some(Block(res)) }) => {
                let symbol = self.interner.mk_custom_symbol(&signature.name.0, "body");
                (symbol, self.intern_res(res))
            }
            Declaration::Function(Function { signature, contract: Contract { precondition: res, .. }, .. }) => {
                let symbol = self.interner.mk_custom_symbol(&signature.name.0, "pre");
                (symbol, self.intern_res(res.as_ref().unwrap()))
            }
            Declaration::Method(m) => {
                let (suffix, res) = if post {
                    ("post", &m.contract.postcondition)
                } else {
                    ("pre", &m.contract.precondition)
                };
                (self.interner.mk_custom_symbol(&m.signature.name.0, suffix), self.intern_res(res.as_ref().unwrap()))
            }
            _ => unreachable!(),
        };
        // TODO: is `DefKind::Domain` correct here?
        let id = self.members.register(symbol, DefKind::Domain);
        AdtDefData { id: id.into(), params: Default::default(), variants }
    }

    fn intern_adt(&mut self, adt_id: MemberId, adt: &Adt) -> AdtDefData<'tcx> {
        let symbol = self.interner.mk_symbol(&adt.name.0);
        let id = self.members.register(symbol, DefKind::Adt);

        let params = self.get_params(Ok(adt_id)).unwrap();
        let ctxt = self.type_translator(params);
        let variants = adt.variants.iter().map(|variant| {
            let fields = variant.destructors().map(|field| {
                let name = self.interner.mk_symbol(&field.idn.0);
                FieldDef::new(Some(name), ctxt.translate(&field.ty))
            }).collect();
            let name = self.interner.mk_symbol(&variant.name.0);
            VariantDef { name: Some(name), fields }
        }).collect();
        AdtDefData { id: id.into(), params: self.get_ty_params(Ok(adt_id)).unwrap().clone(), variants }
    }

    fn intern_res(&self, res: &HeapExp) -> TiVec<VariantIdx, VariantDef<'tcx>> {
        let fields = res.res.iter().map(|res| {
            let ty = match res.loc() {
                Ok(loc) => {
                    let callee = self.resolve(loc).unwrap();
                    self.predicate_field_ty(callee.unwrap())
                }
                Err((_lhs, _rhs)) => {
                    // TODO:
                    self.types.heap_
                }
            };
            // TODO: add `Option` here if cond is not empty
            FieldDef::new(None, ty)
        }).collect();
        [VariantDef { name: None, fields }].into_iter().collect()
    }
}

impl<'a, 'tcx> CollectTypeT<'a, 'tcx> {
    fn register_decl(&mut self, id: MemberId, decl: &Declaration) -> DomainData<'tcx> {
        let mut register = |tcx: &TyCtxt<'tcx>, post| {
            let reg = tcx.interner.register_adt_def();
            let adt = reg.id();
            if let Some(post) = post {
                self.to_intern_exp.push((id, post, reg));
            } else {
                self.to_intern_adt.push((id, reg));
            }
            adt
        };
        let params = |params: &[IdnDecl]| params.iter().enumerate().map(|(i, param)| {
            let name = self.tcx.interner.mk_symbol(&param.0);
            ParamTy { name, index: i as u32 }
        }).collect();
        let mk_ty = |tcx: &TyCtxt<'tcx>, kind: DomainKind| {
            tcx.interner.mk_ty_from_kind(TyKind::Domain(kind, TyList::empty()))
        };
        match decl {
            Declaration::Domain(domain) => {
                let params = params(&domain.params);
                let symbol = self.tcx.interner.mk_symbol(&domain.name.0);
                let id = self.tcx.members.register(symbol, DefKind::Domain);
                DomainData::Domain(DomainKind::Domain(DomainDef { id: id.into() }), params)
            }
            Declaration::Adt(adt) => {
                DomainData::Domain(DomainKind::Adt(register(self.tcx, None)), params(&adt.params))
            }
            Declaration::Predicate(Predicate { signature, body }) => {
                let (res_snap, fold_post_snap) = body.as_ref().map(|_| {
                    let res_snap = mk_ty(self.tcx, DomainKind::Adt(register(self.tcx, Some(false))));
                    let name = self.tcx.interner.mk_custom_symbol(&signature.name.0, "fold");
                    (res_snap, Self::mk_simple_adt(self.tcx, name, res_snap))
                }).unzip();
                let res_snap = res_snap.unwrap_or_else(|| {
                    let symbol = self.tcx.interner.mk_symbol(&signature.name.0);
                    let id = self.tcx.members.register(symbol, DefKind::Domain);
                    mk_ty(self.tcx, DomainKind::Domain(DomainDef { id: id.into() }))
                });
                DomainData::Predicate { res_snap, fold_post_snap }
            }
            Declaration::Function(f) => {
                let has_pre = f.contract.precondition.is_some();
                DomainData::Function { pre_snap: has_pre.then(|| mk_ty(self.tcx, DomainKind::Adt(register(self.tcx, Some(false))))) }
            }
            Declaration::Method(m) => {
                let has_pre = m.contract.precondition.is_some();
                let pre_snap = has_pre.then(|| mk_ty(self.tcx, DomainKind::Adt(register(self.tcx, Some(false)))));
                let has_post = m.contract.postcondition.is_some();
                let post_snap = has_post.then(|| mk_ty(self.tcx, DomainKind::Adt(register(self.tcx, Some(true)))));
                DomainData::Method { pre_snap, post_snap }
            }
            _ => DomainData::None,
        }
    }

    fn mk_simple_adt(tcx: &mut TyCtxt<'tcx>, name: Symbol<'tcx>, field: Ty<'tcx>) -> Ty<'tcx> {
        let field = FieldDef::new(None, field);
        let adt = [VariantDef { name: None, fields: [field].into_iter().collect() }].into_iter().collect();
        let id = tcx.members.register(name, DefKind::Adt);
        let adt = AdtDefData { id: id.into(), params: Default::default(), variants: adt };
        let adt = tcx.interner.intern_adt_def(tcx.interner.register_adt_def(), adt);
        tcx.interner.mk_ty_from_kind(TyKind::Domain(DomainKind::Adt(adt), TyList::empty()))
    }
}

impl<'a, 'tcx> Deref for TypeT<'a, 'tcx> {
    type Target = ResolveT<'a, 'tcx>;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'tcx> DerefMut for TypeT<'_, 'tcx> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}
