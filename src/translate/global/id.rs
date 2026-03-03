use core::ops::{Deref, DerefMut};

use crate::{parse::{self, Declaration, DomainElementKind, HeapExp, Ident, IdnDecl, MemberId, Program}, translate::global::AnyId, vmir::{self, members::MemberRef, middle::{AdtId, Exp, ExpLine, ExpLineKind, ExpOperand, ExpOperandKind, FunctionId, Local, Maybe, MethodId, Operand, OperandKind, Resource, ResourceExp, ResourceId, VariantIdx}, ty::{DomainKind, Interner, Ty, TyKind}, Binder, DefKind, FunctionCall, Locals, ResourceCall, Std}, TiVec};

use super::TypeT;

pub struct IdT<'a, 'tcx> {
    pub inner: TypeT<'a, 'tcx>,
    pub ids: TiVec<MemberId, Option<IdData>>,
}

impl<'a, 'tcx> IdT<'a, 'tcx> {
    pub fn new(mut ty: TypeT<'a, 'tcx>, program: &Program) -> Self {
        let ids = program.iter().map(|(id, decl)| {
            Self::mk_id_data(&mut ty, id, decl)
        }).collect();
        Self { inner: ty, ids }
    }

    pub fn get_id_data(&self, id: MemberId) -> Option<IdData> {
        self.ids[id]
    }

    pub fn get_member_kind(&self, id: AnyId) -> Option<MemberKind> {
        let id = match id {
            Ok(id) => id,
            Err(id) => return Std::get_member_kind(id),
        };
        let kind = match self.ids[id].as_ref()? {
            IdData::Field { .. } => MemberKind::Field,
            IdData::Method { .. } => MemberKind::Method,
            IdData::Function { .. } => MemberKind::Function,
            IdData::Predicate { .. } => MemberKind::Predicate,
            IdData::AdtConstructor { .. } => MemberKind::AdtConstructor,
        };
        Some(kind)
    }

    pub fn get_method(&self, id: MemberId) -> Option<(MethodId, &[Ty<'tcx>], &[Ty<'tcx>])> {
        let Some(IdData::Method { method, .. }) = self.ids[id] else {
            return None;
        };
        let m = self.members.methods[method].sig();
        Some((method, &m.params().raw, &m.rets()))
    }

    pub fn get_address(&self, id: MemberId) -> Option<(FunctionId, bool, Option<Binder<&[Ty<'tcx>]>>, Binder<Ty<'tcx>>)> {
        let address = match self.ids[id] {
            Some(IdData::Field { address }) => address,
            Some(IdData::Predicate { address, .. }) => address,
            _ => return None
        };
        let f = self.members.functions[address].sig();
        assert!(f.heapless());
        Some((address, f.heapless(), Some(f.params()), f.ret()))
    }

    pub fn get_function(&self, id: AnyId<'tcx>) -> Option<(FunctionId, bool, Option<Binder<&[Ty<'tcx>]>>, Binder<Ty<'tcx>>)> {
        let id = match id {
            Ok(id) => id,
            Err(id) => return Std::get_function(id),
        };
        let Some(IdData::Function { function, .. }) = self.ids[id] else {
            return None;
        };
        let f = &self.members.functions[function].sig();
        Some((function, f.heapless(), Some(f.params()), f.ret()))
    }

    pub fn get_any_function(&self, id: AnyId<'tcx>) -> Option<(FunctionId, bool, Option<Binder<&[Ty<'tcx>]>>, Binder<Ty<'tcx>>)> {
        self.get_function(id).or_else(|| self.get_address(id.unwrap()))
    }

    pub fn get_constructor(&self, id: MemberId) -> Option<(AdtId, VariantIdx)> {
        let Some(IdData::AdtConstructor { adt, vid }) = self.ids[id] else {
            return None;
        };
        Some((adt, vid))
    }

    pub fn get_fold_unfold(&self, id: MemberId) -> (MethodId, &[Ty<'tcx>]) {
        let Some(IdData::Predicate { resource_and_fold: Some((_, fold)), .. }) = self.ids[id] else {
            panic!("not a predicate with body")
        };
        let m = &self.members.methods[fold].sig();
        (fold, &m.params().raw)
    }

    pub fn get_all_fields(&self) -> impl Iterator<Item = FunctionId> + '_ {
        self.ids.iter().filter_map(|data| {
            if let Some(IdData::Field { address }) = data {
                Some(*address)
            } else {
                None
            }
        })
    }

    fn mk_id_data(ty: &mut TypeT<'a, 'tcx>, id: MemberId, decl: &Declaration) -> Option<IdData> {
        use Declaration::*;
        let data = match decl {
            Import(..) | Define(..) | Domain(..) | Adt(..) => return None,
            AdtConstructor(cons) => {
                let name = ty.interner.mk_symbol(&cons.signature.name.0);

                let adt_mem = ty.resolve(cons.adt()).unwrap();
                let (adt, data) = ty.get_adt(adt_mem).unwrap();
                let (vid, _) = data.variants.iter_enumerated().find(|(_, v)| v.name.unwrap() == name).unwrap();

                IdData::AdtConstructor { adt, vid }
            }
            DomainElement(parse::DomainElement { kind: DomainElementKind::Axiom(..), .. }) => return None,
            DomainElement(parse::DomainElement { domain, kind: DomainElementKind::Function(f) }) => {
                let domain = ty.resolve(domain).unwrap();
                let tycx = ty.type_translator(ty.get_params(domain).unwrap());
                let args = f.signature.args.iter().map(|arg| tycx.translate(arg.ty()));
                let ret = tycx.translate(f.signature.ret[0].ty());
                let name = ty.interner.mk_symbol(&f.signature.name.0);
                let locals = [ret].into_iter().chain(args).collect();
                let f = vmir::Function {
                    nested_in: None,
                    ty_params: ty.get_ty_params(domain).unwrap().clone(),
                    locals: Locals::new(ty.interner.mk_ty_list(locals)),
                    pre: None,
                    post: None,
                    body: None,
                    id: ty.members.register(name, DefKind::Function),
                };
                let function = ty.members.functions.push_and_get_key(f);
                IdData::Function { function }
            }
            Field(f) => {
                const ARG: Ty<'static> = Interner::mk_ty_const(TyKind::Ref).unwrap();
                let result = ty.predicate_field_ty(id);
                let address = ty.interner.mk_ty_from_kind(TyKind::Address(result));
                let name = ty.interner.mk_symbol(&f.0.name.0);
                let locals = [address, ARG].into_iter().collect();
                let f = vmir::Function {
                    id: ty.members.register(name, DefKind::Function),
                    nested_in: None,
                    ty_params: Default::default(),
                    locals: Locals::new(ty.interner.mk_ty_list(locals)),
                    pre: None,
                    post: None,
                    body: None,
                };
                let address = ty.members.functions.push_and_get_key(f);
                IdData::Field { address }
            }
            Function(f) => {
                const RET: Ty<'static> = Interner::mk_ty_const(TyKind::Bool).unwrap();
                let tycx = ty.type_translator(Default::default());
                let args = f.signature.args.iter().map(|arg| tycx.translate(arg.ty()));
                let heap = ty.function_pre_ty(id);
                let ret = tycx.translate(f.signature.ret[0].ty());
                let locals: TiVec<Local, _> = [ret].into_iter().chain(args).chain(heap).collect();
                let args = || locals[Local::from(1)..=Local::from(f.signature.args.len())].iter_enumerated();

                let pre = heap.map(|snap| {
                    Self::mk_precondition(&f.signature.name, ty, snap, args())
                });
                let has_post = f.contract.postcondition.is_some();
                let post = has_post.then(|| {
                    let name = ty.interner.mk_custom_symbol(&f.signature.name.0, "post");
                    let p_locals = [RET].into_iter().chain(args().map(|(_, ty)| *ty)).chain(heap).chain([ret]).collect();
                    let f = vmir::Function {
                        id: ty.members.register(name, DefKind::Function),
                        nested_in: None,
                        ty_params: Default::default(),
                        locals: Locals::new(ty.interner.mk_ty_list(p_locals)),
                        pre: pre.clone(),
                        post: None,
                        body: None,
                    };
                    let fun = ty.members.functions.push_and_get_key(f).into();
                    FunctionCall {
                        fun,
                        args: locals.iter_enumerated().skip(1).map(|(l, _)| l).chain([Local::ZERO]).collect(),
                    }
                });
                let name = ty.interner.mk_symbol(&f.signature.name.0);
                let f = vmir::Function {
                    id: ty.members.register(name, DefKind::Function),
                    nested_in: None,
                    ty_params: Default::default(),
                    locals: Locals::new(ty.interner.mk_ty_list(locals.raw)),
                    pre,
                    post,
                    body: None,
                };
                let function = ty.members.functions.push_and_get_key(f);
                IdData::Function {
                    function,
                }
            }
            Predicate(p) => {
                let tycx = ty.type_translator(Default::default());
                let args = p.signature.args.iter().map(|arg| tycx.translate(arg.ty()));
                let result = ty.predicate_field_ty(id);
                let address_ty = ty.interner.mk_ty_from_kind(TyKind::Address(result));
                let locals: TiVec<Local, _> = [address_ty].into_iter().chain(args).collect();

                let name = ty.interner.mk_symbol(&p.signature.name.0);
                let f = vmir::Function {
                    id: ty.members.register(name, DefKind::Function),
                    nested_in: None,
                    ty_params: Default::default(),
                    locals: Locals::new(ty.interner.mk_ty_list(locals.raw.clone())),
                    pre: None,
                    post: None,
                    body: None,
                };
                let address = ty.members.functions.push_and_get_key(f);

                let args = || locals.iter_enumerated().skip(1);
                let fold_post_snap = ty.predicate_fold_ty(id);
                let resource_and_fold = fold_post_snap.map(|snap| {
                    let name = ty.interner.mk_custom_symbol(&p.signature.name.0, "body");
                    let b_locals = args().map(|(_, ty)| *ty).collect();
                    let r = vmir::Resource {
                        id: ty.members.register(name, DefKind::Resource),
                        nested_in: None,
                        snap: result,
                        locals: Locals::new(ty.interner.mk_ty_list(b_locals)),
                        pre: None,
                        post: None,
                        body: None,
                    };
                    let res = ty.members.resources.push_and_get_key(r);

                    let body = Self::simple_resource_body(ty, address_ty, address, locals[Local::from(1)..].iter_enumerated());
                    let name = ty.interner.mk_custom_symbol(&p.signature.name.0, "fold_post");
                    let p_locals = args().map(|(_, ty)| *ty).collect();
                    let r = vmir::Resource {
                        id: ty.members.register(name, DefKind::Resource),
                        nested_in: None,
                        snap,
                        locals: Locals::new(ty.interner.mk_ty_list(p_locals)),
                        pre: None,
                        post: None,
                        body: Some(body),
                    };
                    let folded = ty.members.resources.push_and_get_key(r).into();
                    let name = ty.interner.mk_custom_symbol(&p.signature.name.0, "fold");
                    let f_locals = args().map(|(_, ty)| *ty).collect();
                    let m = vmir::Method {
                        id: ty.members.register(name, DefKind::Method),
                        nested_in: None,
                        ghost: true,
                        params: p.signature.args.len() as u32,
                        locals: Locals::new(ty.interner.mk_ty_list(f_locals)),
                        pre: Some(ResourceCall {
                            res: res,
                            args: args().map(|(l, _)| l).collect(),
                        }),
                        post: Some(ResourceCall { res: folded, args: args().map(|(l, _)| l).collect() }),
                        body: None,
                    };
                    (res, ty.members.methods.push_and_get_key(m))
                });

                IdData::Predicate { address, resource_and_fold }
            }
            Method(m) => {
                let tycx = ty.type_translator(Default::default());
                let args = m.signature.args.iter().map(|arg| tycx.translate(arg.ty()));
                let rets = m.signature.ret.iter().map(|arg| tycx.translate(arg.ty()));
                let (pre_snap, post_snap) = ty.method_sig_ty(id);
                let locals: TiVec<Local, _> = args.chain(rets).collect();

                let params =  m.signature.args.len();
                let pre = pre_snap.map(|snap| {
                    Self::mk_precondition(&m.signature.name, ty, snap, locals.iter_enumerated().take(params))
                });
                let post = post_snap.map(|snap| {
                    let name = ty.interner.mk_custom_symbol(&m.signature.name.0, "post");
                    let r = vmir::Resource {
                        id: ty.members.register(name, DefKind::Resource),
                        nested_in: None,
                        snap,
                        locals: Locals::new(ty.interner.mk_ty_list(locals.raw.clone())),
                        pre: pre.clone(),
                        post: None,
                        body: None,
                    };
                    let res = ty.members.resources.push_and_get_key(r).into();
                    ResourceCall {
                        res,
                        args: locals.iter_enumerated().map(|(l, _)| l).collect(),
                    }
                });
                let name = ty.interner.mk_symbol(&m.signature.name.0);
                let m = vmir::Method {
                    id: ty.members.register(name, DefKind::Method),
                    nested_in: None,
                    ghost: false,
                    params: params as u32,
                    locals: Locals::new(ty.interner.mk_ty_list(locals.raw)),
                    pre,
                    post,
                    body: None,
                };
                let method = ty.members.methods.push_and_get_key(m);
                IdData::Method {
                    method,
                }
            }
        };
        Some(data)
    }

    fn mk_precondition<'b>(name: &IdnDecl, ty: &mut TypeT<'a, 'tcx>, snap: Ty<'tcx>, locals: impl Iterator<Item = (Local, &'b Ty<'tcx>)>) -> ResourceCall where 'tcx: 'b {
        let (args, locals) = locals.map(|(l, ty)| (l, *ty)).unzip();
        let name = ty.interner.mk_custom_symbol(&name.0, "pre");
        let r = vmir::Resource {
            id: ty.members.register(name, DefKind::Resource),
            nested_in: None,
            snap,
            locals: Locals::new(ty.interner.mk_ty_list(locals)),
            pre: None,
            post: None,
            body: None,
        };
        let res = ty.members.resources.push_and_get_key(r).into();
        ResourceCall {
            res,
            args,
        }
    }

    fn simple_resource_body<'b>(ty: &mut TypeT<'a, 'tcx>, address_ty: Ty<'tcx>, address: FunctionId, args: impl Iterator<Item = (Local, &'b Ty<'tcx>)>) -> ResourceExp<'tcx> where 'tcx: 'b {
        let mut pure = Exp::new_use(ty.const_operand(parse::ConstKind::bool(true)));
        let after_line = pure.lines.push_and_get_key(ExpLine {
            cond: Default::default(),
            ty: address_ty,
            kind: ExpLineKind::Call(address, args.map(|(l, ty)| {
                Operand {
                    ty: *ty,
                    kind: OperandKind::Local(l),
                }.into()
            }).collect()),
        });
        let resource = Resource {
            after_line,
            cond: Default::default(),
            loc: ExpOperand {
                ty: address_ty,
                kind: ExpOperandKind::ExpLocal(0, after_line),
            },
            perm: ty.const_operand(parse::ConstKind::write()).into(),
        };
        ResourceExp { resources: [resource].into_iter().collect(), pure }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum IdData {
    Field {
        address: FunctionId,
    },
    Method {
        method: MethodId,
    },
    Function {
        function: FunctionId,
    },
    Predicate {
        address: FunctionId,
        resource_and_fold: Option<(ResourceId, MethodId)>,
    },
    AdtConstructor {
        adt: AdtId,
        vid: VariantIdx,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum MemberKind {
    Field,
    Predicate,
    Function,
    Method,
    AdtConstructor,
}

impl<'a, 'tcx> Deref for IdT<'a, 'tcx> {
    type Target = TypeT<'a, 'tcx>;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'tcx> DerefMut for IdT<'_, 'tcx> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}
