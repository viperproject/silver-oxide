use crate::{parse::{ConstHeapKind, ConstKind, ExpKind}, translate::member::{tychk::{ExpectedTys, TyIfx, TypeChecker}, ArgRef, TranslationCtxt}, vmir::{middle::{Exp, ExpLine, ExpLocal, ExpOperand, ExpOperandKind, Operand, OperandKind}, ty::{Ty, TyList}}, HashMap, HashSet, TiVec};

#[derive(Debug)]
pub struct ExpBuilder<'tcx> {
    /// The expression under construction
    e: TiVec<ExpLocal, ExpLine<'tcx>>,

    /// How much has been finished with type checking and optimising
    built: ExpLocal,

    /// Cache of already built lines for reuse
    evaluated: HashMap<ExpLine<'tcx>, ExpOperand<'tcx>>,

    /// The infer context for the currently under construction expression
    param_infcx: TypeChecker<'tcx>,
}

impl Default for ExpBuilder<'_> {
    fn default() -> Self {
        Self {
            e: Default::default(),
            built: ExpLocal::ZERO,
            evaluated: Default::default(),
            param_infcx: Default::default(),
        }
    }
}

impl<'tcx> ExpBuilder<'tcx> {
    pub fn new_line(&mut self, line: ExpLine<'tcx>, curr_nest: u16) -> ExpOperand<'tcx> {
        let ty = line.ty;
        let key = self.e.push_and_get_key(line);
        ExpOperand { ty, kind: ExpOperandKind::ExpLocal(curr_nest, key) }
    }

    pub fn curr_line(&self) -> Option<ExpLocal> {
        // assert_eq!(self.built, self.e.next_key());
        self.e.last_key()
    }

    pub fn build(&mut self) -> ExpLocal {
        // todo!();
        self.param_infcx = TypeChecker::default();
        self.built = self.e.next_key();
        self.built
    }

    pub fn finish(&mut self, result: ExpOperand<'tcx>, used: HashSet<ExpLocal>) -> Exp<'tcx> {
        self.build();
        let lines = core::mem::take(&mut self.e);
        // todo!("optimise");
        Exp { lines, result }
    }

    pub fn ty_params<'a>(&mut self, tcx: &'a TranslationCtxt<'_, 'tcx>, count: usize) -> (TyList<'tcx>, Vec<impl Drop + use<'a, 'tcx>>) {
        let (ty_list, drop) = (0..count)
            .map(|_| {
                let ty = self.param_infcx.any_ty(tcx);
                (ty.ty(), ty)
            })
            .unzip();
        (tcx.tcx.interner.mk_ty_list(ty_list), drop)
    }

    pub fn constrained_ty<'a>(&mut self, tcx: &'a TranslationCtxt<'_, 'tcx>, t: ExpectedTys<'tcx>) -> TyIfx<'a, 'tcx> {
        let ty = self.param_infcx.any_ty(tcx);
        self.param_infcx.constrain(ty.ty(), t);
        ty
    }

    pub fn any_address<'a>(&mut self, tcx: &'a TranslationCtxt<'_, 'tcx>) -> TyIfx<'a, 'tcx> {
        self.param_infcx.any_address(tcx)
    }

    pub fn ty_eq<'a>(&mut self, cty: Ty<'tcx>, ty: Ty<'tcx>) {
        self.param_infcx.constrain(cty, ExpectedTys::Exact(ty));
    }







    // pub fn translate(&mut self, tcx: &TranslationCtxt<'_, 'tcx>, exp: &crate::parse::Exp, expected: ExpectedTy<'tcx>) -> ExpOperand<'tcx> {
    //     todo!()
    // }

    // fn translate_inner(&mut self, tcx: &TranslationCtxt<'_, 'tcx>, exp: &crate::parse::Exp, cty: Ty<'tcx>) -> ExpOperand<'tcx> {
    //     // println!("EQOIHI: {tys:?}\n({exp:?})\n");
    //     let line = match &**exp {
    //         ExpKind::Field(..) | ExpKind::Acc(..) | ExpKind::MagicWand(..) => unreachable!(),
    //         ExpKind::Const(c) => {
    //             let c = tcx.tcx.interner.mk_const_ref(c);
    //             let ty = tcx.tcx.const_ty(c);
    //             self.ty_eq(cty, ty);
    //             return Operand {
    //                 ty,
    //                 kind: OperandKind::Const(c),
    //             }
    //             .into();
    //         }
    //         ExpKind::Result => return self.mk_use(tcx, ArgRef::Result, cty),
    //         ExpKind::Old(ident, e) => {
    //             let heap = self.heap.expect("old without heap");
    //             let ty = tcx.tcx.types.heap_;
    //             let new = match ident {
    //                 None => {
    //                     let new = tcx
    //                         .tcx
    //                         .interner
    //                         .mk_const(ConstKind::Heap(ConstHeapKind::Old));
    //                     Operand {
    //                         ty,
    //                         kind: OperandKind::Const(new),
    //                     }
    //                 }
    //                 Some(label) => {
    //                     let label = tcx.tcx.interner.mk_symbol(label);
    //                     Operand {
    //                         ty,
    //                         kind: OperandKind::Local(tcx.params[&ArgRef::Label(label)].0),
    //                     }
    //                 }
    //             };
    //             let new = ExpOperand::from(new);
    //             if heap == new {
    //                 eprintln!("warning: unnecessary old");
    //             }
    //             self.heap = Some(new);
    //             let e = self.translate_inner(tcx, e, cty);
    //             self.heap = Some(heap);
    //             return e;
    //         }
    //         ExpKind::Ascribe(e, ty) => {
    //             let ty = tcx.tycx.translate(ty);
    //             self.ty_eq(cty, ty);
    //             return self.translate_inner(tcx, e, cty);
    //         }
    //         ExpKind::HeapUpdate(op, acc, e) => {
    //             let heap = self.mk_heap_update(*op, acc);
    //             let heap = self.heap.replace(heap);
    //             let r = self.translate_inner(tcx, e, cty);
    //             self.heap = heap;
    //             return r;
    //         }
    //         ExpKind::Quantifier(kind, qvars, triggers, body) => {
    //             let curr_cond = self.curr_cond.take();
    //             let curr_qvars = self.curr_qvars;
    //             self.curr_qvars = QuantLocal::from(usize::from(curr_qvars) + qvars.len());

    //             let (idns, tys): (Vec<_>, Vec<_>) = qvars
    //                 .iter()
    //                 .map(|qv| {
    //                     let idn = tcx.tcx.interner.mk_symbol(&qv.idn.0);
    //                     let ty = tcx.tycx.translate(&qv.ty);
    //                     (idn, ty)
    //                 })
    //                 .unzip();
    //             let tys = tcx.tcx.interner.mk_ty_list(tys);
    //             for (i, (idn, ty)) in idns.iter().zip(tys.iter()).enumerate() {
    //                 let kind =
    //                     ExpOperandKind::QuantLocal(QuantLocal::from(usize::from(curr_qvars) + i));
    //                 let old = self.let_bound.insert(*idn, ExpOperand { ty: *ty, kind });
    //                 assert!(
    //                     old.is_none(),
    //                     "duplicate variable name bound in quantifier {idn:?}"
    //                 );
    //             }
    //             let triggers = triggers
    //                 .iter()
    //                 .map(|t| {
    //                     t.exp
    //                         .iter()
    //                         .map(|st| self.translate_nest(st, ExpectedTys::default()))
    //                         .collect()
    //                 })
    //                 .collect();

    //             let body = self.translate_nest(body, &[tcx.tcx.types.bool_][..]);

    //             for idn in idns.iter().rev() {
    //                 self.let_bound.swap_remove(idn);
    //             }
    //             self.curr_qvars = curr_qvars;
    //             self.curr_cond = curr_cond;

    //             self.mk_line(
    //                 tcx.tcx.types.bool_,
    //                 ExpLineKind::Quantifier(*kind, tys, triggers, body),
    //             )
    //         }
    //         ExpKind::LetIn(decl, val, e) => {
    //             let decl = tcx.tcx.interner.mk_symbol(&decl.0);
    //             let v = self.translate_resolve(val, None);

    //             let old = self.let_bound.insert(decl, v);
    //             assert!(old.is_none(), "duplicate let bound");

    //             let r = self.translate_inner(tcx, e, cty);
    //             self.let_bound.swap_remove(&decl);
    //             return r;
    //         }
    //         ExpKind::ForPerm(..) => todo!(),
    //         ExpKind::FuncApp(ident, args) => {
    //             let callee = tcx.tcx.resolve(ident).unwrap();
    //             let (function, ..) = tcx.tcx.get_any_function(callee).unwrap();
    //             let f = &tcx.tcx.members.functions[function];
    //             let (ifx_params, _) = self.e.ty_params(tcx, f.ty_params.len());
    //             let params = f.sig().params_iter().map(|(_, t)| t.instantiate(tcx(), ifx_params)).collect::<Vec<_>>();
    //             let ret = f.sig().ret().instantiate(tcx(), ifx_params);

    //             // let heap_dependent = tcx.tcx.is_heap_dependent(callee);
    //             // let heap = heap_dependent.then(|| self.heap.unwrap());
    //             // let sig = tcx.tcx.fn_sig(callee).unwrap();

    //             // let caller_args = sig.caller_args().1;
    //             assert_eq!(args.len(), params.len());
    //             let mut args: Vec<_> = args
    //                 .iter()
    //                 .zip(params)
    //                 .map(|(arg, ty)| self.translate_resolve(arg, Some(ty)))
    //                 .collect();
    //             if let Some((pre, snap)) = f.heap_pre() {
    //                 let line = self.mk_line(snap, ExpLineKind::Snapshot(pre, args.clone()));
    //                 let heap_arg = self.new_line(line);
    //                 args.push(heap_arg);
    //             }

    //             let kind = ExpLineKind::Call(function, args);
    //             self.mk_line(ret, kind)
    //         }
    //         ExpKind::Ident(ident) => {
    //             let i = tcx.tcx.interner.mk_symbol(ident);
    //             let lb = self.let_bound.get(&i).copied();
    //             if let Some(nd) = lb {
    //                 self.e.ty_eq(cty, nd.ty);
    //             }
    //             return lb.unwrap_or_else(|| self.mk_use(ArgRef::Ident(i), cty));
    //         }
    //         ExpKind::BinOp(op, lhs, rhs) => {
    //             use crate::parse::BinOp::*;
    //             match *op {
    //                 Or | Implies | And => unreachable!(),
    //                 op => {
    //                     let pbo = self.possible_bin_op(op);
    //                     let mut lhs = self.translate_inner(tcx, lhs, pbo.lhs);
    //                     let mut rhs = self.translate_inner(tcx, rhs, pbo.rhs);
    //                     // if pbo.must_eq {
    //                     //     self.equate_tys(&mut lhs, &mut rhs, pbo.upgrade_real_pre_op && tys.want_real());
    //                     // }
    //                     self.apply_bin_op(op, lhs, rhs)
    //                 }
    //             }
    //         }
    //         ExpKind::Ternary(c, t, e) => match self.mk_ternary(c, t, e, cty) {
    //             Ok(line) => line,
    //             Err(operand) => return operand,
    //         },
    //         ExpKind::Index(..) => todo!(),
    //         ExpKind::UnOp(op, e) => {
    //             let tys = self.possible_un_op(*op);
    //             let e = self.translate_inner(tcx, e, tys.ty());
    //             self.apply_un_op(*op, e)
    //         }
    //         ExpKind::AdtConstructor(cons, args) => {
    //             let cons = tcx.tcx.resolve(cons).unwrap();
    //             let (adt, vid) = tcx.tcx.get_constructor(cons.expect("todo std")).unwrap();
    //             let data = tcx.tcx.interner.get_adt_def(adt).data();
    //             let fields = &data.variants[vid].fields;
    //             assert_eq!(fields.len(), args.len(), "wrong number of fields for adt {adt:?}");
    //             let (ty_list, _) = self.e.ty_params(tcx, data.params.len());
    //             let params = fields.iter().map(|f| f.ty(tcx.tcx, ty_list));
    //             let args = args
    //                 .iter()
    //                 .zip(params)
    //                 .map(|(arg, ty)| self.translate_inner(tcx, arg, ty))
    //                 .collect();
    //             let kind = ExpLineKind::Adt(adt, AdtOp::Construct(vid, args));

    //             let ty = tcx.tcx.interner.mk_ty_from_kind(TyKind::Domain(DomainKind::Adt(adt), ty_list));
    //             self.mk_line(ty, kind)
    //         }
    //         ExpKind::AdtDestructor(e, fd) => {
    //             let (cons, idx) = tcx.tcx.resolve_adt_destr(fd).unwrap();
    //             let fid = FieldIdx::from(idx);
    //             let (adt, vid) = tcx.tcx.get_constructor(cons).unwrap();
    //             let data = tcx.tcx.interner.get_adt_def(adt).data();

    //             let (ty_list, _) = self.e.ty_params(tcx, data.params.len());
    //             let ty = tcx.tcx.interner.mk_ty_from_kind(TyKind::Domain(DomainKind::Adt(adt), ty_list));
    //             let e = self.translate_inner(tcx, e, ty);

    //             let data = tcx.tcx.interner.get_adt_def(adt).data();
    //             let field = data.variants[vid].fields[fid].ty(tcx.tcx, ty_list);

    //             let kind = ExpLineKind::Adt(adt, AdtOp::Destructor(e, vid, fid));
    //             self.mk_line(field, kind)
    //         }
    //         ExpKind::AdtDiscriminator(e, cons) => {
    //             let cons = tcx.tcx.resolve(cons).unwrap();
    //             let (adt, vid) = tcx.tcx.get_constructor(cons.expect("todo std")).unwrap();
    //             let data = tcx.tcx.interner.get_adt_def(adt).data();

    //             let (ty_list, _) = self.e.ty_params(tcx, data.params.len());
    //             let ty = tcx.tcx.interner.mk_ty_from_kind(TyKind::Domain(DomainKind::Adt(adt), ty_list));
    //             let e = self.translate_inner(tcx, e, ty);

    //             let kind = ExpLineKind::Adt(adt, AdtOp::Discriminator(e, vid));
    //             self.mk_line(tcx.tcx.types.bool_, kind)
    //         }
    //     };
    //     let mut result = self.new_line(line);
    //     // if tys.want_real() && matches!(result.ty.kind(), TyKind::Int) {
    //     //     result = self.int_to_real(result);
    //     // }
    //     self.e.ty_eq(cty, result.ty);
    //     result
    // }

    // fn mk_use(&mut self, tcx: &TranslationCtxt<'_, 'tcx>, r: ArgRef<'tcx>, cty: Ty<'tcx>) -> ExpOperand<'tcx> {
    //     let (local, ty) = tcx.get_param(r);
    //     self.ty_eq(cty, ty);
    //     Operand {
    //         ty,
    //         kind: OperandKind::Local(local),
    //     }
    //     .into()
    // }
}

// pub enum ExpectedTy<'tcx> {
//     Exact(Ty<'tcx>),
//     Any,
// }
