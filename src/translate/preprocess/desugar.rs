use crate::{parse::*, translate::{global::{GlobalT, MemberKind}, TyCtxt}};

impl GlobalT<'_, '_> {
    pub(crate) fn desugar_program(&self, program: &mut Program) {
        for (id, decl) in program.iter_mut() {
            Desugar::desugar_decl(self, id, decl);
        }
    }

    // fn get_field_ty(&self, ident: &Ident) -> Ty<'tcx> {
    //     let fd = self.get_callee(ident);
    //     let sig = self.fn_sig(fd).unwrap();
    //     let ty = sig.returns().1[0].kind();
    //     let TyKind::Resource(ResourceKind::Field(ty)) = ty else {
    //         unreachable!();
    //     };
    //     *ty
    // }
}

pub struct Desugar<'r, 'tcx> {
    tcx: &'r GlobalT<'r, 'tcx>,
    _def_id: MemberId,

    in_assert_assume: bool,
    mk_wildcard: bool,

    impure: Option<ImpureCollector<'r>>,
}

impl<'r, 'tcx> Desugar<'r, 'tcx> {
    /// Converts `assert/assume e` into `assert/assume e'` where e.g.
    /// `acc(x.f, p)` is converted to `perm(x.f) >= p`, or `pred(...)` is
    /// converted to `perm(pred(...)) >= write`.
    pub fn desugar_decl(tcx: &'r GlobalT<'r, 'tcx>, _def_id: MemberId, decl: &mut Declaration) {
        let mk_wildcard = matches!(decl, Declaration::Function(_));
        let mut self_ = Self {
            tcx,
            _def_id,
            in_assert_assume: false,
            mk_wildcard,
            impure: None,
        };
        self_.walk_mut_declaration(decl);
    }

    // fn fresh<'s>(&self) -> Desugar<'s, 'tcx> where 'r: 's {
    //     Desugar {
    //         tcx: self.tcx,
    //         def_id: self.def_id,
    //         in_assert_assume: self.in_assert_assume,
    //         mk_wildcard: self.mk_wildcard,
    //         impure: None,
    //     }
    // }
}

impl<'a> AstWalkerMut<'a> for Desugar<'_, '_> {
    fn walk_mut_function(&mut self, ast: &'a mut Function) {
        ast.walk_mut_children(self);
        assert!(
            ast.contract.postcondition.as_ref().is_none_or(|r| r.res.is_empty()),
            "function postcondition not pure"
        )
    }

    fn walk_mut_heap_exp(&mut self, ast: &'a mut HeapExp) {
        assert_eq!(ast.res.len(), 0);
        assert!(self.impure.is_none());
        self.impure = Some(ImpureCollector::default());
        ast.exp.walk_mut(self);
        ast.res = self.impure.take().unwrap().res;
        assert!(!self.in_assert_assume || ast.res.is_empty());
    }

    fn walk_mut_block(&mut self, ast: &'a mut StmtBlock) {
        // Split up `var x, y = e` into `var x, y; x, y = e;`.
        let mut idx = 0;
        while idx < ast.0.len() {
            let Some((keep, replace)) = self.walk_mut_stmt(&mut ast.0[idx]) else {
                idx += 1;
                continue;
            };
            let range = if keep { idx + 1..idx + 1 } else { idx..idx + 1 };
            ast.0.splice(range, replace);
        }
        ast.walk_mut_children(self);
    }

    fn walk_mut_statement(&mut self, ast: &'a mut Statement) {
        self.in_assert_assume = match ast {
            Statement::Assert(exp) => {
                let exp = Self::take(exp);
                *ast = Statement::Exhale(HeapExp::new(exp));
                true
            }
            Statement::Assume(exp) => {
                let exp = Self::take(exp);
                *ast = Statement::Inhale(HeapExp::new(exp));
                true
            }
            _ => false,
        };
        ast.walk_mut_children(self);
        self.in_assert_assume = false;
    }

    fn walk_mut_assign_rhs(&mut self, ast: &'a mut AssignRhs) {
        if let AssignRhs::Call(ident, ..) = ast {
            let callee = self.tcx.resolve(ident).unwrap();
            let kind = self.tcx.get_member_kind(callee).unwrap();
            if !matches!(kind, MemberKind::Method) {
                let call = core::mem::replace(ast, AssignRhs::New(StarOrNames::Star));
                let AssignRhs::Call(ident, args) = call else {
                    unreachable!();
                };
                *ast = AssignRhs::Exp(Box::new(ExpKind::FuncApp(ident, args)));
            }
        }
        ast.walk_mut_children(self);
    }

    fn walk_mut_acc_exp(&mut self, ast: &'a mut AccExp) {
        // Skip over `loc` itself to avoid adding a `Deref` for field
        // accesses/extra `ExpKind::Acc` for predicate.
        self.walk_mut_loc(&mut ast.acc.loc);
        ast.perm.walk_mut(self);
    }

    fn walk_mut_exp(&mut self, ast: &'a mut Exp) {
        let impure = self.impure.take();
        self.impure = self.walk_mut_exp_inner(impure, ast);
    }
}

impl<'r> Desugar<'r, '_> {
    fn walk_mut_exp_inner(
        &mut self,
        mut impure: Option<ImpureCollector<'r>>,
        ast: &mut Exp,
    ) -> Option<ImpureCollector<'r>> {
        match &mut **ast {
            ExpKind::Field(rcv, fd) => {
                if let Some(adt_cons) = fd.0.strip_prefix("is").filter(|s| self.tcx.resolve_adt_cons(s).is_some()) {
                    let new = ExpKind::AdtDiscriminator(Self::take(rcv), Ident(adt_cons.to_string()));
                    self.replace_exp(ast, new);
                } else if let Some(..) = self.tcx.resolve_adt_destr(fd) {
                    let new = ExpKind::AdtDestructor(Self::take(rcv), fd.clone());
                    self.replace_exp(ast, new);
                } else {
                    let loc = Self::take(ast);
                    self.replace_exp(ast, ExpKind::UnOp(UnOp::Deref, loc));
                }
                return impure;
            }
            ExpKind::Acc(acc) => {
                if self.in_assert_assume {
                    assert!(!self.mk_wildcard);
                    let loc = Self::take(&mut acc.acc.loc);
                    let perm = Self::take(&mut acc.perm);
                    let new =
                        ExpKind::BinOp(BinOp::Ge, Box::new(ExpKind::UnOp(UnOp::Perm, loc)), perm);
                    self.replace_exp(ast, new);
                    return impure;
                }
                let Some(impure) = &mut impure else {
                    panic!("impure access in pure context: {ast:?}");
                };
                acc.walk_mut(self);
                let exp = core::mem::replace(ast, ExpKind::bool(true));
                let ExpKind::Acc(acc) = *exp else {
                    unreachable!();
                };
                impure.push(acc, self.mk_wildcard);
            }
            ExpKind::HeapUpdate(_, acc, _) => {
                if self.mk_wildcard && !matches!(*acc.perm, ExpKind::Const(ConstKind::Wildcard)) {
                    assert_eq!(*acc.perm, ExpKind::Const(ConstKind::write()));
                    *acc.perm = ExpKind::Const(ConstKind::Wildcard);
                }
            }
            ExpKind::FuncApp(ident, _) => {
                let callee = self.tcx.resolve(ident).unwrap();
                let kind = self.tcx.get_member_kind(callee).unwrap();
                match kind {
                    MemberKind::Predicate => {
                        let loc = Self::take(ast);
                        let loc = AccExp {
                            acc: LocAccess { loc },
                            perm: ExpKind::write(),
                        };
                        self.impure = impure;
                        self.replace_exp(ast, ExpKind::Acc(loc));
                        return self.impure.take();
                    }
                    MemberKind::AdtConstructor => {
                        let ExpKind::FuncApp(ident, args) = *Self::take(ast) else {
                            unreachable!();
                        };
                        let new = ExpKind::AdtConstructor(ident, args);
                        self.replace_exp(ast, new);
                        return impure;
                    }
                    _ => (),
                }
            }
            ExpKind::MagicWand(..) => {
                let loc = Self::take(ast);
                let loc = AccExp {
                    acc: LocAccess { loc },
                    perm: ExpKind::write(),
                };
                self.impure = impure;
                self.replace_exp(ast, ExpKind::Acc(loc));
                return self.impure.take();
            }
            ExpKind::UnOp(UnOp::Perm | UnOp::Deref, e) => {
                self.walk_mut_loc(e);
                return impure;
            }
            ExpKind::BinOp(op, lhs, rhs) => match *op {
                BinOp::And if impure.is_some() => {
                    self.impure = impure;
                    lhs.walk_mut(self);

                    let c = Self::take(lhs);
                    let mut t = Self::take(rhs);
                    let mut e = ExpKind::bool(false);

                    let impure = self.impure.take().unwrap();
                    let impure = impure.with_cond(self, &c, &mut t, &mut e);

                    **ast = ExpKind::Ternary(c, t, e);
                    return Some(impure);
                }
                BinOp::And | BinOp::Or | BinOp::Implies => {
                    let lhs = Self::take(lhs);
                    let rhs = Self::take(rhs);
                    let (t, e) = match *op {
                        BinOp::And => (rhs, ExpKind::bool(false)),
                        BinOp::Or => (ExpKind::bool(true), rhs),
                        BinOp::Implies => (rhs, ExpKind::bool(true)),
                        _ => unreachable!(),
                    };
                    let new = ExpKind::Ternary(lhs, t, e);
                    self.impure = impure;
                    self.replace_exp(ast, new);
                    return self.impure.take();
                }
                BinOp::Gt => {
                    let lhs = Self::take(lhs);
                    let rhs = Self::take(rhs);
                    let new = ExpKind::BinOp(BinOp::Lt, rhs, lhs);
                    self.replace_exp(ast, new);
                    return impure;
                }
                BinOp::Ge => {
                    let lhs = Self::take(lhs);
                    let rhs = Self::take(rhs);
                    let new = ExpKind::BinOp(BinOp::Le, rhs, lhs);
                    self.replace_exp(ast, new);
                    return impure;
                }
                BinOp::Neq => {
                    *op = BinOp::Eq;
                    let eq = Self::take(ast);
                    let new = ExpKind::UnOp(UnOp::Not, eq);
                    self.replace_exp(ast, new);
                    return impure;
                }
                _ => (),
            },
            ExpKind::Ternary(c, t, e) if impure.is_some() => {
                c.walk_mut(self);
                let impure = impure.unwrap().with_cond(self, c, t, e);
                return Some(impure);
            }
            // Need to insert this into `impure` somehow.
            ExpKind::LetIn(..) if impure.is_some() => todo!(),
            _ => (),
        };
        ast.walk_mut_children(self);
        impure
    }

    fn take(e: &mut Exp) -> Exp {
        let src = Box::new(ExpKind::Ident(Ident(String::new())));
        std::mem::replace(e, src)
    }

    fn replace_exp(&mut self, exp: &mut Exp, new: ExpKind) {
        *exp = Box::new(new);
        self.walk_mut_exp(exp);
    }

    fn walk_mut_loc(&mut self, exp: &mut Exp) {
        if let ExpKind::Field(..) = &**exp {
            let ExpKind::Field(rcv, fd) = *Self::take(exp) else {
                unreachable!();
            };
            *exp = Box::new(ExpKind::FuncApp(fd, vec![rcv]));
        }
        ExpKind::walk_mut_children(&mut **exp, self);
    }

    fn walk_mut_stmt(
        &mut self,
        ast: &mut Statement,
    ) -> Option<(bool, impl Iterator<Item = Statement> + '_)> {
        match ast {
            Statement::Var(new, init) => {
                let init = init.take()?;
                let tgts: Vec<_> = new
                    .iter()
                    .map(|n| Box::new(ExpKind::Ident(n.idn.0.clone())))
                    .collect();
                Some((true, [Statement::Assign(tgts, init)].into_iter()))
            }
            // Statement::Assign(tgts, e) => {
            //     let mut pre_assigns = Vec::new();
            //     let mut post_assigns = Vec::new();
            //     // Replace all `fd(rcv) := ...` with:
            //     // ```
            //     // var ref_tmp: Ref := rcv
            //     // var fd_tmp: FieldType
            //     // fd_tmp := ...
            //     // fd(ref_tmp) := fd_tmp
            //     // ```
            //     for tgt in tgts.iter_mut() {
            //         let ExpKind::UnOp(UnOp::Deref, e) = &mut **tgt else {
            //             continue;
            //         };
            //         let ExpKind::FuncApp(fd, args) = &mut **e else {
            //             unreachable!();
            //         };
            //         assert_eq!(args.len(), 1);
            //         let ref_ty = self.tcx.types.ref_;
            //         let ref_tmp = self.new_tmp();
            //         let ref_tmp = Statement::Var(vec![IdnDeclTyped { idn: IdnDecl(ref_tmp.clone()), ty: Type::Ref }], ())

            //         let fd_ty = self.tcx.get_field_ty(fd);
            //         let fd_tmp = self.new_tmp();
            //         core::mem::replace(&mut *args[0], ExpKind::Ident()
            //     }
            //     todo!()
            // }
            _ => None,
        }
    }
}

#[derive(Default)]
struct ImpureCollector<'r> {
    res: Vec<ResourceExp>,
    pc: Vec<(bool, &'r Exp)>,
}

impl<'r> ImpureCollector<'r> {
    fn push(&mut self, mut acc: AccExp, mk_wildcard: bool) {
        let mut cond: Vec<_> = self
            .pc
            .iter()
            .copied()
            .map(|(neg, c)| (neg, c.clone()))
            .collect();
        if mk_wildcard {
            let c = ExpKind::BinOp(BinOp::Lt, ExpKind::none(), acc.perm);
            cond.push((false, Box::new(c)));
            acc.perm = ExpKind::wildcard();
        }
        self.res.push(ResourceExp { cond, acc });
    }

    fn with_cond<'tcx>(
        mut self,
        desugar: &mut Desugar<'r, 'tcx>,
        c: &Exp,
        t: &mut Exp,
        e: &mut Exp,
    ) -> Self {
        let c = unsafe { &*(c as *const _) };
        let expect_len = self.pc.len();
        self.pc.push((false, c));

        desugar.impure = Some(self);
        t.walk_mut(desugar);
        self = desugar.impure.take().unwrap();

        assert_eq!(self.pc.len(), expect_len + 1);
        self.pc.last_mut().unwrap().0 = true;

        desugar.impure = Some(self);
        e.walk_mut(desugar);
        self = desugar.impure.take().unwrap();

        assert_eq!(self.pc.len(), expect_len + 1);
        self.pc.pop();
        self
    }
}
