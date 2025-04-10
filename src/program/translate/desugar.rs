use crate::{parse::*, program::{idx::LocalDefId, TyCtxt}};

impl<'tcx> TyCtxt<'tcx> {
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
    tcx: &'r TyCtxt<'tcx>,
    def_id: LocalDefId,

    in_assert_assume: bool,
    replace_write_wildcard: bool,
    // tmp_count: usize,
}

impl<'r, 'tcx> Desugar<'r, 'tcx> {
    /// Converts `assert/assume e` into `assert/assume e'` where e.g.
    /// `acc(x.f, p)` is converted to `perm(x.f) >= p`, or `pred(...)` is
    /// converted to `perm(pred(...)) >= write`.
    pub fn desugar_decl(tcx: &'r TyCtxt<'tcx>, def_id: LocalDefId, decl: &mut Declaration) {
        let replace_write_wildcard = matches!(decl, Declaration::Function(_));
        let mut self_ = Self {
            tcx,
            def_id,
            in_assert_assume: false,
            replace_write_wildcard,
            // tmp_count: 0,
        };
        self_.walk_mut_declaration(decl);
    }

    fn fresh(&self) -> Self {
        Self {
            tcx: self.tcx,
            def_id: self.def_id,
            in_assert_assume: false,
            replace_write_wildcard: false,
        }
    }
}

impl<'a> AstWalkerMut<'a> for Desugar<'_, '_> {
    fn walk_mut_block(&mut self, ast: &'a mut Block) {
        // Split up `var x, y = e` into `var x, y; x, y = e;`.
        let mut idx = 0;
        while idx < ast.statements.len() {
            let Some((keep, replace)) = self.walk_mut_stmt(&mut ast.statements[idx]) else {
                idx += 1;
                continue;
            };
            let range = if keep { idx+1..idx+1 } else { idx..idx+1 };
            ast.statements.splice(range, replace);
        }
        ast.walk_mut_children(self);
    }

    fn walk_mut_statement(&mut self, ast: &'a mut Statement) {
        self.in_assert_assume = match ast {
            Statement::Assert(exp) => {
                let exp = Self::take(exp);
                *ast = Statement::Exhale(exp);
                true
            }
            Statement::Assume(exp) => {
                let exp = Self::take(exp);
                *ast = Statement::Inhale(exp);
                true
            }
            _ => false,
        };
        ast.walk_mut_children(self);
        self.in_assert_assume = false;
    }

    fn walk_mut_acc_exp(&mut self, ast: &'a mut AccExp) {
        ast.acc.loc.walk_mut_children(self);
        ast.perm.walk_mut_children(self);
    }

    fn walk_mut_exp(&mut self, ast: &'a mut Exp) {
        match &mut **ast {
            ExpKind::Field(..) => {
                let loc = Self::take(ast);
                return self.replace_exp(ast, ExpKind::UnOp(UnOp::Deref, loc));
            }
            ExpKind::Acc(acc) => {
                if self.replace_write_wildcard {
                    Self::mk_perm_wildcard(&mut acc.perm);
                }
                if self.in_assert_assume {
                    let loc = Self::take(&mut acc.acc.loc);
                    let perm = Self::take(&mut acc.perm.as_mut().ok().unwrap());
                    let new = ExpKind::BinOp(
                        BinOp::Ge,
                        Box::new(ExpKind::UnOp(UnOp::Perm, loc)),
                        perm,
                    );
                    return self.replace_exp(ast, new);
                }
                self.walk_mut_pred(&mut acc.acc.loc);
                acc.perm.walk_mut(self);
                return;
            }
            ExpKind::HeapUpdate(op, acc, e) => {
                op.walk_mut(self);
                acc.walk_mut(&mut self.fresh());
                e.walk_mut(self);
                if self.replace_write_wildcard {
                    let perm = acc.perm.as_mut().ok().unwrap();
                    assert_eq!(**perm, ExpKind::Const(ConstKind::Write));
                    **acc.perm.as_mut().ok().unwrap() = ExpKind::Const(ConstKind::Wildcard);
                }
                return;
            }
            ExpKind::FuncApp(ident, _) => {
                let callee = self.tcx.get_callee(ident);
                if self.tcx.is_predicate(callee) {
                    let loc = Self::take(ast);
                    let loc = AccExp {
                        acc: LocAccess { loc },
                        perm: Ok(ConstKind::write()),
                    };
                    return self.replace_exp(ast, ExpKind::Acc(loc));
                }
            }
            ExpKind::UnOp(UnOp::Perm | UnOp::Deref, e) => {
                return self.walk_mut_pred(e);
            },
            ExpKind::BinOp(op, lhs, rhs) => match *op {
                // BinOp::And => {
                //     let lhs = Self::take(lhs);
                //     let rhs = Self::take(rhs);
                //     let new = ExpKind::Ternary(lhs, rhs, ConstKind::bool(false));
                //     return self.replace_exp(ast, new);
                // }
                BinOp::Or => {
                    let lhs = Self::take(lhs);
                    let rhs = Self::take(rhs);
                    let new = ExpKind::Ternary(lhs, ConstKind::bool(true), rhs);
                    return self.replace_exp(ast, new);
                }
                BinOp::Implies => {
                    let lhs = Self::take(lhs);
                    let rhs = Self::take(rhs);
                    let new = ExpKind::Ternary(lhs, rhs, ConstKind::bool(true));
                    return self.replace_exp(ast, new);
                }
                _ => (),
            }
            _ => (),
        };
        ast.walk_mut_children(self);
    }
}

impl<'r, 'tcx> Desugar<'r, 'tcx> {
    pub fn replace_exp(&mut self, exp: &mut Exp, new: ExpKind) {
        *exp = Box::new(new);
        self.walk_mut_exp(exp);
    }

    pub fn walk_mut_pred(&mut self, exp: &mut Exp) {
        if let ExpKind::Field(..) = &**exp {
            let ExpKind::Field(rcv, fd) = *Self::take(exp) else {
                unreachable!();
            };
            *exp = Box::new(ExpKind::FuncApp(fd, vec![rcv]));
        }
        exp.walk_mut_children(self);
    }

    fn take(e: &mut Exp) -> Exp {
        std::mem::replace(e, Box::new(ExpKind::Result))
    }

    fn walk_mut_stmt(&mut self, ast: &mut Statement) -> Option<(bool, impl Iterator<Item = Statement> + '_)> {
        match ast {
            Statement::Var(new, init) => {
                let init = init.take()?;
                let tgts: Vec<_> = new.iter().map(|n| Box::new(ExpKind::Ident(n.idn.0.clone()))).collect();
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

    fn mk_perm_wildcard(perm: &mut Result<Exp, Exp>) {
        let p = Self::take(perm.as_mut().ok().unwrap());
        *perm = Err(Box::new(ExpKind::BinOp(BinOp::Lt, ConstKind::none(), p)));
    }

    // fn new_tmp(&mut self) -> Ident {
    //     let tmp = format!("__tmp_{}", self.tmp_count);
    //     self.tmp_count += 1;
    //     Ident::new(tmp)
    // }
}
