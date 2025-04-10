use num_bigint::BigInt;

use crate::{parse::ast::*, program::idx::LocalDefId, TiVec};

macro_rules! walk_children {
    ($name:ident, $l:lifetime, $ty:ident) => {
        /// Call `ast.walk_children(self)` to keep recursing.
        fn $name(&mut self, ast: &$l $ty) {
            ast.walk_children(self);
        }
    };
}

pub trait AstWalker<'a>: Sized {
    walk_children!(walk_program, 'a, Program);
    walk_children!(walk_pre_post_dec, 'a, PrePostDec);
    walk_children!(walk_decreases, 'a, Decreases);
    walk_children!(walk_decreases_kind, 'a, DecreasesKind);
    walk_children!(walk_ident, 'a, Ident);
    walk_children!(walk_idn_decl, 'a, IdnDecl);
    walk_children!(walk_declaration, 'a, Declaration);
    walk_children!(walk_domain_element, 'a, DomainElement);
    walk_children!(walk_domain_element_kind, 'a, DomainElementKind);
    walk_children!(walk_axiom, 'a, Axiom);
    walk_children!(walk_import, 'a, Import);
    walk_children!(walk_define, 'a, Define);
    walk_children!(walk_exp_or_block, 'a, ExpOrBlock);
    walk_children!(walk_idn_decl_typed, 'a, IdnDeclTyped);
    walk_children!(walk_arg_or_type, 'a, ArgOrType);
    walk_children!(walk_exp_block, 'a, ExpBlock);
    walk_children!(walk_exp, 'a, Exp);
    walk_children!(walk_exp_kind, 'a, ExpKind);
    walk_children!(walk_const, 'a, ConstKind);
    walk_children!(walk_heap_op_kind, 'a, HeapUpdateOp);
    walk_children!(walk_quantifier_kind, 'a, QuantifierKind);
    walk_children!(walk_acc_exp, 'a, AccExp);
    walk_children!(walk_bin_op, 'a, BinOp);
    walk_children!(walk_un_op, 'a, UnOp);
    walk_children!(walk_trigger, 'a, Trigger);
    walk_children!(walk_res_access, 'a, ResAccess);
    walk_children!(walk_block, 'a, Block);
    walk_children!(walk_statement, 'a, Statement);
    walk_children!(walk_star_or_names, 'a, StarOrNames);
    walk_children!(walk_index_op, 'a, IndexOp);
    walk_children!(walk_invariant, 'a, Invariant);
    walk_children!(walk_while_spec, 'a, WhileSpec);
    walk_children!(walk_loc_access, 'a, LocAccess);
    walk_children!(walk_field, 'a, Field);
    walk_children!(walk_domain, 'a, Domain);
    walk_children!(walk_domain_function, 'a, DomainFunction);
    walk_children!(walk_signature, 'a, Signature);
    walk_children!(walk_type, 'a, Type);
    walk_children!(walk_predicate, 'a, Predicate);
    walk_children!(walk_function, 'a, Function);
    walk_children!(walk_contract, 'a, Contract);
    walk_children!(walk_method, 'a, Method);
    walk_children!(walk_adt, 'a, Adt);
    walk_children!(walk_variant, 'a, Variant);
    // walk_children!(walk_domain_element, 'a, DomainElement);

    walk_children!(walk_string, 'a, String);
    walk_children!(walk_bool, 'a, bool);
    walk_children!(walk_big_int, 'a, BigInt);

    #[allow(unused_variables)]
    fn visit_local_def_id(&mut self, did: LocalDefId) {}
}

macro_rules! walk_mut_children {
    ($name:ident, $l:lifetime, $ty:ident) => {
        fn $name(&mut self, ast: &'a mut $ty) {
            ast.walk_mut_children(self);
        }
    };
}

pub trait AstWalkerMut<'a>: Sized {
    walk_mut_children!(walk_mut_program, 'a, Program);
    walk_mut_children!(walk_mut_pre_post_dec, 'a, PrePostDec);
    walk_mut_children!(walk_mut_decreases, 'a, Decreases);
    walk_mut_children!(walk_mut_decreases_kind, 'a, DecreasesKind);
    walk_mut_children!(walk_mut_ident, 'a, Ident);
    walk_mut_children!(walk_mut_idn_decl, 'a, IdnDecl);
    walk_mut_children!(walk_mut_declaration, 'a, Declaration);
    walk_mut_children!(walk_mut_domain_element, 'a, DomainElement);
    walk_mut_children!(walk_mut_domain_element_kind, 'a, DomainElementKind);
    walk_mut_children!(walk_mut_axiom, 'a, Axiom);
    walk_mut_children!(walk_mut_import, 'a, Import);
    walk_mut_children!(walk_mut_define, 'a, Define);
    walk_mut_children!(walk_mut_exp_or_block, 'a, ExpOrBlock);
    walk_mut_children!(walk_mut_idn_decl_typed, 'a, IdnDeclTyped);
    walk_mut_children!(walk_mut_arg_or_type, 'a, ArgOrType);
    walk_mut_children!(walk_mut_exp_block, 'a, ExpBlock);
    walk_mut_children!(walk_mut_exp, 'a, Exp);
    walk_mut_children!(walk_mut_exp_kind, 'a, ExpKind);
    walk_mut_children!(walk_mut_const, 'a, ConstKind);
    walk_mut_children!(walk_mut_heap_op_kind, 'a, HeapUpdateOp);
    walk_mut_children!(walk_mut_quantifier_kind, 'a, QuantifierKind);
    walk_mut_children!(walk_mut_acc_exp, 'a, AccExp);
    walk_mut_children!(walk_mut_bin_op, 'a, BinOp);
    walk_mut_children!(walk_mut_un_op, 'a, UnOp);
    walk_mut_children!(walk_mut_trigger, 'a, Trigger);
    walk_mut_children!(walk_mut_res_access, 'a, ResAccess);
    walk_mut_children!(walk_mut_block, 'a, Block);
    walk_mut_children!(walk_mut_statement, 'a, Statement);
    walk_mut_children!(walk_mut_star_or_names, 'a, StarOrNames);
    walk_mut_children!(walk_mut_index_op, 'a, IndexOp);
    walk_mut_children!(walk_mut_invariant, 'a, Invariant);
    walk_mut_children!(walk_mut_while_spec, 'a, WhileSpec);
    walk_mut_children!(walk_mut_loc_access, 'a, LocAccess);
    walk_mut_children!(walk_mut_field, 'a, Field);
    walk_mut_children!(walk_mut_domain, 'a, Domain);
    walk_mut_children!(walk_mut_domain_function, 'a, DomainFunction);
    walk_mut_children!(walk_mut_signature, 'a, Signature);
    walk_mut_children!(walk_mut_type, 'a, Type);
    walk_mut_children!(walk_mut_predicate, 'a, Predicate);
    walk_mut_children!(walk_mut_function, 'a, Function);
    walk_mut_children!(walk_mut_contract, 'a, Contract);
    walk_mut_children!(walk_mut_method, 'a, Method);
    walk_mut_children!(walk_mut_adt, 'a, Adt);
    walk_mut_children!(walk_mut_variant, 'a, Variant);
    // walk_mut_children!(walk_mut_domain_element, 'a, DomainElement);

    walk_mut_children!(walk_mut_string, 'a, String);
    walk_mut_children!(walk_mut_bool, 'a, bool);
    walk_mut_children!(walk_mut_big_int, 'a, BigInt);

    #[allow(unused_variables)]
    fn visit_local_def_id(&mut self, did: LocalDefId) {}
}

pub trait AstWalkable {
    fn walk<'a>(&'a self, walker: &mut impl AstWalker<'a>);
    fn walk_mut<'a>(&'a mut self, walker: &mut impl AstWalkerMut<'a>);
    fn walk_children<'a>(&'a self, walker: &mut impl AstWalker<'a>);
    fn walk_mut_children<'a>(&'a mut self, walker: &mut impl AstWalkerMut<'a>);
}

macro_rules! walk_struct {
    ($name:ident, $walk:ident, $walk_mut:ident$(, $field:tt)*) => {
        impl AstWalkable for $name {
            fn walk<'a>(&'a self, walker: &mut impl AstWalker<'a>) {
                walker.$walk(self);
            }
            fn walk_mut<'a>(&'a mut self, walker: &mut impl AstWalkerMut<'a>) {
                walker.$walk_mut(self);
            }
            #[allow(unused_variables)]
            fn walk_children<'a>(&'a self, walker: &mut impl AstWalker<'a>) {
                $(self.$field.walk(walker));*
            }
            #[allow(unused_variables)]
            fn walk_mut_children<'a>(&'a mut self, walker: &mut impl AstWalkerMut<'a>) {
                $(self.$field.walk_mut(walker));*
            }
        }
    };
}

macro_rules! walk_enum {
    ($name:ident, $walk:ident, $walk_mut:ident$(, $variant:ident$(($($field:ident),+))?)*) => {
        impl AstWalkable for $name {
            fn walk<'a>(&'a self, walker: &mut impl AstWalker<'a>) {
                walker.$walk(self);
            }
            fn walk_mut<'a>(&'a mut self, walker: &mut impl AstWalkerMut<'a>) {
                walker.$walk_mut(self);
            }
            #[allow(unused_variables)]
            fn walk_children<'a>(&'a self, walker: &mut impl AstWalker<'a>) {
                match self {
                    $($name::$variant$(($($field),+))? => { $($($field.walk(walker));+)? })*
                }
            }
            #[allow(unused_variables)]
            fn walk_mut_children<'a>(&'a mut self, walker: &mut impl AstWalkerMut<'a>) {
                match self {
                    $($name::$variant$(($($field),+))? => { $($($field.walk_mut(walker));+)? })*
                }
            }
        }
    };
}

macro_rules! walk_box {
    ($name:ident, $walk:ident, $walk_mut:ident) => {
        impl AstWalkable for $name {
            fn walk<'a>(&'a self, walker: &mut impl AstWalker<'a>) {
                walker.$walk(self);
            }
            fn walk_mut<'a>(&'a mut self, walker: &mut impl AstWalkerMut<'a>) {
                walker.$walk_mut(self);
            }
            #[allow(unused_variables)]
            fn walk_children<'a>(&'a self, walker: &mut impl AstWalker<'a>) {
                (**self).walk(walker);
            }
            #[allow(unused_variables)]
            fn walk_mut_children<'a>(&'a mut self, walker: &mut impl AstWalkerMut<'a>) {
                (**self).walk_mut(walker);
            }
        }
    };
}

walk_struct!(Program, walk_program, walk_mut_program, 0);
walk_enum!(
    PrePostDec,
    walk_pre_post_dec,
    walk_mut_pre_post_dec,
    Pre(e),
    Post(e),
    Decreases(d)
);
walk_struct!(Decreases, walk_decreases, walk_mut_decreases, kind, guard);
walk_enum!(
    DecreasesKind,
    walk_decreases_kind,
    walk_mut_decreases_kind,
    Star,
    Underscore,
    Exp(e)
);
walk_struct!(Ident, walk_ident, walk_mut_ident, 0);
walk_struct!(IdnDecl, walk_idn_decl, walk_mut_idn_decl, 0);
walk_enum!(
    Declaration,
    walk_declaration,
    walk_mut_declaration,
    Import(i),
    Define(d),
    Domain(d),
    DomainElement(f),
    Field(f),
    Function(f),
    Predicate(p),
    Method(m),
    Adt(a)
);
walk_struct!(
    DomainElement,
    walk_domain_element,
    walk_mut_domain_element,
    domain,
    kind
);
walk_enum!(
    DomainElementKind,
    walk_domain_element_kind,
    walk_mut_domain_element_kind,
    Function(f),
    Axiom(a)
);
walk_struct!(Axiom, walk_axiom, walk_mut_axiom, name, exp);
walk_struct!(Import, walk_import, walk_mut_import, path);
walk_struct!(Define, walk_define, walk_mut_define, name, args, body);
walk_enum!(
    ExpOrBlock,
    walk_exp_or_block,
    walk_mut_exp_or_block,
    Exp(e),
    Block(b)
);
walk_struct!(
    IdnDeclTyped,
    walk_idn_decl_typed,
    walk_mut_idn_decl_typed,
    idn,
    ty
);
walk_enum!(
    ArgOrType,
    walk_arg_or_type,
    walk_mut_arg_or_type,
    Arg(a),
    Type(t)
);
walk_struct!(ExpBlock, walk_exp_block, walk_mut_exp_block, 0);
walk_box!(Exp, walk_exp, walk_mut_exp);
walk_enum!(
    ExpKind,
    walk_exp_kind,
    walk_mut_exp_kind,
    Const(c),
    Result,
    Old(i, e),
    Ascribe(e, t),
    HeapUpdate(kind, acc_exp, e),
    Quantifier(kind, vars, triggers, e),
    LetIn(i, e1, e2),
    ForPerm(vars, p, e),
    Acc(a),
    FuncApp(i, args),
    Ident(i),
    BinOp(op, l, r),
    Ternary(c, t, e),
    Field(e, i),
    Index(e, op),
    UnOp(op, e)
);
walk_enum!(
    ConstKind,
    walk_const,
    walk_mut_const,
    Bool(b),
    Int(i),
    Null,
    None,
    Write,
    Epsilon,
    Wildcard
);

walk_enum!(
    HeapUpdateOp,
    walk_heap_op_kind,
    walk_mut_heap_op_kind,
    Unfold,
    Fold,
    Apply,
    Package
);
walk_enum!(
    QuantifierKind,
    walk_quantifier_kind,
    walk_mut_quantifier_kind,
    Forall,
    Exists
);
walk_struct!(AccExp, walk_acc_exp, walk_mut_acc_exp, acc, perm);
walk_enum!(
    BinOp,
    walk_bin_op,
    walk_mut_bin_op,
    Implies,
    Iff,
    And,
    Or,
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
    In,
    Plus,
    Minus,
    Mult,
    Div,
    Mod,
    IntDiv,
    Union,
    SetMinus,
    Intersection,
    Subset,
    Concat,
    MagicWand,
    Range,
    InhaleExhale
);
walk_enum!(UnOp, walk_un_op, walk_mut_un_op, Not, Neg, IntToReal, Deref, Abs, Perm);
walk_struct!(Trigger, walk_trigger, walk_mut_trigger, exp);
walk_enum!(
    ResAccess,
    walk_res_access,
    walk_mut_res_access,
    Loc(l),
    Exp(e)
);
walk_struct!(Block, walk_block, walk_mut_block, statements);
walk_enum!(
    Statement,
    walk_statement,
    walk_mut_statement,
    Assert(e),
    Refute(e),
    Assume(e),
    Inhale(e),
    Exhale(e),
    Fold(e),
    Unfold(e),
    Goto(i),
    Label(i, invariants),
    Havoc(l),
    QuasiHavoc(e1, e2),
    QuasiHavocAll(vars, e1, e2),
    Var(vars, e),
    While(e, specs, b),
    If(e, b, branches, else_),
    Wand(i, e),
    Package(e, b),
    Apply(e),
    Assign(lhs, rhs),
    Fresh(vars),
    Constraining(vars, b),
    Block(b),
    New(i, star_or_names)
);
walk_enum!(
    StarOrNames,
    walk_star_or_names,
    walk_mut_star_or_names,
    Star,
    Names(names)
);
walk_enum!(
    IndexOp,
    walk_index_op,
    walk_mut_index_op,
    Index(e),
    LowerBound(e),
    UpperBound(e),
    Range(e1, e2),
    Assign(e1, e2)
);
walk_struct!(Invariant, walk_invariant, walk_mut_invariant, 0);
walk_enum!(
    WhileSpec,
    walk_while_spec,
    walk_mut_while_spec,
    Inv(i),
    Dec(d)
);
walk_struct!(LocAccess, walk_loc_access, walk_mut_loc_access, loc);
walk_struct!(Field, walk_field, walk_mut_field, 0);
walk_struct!(Domain, walk_domain, walk_mut_domain, name, interpretation);
walk_struct!(
    Function,
    walk_function,
    walk_mut_function,
    signature,
    contract,
    body
);
walk_struct!(
    Contract,
    walk_contract,
    walk_mut_contract,
    precondition,
    postcondition,
    decreases
);
walk_struct!(
    DomainFunction,
    walk_domain_function,
    walk_mut_domain_function,
    unique,
    signature,
    interpretation
);
walk_struct!(
    Signature,
    walk_signature,
    walk_mut_signature,
    name,
    args,
    ret
);
walk_enum!(
    Type,
    walk_type,
    walk_mut_type,
    Bool,
    Int,
    Real,
    Ref,
    Domain(i, ts)
);
walk_struct!(
    Predicate,
    walk_predicate,
    walk_mut_predicate,
    signature,
    body
);
walk_struct!(
    Method,
    walk_method,
    walk_mut_method,
    signature,
    contract,
    body
);
walk_struct!(Adt, walk_adt, walk_mut_adt, name, args, variants, derives);
walk_struct!(Variant, walk_variant, walk_mut_variant, name, fields);

// impl<T: AstWalkable> AstWalkable for Box<T> {
//     fn walk<'a>(&'a self, walker: &mut impl AstWalker<'a>) {
//         self.walk_children(walker);
//     }
//     fn walk_mut<'a>(&'a mut self, walker: &mut impl AstWalkerMut<'a>) {
//         self.walk_mut_children(walker);
//     }
//     fn walk_children<'a>(&'a self, walker: &mut impl AstWalker<'a>) {
//         (**self).walk(walker);
//     }
//     fn walk_mut_children<'a>(&'a mut self, walker: &mut impl AstWalkerMut<'a>) {
//         (**self).walk_mut(walker);
//     }
// }
impl<T: AstWalkable> AstWalkable for Vec<T> {
    fn walk<'a>(&'a self, walker: &mut impl AstWalker<'a>) {
        self.walk_children(walker);
    }
    fn walk_mut<'a>(&'a mut self, walker: &mut impl AstWalkerMut<'a>) {
        self.walk_mut_children(walker);
    }
    fn walk_children<'a>(&'a self, walker: &mut impl AstWalker<'a>) {
        for item in self {
            item.walk(walker);
        }
    }
    fn walk_mut_children<'a>(&'a mut self, walker: &mut impl AstWalkerMut<'a>) {
        for item in self {
            item.walk_mut(walker);
        }
    }
}
impl<I: AstVisitable + From<usize>, T: AstWalkable> AstWalkable for TiVec<I, T> {
    fn walk<'a>(&'a self, walker: &mut impl AstWalker<'a>) {
        self.walk_children(walker);
    }
    fn walk_mut<'a>(&'a mut self, walker: &mut impl AstWalkerMut<'a>) {
        self.walk_mut_children(walker);
    }
    fn walk_children<'a>(&'a self, walker: &mut impl AstWalker<'a>) {
        for (i, item) in self.iter_enumerated() {
            i.visit(walker);
            item.walk(walker);
        }
    }
    fn walk_mut_children<'a>(&'a mut self, walker: &mut impl AstWalkerMut<'a>) {
        for (i, item) in self.iter_mut_enumerated() {
            i.visit_mut(walker);
            item.walk_mut(walker);
        }
    }
}
impl<T: AstWalkable> AstWalkable for Option<T> {
    fn walk<'a>(&'a self, walker: &mut impl AstWalker<'a>) {
        self.walk_children(walker);
    }
    fn walk_mut<'a>(&'a mut self, walker: &mut impl AstWalkerMut<'a>) {
        self.walk_mut_children(walker);
    }
    fn walk_children<'a>(&'a self, walker: &mut impl AstWalker<'a>) {
        if let Some(item) = self {
            item.walk(walker);
        }
    }
    fn walk_mut_children<'a>(&'a mut self, walker: &mut impl AstWalkerMut<'a>) {
        if let Some(item) = self {
            item.walk_mut(walker);
        }
    }
}
impl<T: AstWalkable, U: AstWalkable> AstWalkable for Result<T, U> {
    fn walk<'a>(&'a self, walker: &mut impl AstWalker<'a>) {
        self.walk_children(walker);
    }
    fn walk_mut<'a>(&'a mut self, walker: &mut impl AstWalkerMut<'a>) {
        self.walk_mut_children(walker);
    }
    fn walk_children<'a>(&'a self, walker: &mut impl AstWalker<'a>) {
        match self {
            Ok(item) => item.walk(walker),
            Err(item) => item.walk(walker),
        }
    }
    fn walk_mut_children<'a>(&'a mut self, walker: &mut impl AstWalkerMut<'a>) {
        match self {
            Ok(item) => item.walk_mut(walker),
            Err(item) => item.walk_mut(walker),
        }
    }
}
impl<T: AstWalkable, U: AstWalkable> AstWalkable for (T, U) {
    fn walk<'a>(&'a self, walker: &mut impl AstWalker<'a>) {
        self.walk_children(walker);
    }
    fn walk_mut<'a>(&'a mut self, walker: &mut impl AstWalkerMut<'a>) {
        self.walk_mut_children(walker);
    }
    fn walk_children<'a>(&'a self, walker: &mut impl AstWalker<'a>) {
        self.0.walk(walker);
        self.1.walk(walker);
    }
    fn walk_mut_children<'a>(&'a mut self, walker: &mut impl AstWalkerMut<'a>) {
        self.0.walk_mut(walker);
        self.1.walk_mut(walker);
    }
}
walk_struct!(String, walk_string, walk_mut_string);
walk_struct!(bool, walk_bool, walk_mut_bool);
walk_struct!(BigInt, walk_big_int, walk_mut_big_int);


pub trait AstVisitable {
    fn visit<'a>(self, walker: &mut impl AstWalker<'a>);
    fn visit_mut<'a>(self, walker: &mut impl AstWalkerMut<'a>);
}

macro_rules! visit {
    ($name:ident, $walk:ident) => {
        impl AstVisitable for $name {
            fn visit<'a>(self, walker: &mut impl AstWalker<'a>) {
                walker.$walk(self);
            }
            fn visit_mut<'a>(self, walker: &mut impl AstWalkerMut<'a>) {
                walker.$walk(self);
            }
        }
    };
}

visit!(LocalDefId, visit_local_def_id);
