use num_bigint::BigInt;

use crate::ast::*;

macro_rules! walk_children {
    ($name:ident, $l:lifetime, $ty:ident) => {
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
    walk_children!(walk_declaration, 'a, Declaration);
    walk_children!(walk_axiom, 'a, Axiom);
    walk_children!(walk_import, 'a, Import);
    walk_children!(walk_define, 'a, Define);
    walk_children!(walk_expr_or_block, 'a, ExpOrBlock);
    walk_children!(walk_arg_or_type, 'a, ArgOrType);
    walk_children!(walk_expr, 'a, Exp);
    walk_children!(walk_const, 'a, Const);
    walk_children!(walk_set_constructor, 'a, SetConstructor);
    walk_children!(walk_seq_constructor, 'a, SeqConstructor);
    walk_children!(walk_map_constructor, 'a, MapConstructor);
    walk_children!(walk_acc_exp, 'a, AccExp);
    walk_children!(walk_bin_op, 'a, BinOp);
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
    walk_children!(walk_domain_element, 'a, DomainElement);

    walk_children!(walk_string, 'a, String);
    walk_children!(walk_bool, 'a, bool);
    walk_children!(walk_big_int, 'a, BigInt);
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
    walk_mut_children!(walk_mut_declaration, 'a, Declaration);
    walk_mut_children!(walk_mut_axiom, 'a, Axiom);
    walk_mut_children!(walk_mut_import, 'a, Import);
    walk_mut_children!(walk_mut_define, 'a, Define);
    walk_mut_children!(walk_mut_expr_or_block, 'a, ExpOrBlock);
    walk_mut_children!(walk_mut_arg_or_type, 'a, ArgOrType);
    walk_mut_children!(walk_mut_expr, 'a, Exp);
    walk_mut_children!(walk_mut_const, 'a, Const);
    walk_mut_children!(walk_mut_set_constructor, 'a, SetConstructor);
    walk_mut_children!(walk_mut_seq_constructor, 'a, SeqConstructor);
    walk_mut_children!(walk_mut_map_constructor, 'a, MapConstructor);
    walk_mut_children!(walk_mut_acc_exp, 'a, AccExp);
    walk_mut_children!(walk_mut_bin_op, 'a, BinOp);
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
    walk_mut_children!(walk_mut_domain_element, 'a, DomainElement);

    walk_mut_children!(walk_mut_string, 'a, String);
    walk_mut_children!(walk_mut_bool, 'a, bool);
    walk_mut_children!(walk_mut_big_int, 'a, BigInt);
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

walk_struct!(Program, walk_program, walk_mut_program, 0);
walk_enum!(PrePostDec, walk_pre_post_dec, walk_mut_pre_post_dec, Pre(e), Post(e), Decreases(d));
walk_struct!(Decreases, walk_decreases, walk_mut_decreases, kind, guard);
walk_enum!(DecreasesKind, walk_decreases_kind, walk_mut_decreases_kind, Star, Underscore, Exp(e));
walk_struct!(Ident, walk_ident, walk_mut_ident, 0);
walk_enum!(Declaration, walk_declaration, walk_mut_declaration, Import(i), Define(d), Domain(d), Field(f), Function(f), Predicate(p), Method(m), Adt(a));
walk_struct!(Axiom, walk_axiom, walk_mut_axiom, name, exp);
walk_struct!(Import, walk_import, walk_mut_import, path);
walk_struct!(Define, walk_define, walk_mut_define, name, args, body);
walk_enum!(ExpOrBlock, walk_expr_or_block, walk_mut_expr_or_block, Exp(e), Block(b));
walk_enum!(ArgOrType, walk_arg_or_type, walk_mut_arg_or_type, Arg(a), Type(t));
walk_enum!(Exp, walk_expr, walk_mut_expr, Const(c), Result, At(i, e), Old(i, e), Lhs(e), Ascribe(e, t), Perm(l), Unfolding(a, e), Folding(a, e), Applying(w, e), Packaging(w, e), Forall(vars, triggers, e), Exists(vars, triggers, e), SeqConstructor(s), SetConstructor(s), MapConstructor(s), Abs(e), LetIn(i, e1, e2), ForPerm(vars, p, e), Acc(a), FuncApp(i, args), Ident(i), BinOp(op, l, r), Ternary(c, t, e), Field(e, i), Index(e, op), Neg(e), Not(e), InhaleExhale(e1, e2));
walk_enum!(Const, walk_const, walk_mut_const, Bool(b), Int(i), Null, None, Write, Epsilon, Wildcard);
walk_enum!(SetConstructor, walk_set_constructor, walk_mut_set_constructor, Empty(t), NonEmpty(es), MultisetEmpty(t), MultisetNonEmpty(es));
walk_enum!(SeqConstructor, walk_seq_constructor, walk_mut_seq_constructor, Empty(t), NonEmpty(es), Range(e1, e2));
walk_enum!(MapConstructor, walk_map_constructor, walk_mut_map_constructor, Empty(t1, t2), NonEmpty(es));
walk_struct!(AccExp, walk_acc_exp, walk_mut_acc_exp, acc, perm);
walk_enum!(BinOp, walk_bin_op, walk_mut_bin_op, Implies, Iff, And, Or, Eq, Neq, Lt, Le, Gt, Ge, In, Plus, Minus, Mult, Div, Mod, PermDiv, Union, SetMinus, Intersection, Subset, Concat, MagicWand);
walk_struct!(Trigger, walk_trigger, walk_mut_trigger, exp);
walk_enum!(ResAccess, walk_res_access, walk_mut_res_access, Loc(l), Exp(e));
walk_struct!(Block, walk_block, walk_mut_block, statements);
walk_enum!(Statement, walk_statement, walk_mut_statement, Assert(e), Refute(e), Assume(e), Inhale(e), Exhale(e), Fold(e), Unfold(e), Goto(i), Label(i, invariants), Havoc(l), QuasiHavoc(e1, e2), QuasiHavocAll(vars, e1, e2), Var(vars, e), While(e, specs, b), If(e, b, branches, else_), Wand(i, e), Package(e, b), Apply(e), Assign(lhs, rhs), Fresh(vars), Constraining(vars, b), Block(b), New(i, star_or_names));
walk_enum!(StarOrNames, walk_star_or_names, walk_mut_star_or_names, Star, Names(names));
walk_enum!(IndexOp, walk_index_op, walk_mut_index_op, Index(e), LowerBound(e), UpperBound(e), Range(e1, e2), Assign(e1, e2));
walk_struct!(Invariant, walk_invariant, walk_mut_invariant, 0);
walk_enum!(WhileSpec, walk_while_spec, walk_mut_while_spec, Inv(i), Dec(d));
walk_struct!(LocAccess, walk_loc_access, walk_mut_loc_access, loc);
walk_struct!(Field, walk_field, walk_mut_field, fields);
walk_struct!(Domain, walk_domain, walk_mut_domain, name, interpretation, elements);
walk_struct!(Function, walk_function, walk_mut_function, signature, contract, body);
walk_struct!(Contract, walk_contract, walk_mut_contract, precondition, postcondition, decreases);
walk_enum!(DomainElement, walk_domain_element, walk_mut_domain_element, DomainFunction(f), Axiom(a));
walk_struct!(DomainFunction, walk_domain_function, walk_mut_domain_function, unique, signature, interpretation);
walk_struct!(Signature, walk_signature, walk_mut_signature, name, args, ret);
walk_enum!(Type, walk_type, walk_mut_type, Int, Bool, Perm, Ref, Rational, Seq(t), Set(t), Map(t1, t2), User(i, ts));
walk_struct!(Predicate, walk_predicate, walk_mut_predicate, signature, body);
walk_struct!(Method, walk_method, walk_mut_method, signature, contract, body);
walk_struct!(Adt, walk_adt, walk_mut_adt, name, args, variants, derives);
walk_struct!(Variant, walk_variant, walk_mut_variant, name, fields);

impl<T: AstWalkable> AstWalkable for Box<T> {
    fn walk<'a>(&'a self, walker: &mut impl AstWalker<'a>) {
        self.walk_children(walker);
    }
    fn walk_mut<'a>(&'a mut self, walker: &mut impl AstWalkerMut<'a>) {
        self.walk_mut_children(walker);
    }
    fn walk_children<'a>(&'a self, walker: &mut impl AstWalker<'a>) {
        (&**self).walk(walker);
    }
    fn walk_mut_children<'a>(&'a mut self, walker: &mut impl AstWalkerMut<'a>) {
        (&mut **self).walk_mut(walker);
    }
}
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
