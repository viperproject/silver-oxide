use crate::ast::*;

/// Mutable visitors for Silver Oxide
///
/// These visitors will recursively traverse the given AST, providing write access
/// at each level. For immutable access see [`Visitor`].
///
/// To implement a visitor, implement the relevant methods, handle the cases of interest
/// and call the appropriate `super_mut_visit_*` method to recursively traverse the type.
pub trait MutVisitor: Sized {
    fn mut_visit_exp(&mut self, exp: &Exp) {
        super_mut_visit_exp(self, exp)
    }

    fn mut_visit_type(&mut self, ty: &Type) {
        super_mut_visit_type(self, ty)
    }

    fn mut_visit_statement(&mut self, stmt: &Statement) {
        super_mut_visit_statement(self, stmt)
    }

    fn mut_visit_declaration(&mut self, decl: &Declaration) {
        super_mut_visit_declaration(self, decl)
    }
}

pub fn super_mut_visit_exp<V: MutVisitor>(v: &mut V, exp: &Exp) {
    match exp {
        Exp::True => (),
        Exp::False => (),
        Exp::Int(_) => (),
        Exp::Null => (),
        Exp::Result => (),
        Exp::At(_, e) => v.mut_visit_exp(e),
        Exp::Old(_, e) => v.mut_visit_exp(e),
        Exp::Lhs(e) => v.mut_visit_exp(e),
        Exp::None => (),
        Exp::Write => (),
        Exp::Epsilon => (),
        Exp::Wildcard => (),
        Exp::Ascribe(e, t) => {
            v.mut_visit_exp(e);
            v.mut_visit_type(t);
        }
        Exp::Perm(loc_access) => super_mut_visit_loc_access(v, loc_access),
        Exp::Unfolding(acc_exp, e) => {
            super_mut_visit_acc_exp(v, acc_exp);
            v.mut_visit_exp(e);
        }
        Exp::Folding(acc_exp, e) => {
            super_mut_visit_acc_exp(v, acc_exp);
            v.mut_visit_exp(e);
        }
        Exp::Forall(bindings, triggers, e) => {
            for (_, t) in bindings {
                v.mut_visit_type(t);
            }
            for trigger in triggers {
                super_mut_visit_trigger(v, trigger);
            }
            v.mut_visit_exp(e);
        }
        Exp::Exists(bindings, triggers, e) => {
            for (_, t) in bindings {
                v.mut_visit_type(t);
            }
            for trigger in triggers {
                super_mut_visit_trigger(v, trigger);
            }
            v.mut_visit_exp(e);
        }
        Exp::SeqConstructor(seq_constructor) => super_mut_visit_seq_constructor(v, seq_constructor),
        Exp::SetConstructor(set_constructor) => super_mut_visit_set_constructor(v, set_constructor),
        Exp::MapConstructor(map_constructor) => super_mut_visit_map_constructor(v, map_constructor),
        Exp::ForPerm(bindings, res_access, e) => {
            for (_, t) in bindings {
                v.mut_visit_type(t);
            }
            super_mut_visit_res_access(v, res_access);
            v.mut_visit_exp(e);
        }
        Exp::Acc(acc_exp) => super_mut_visit_acc_exp(v, acc_exp),
        Exp::Index(e, index_op) => {
            v.mut_visit_exp(e);
            super_mut_visit_index_op(v, index_op);
        }
        Exp::FuncApp(e, args) => {
            v.mut_visit_exp(e);
            for arg in args {
                v.mut_visit_exp(arg);
            }
        }
        Exp::Ident(_) => (),
        Exp::BinOp(_, e1, e2) => {
            v.mut_visit_exp(e1);
            v.mut_visit_exp(e2);
        }
        Exp::Ternary(e1, e2, e3) => {
            v.mut_visit_exp(e1);
            v.mut_visit_exp(e2);
            v.mut_visit_exp(e3);
        }
        Exp::Field(e, _) => v.mut_visit_exp(e),
        Exp::Neg(e) => v.mut_visit_exp(e),
        Exp::Not(e) => v.mut_visit_exp(e),
        Exp::InhaleExhale(e1, e2) => {
            v.mut_visit_exp(e1);
            v.mut_visit_exp(e2);
        }
        Exp::Applying(e1, e2) => {
            v.mut_visit_exp(e1);
            v.mut_visit_exp(e2);
        }
        Exp::Packaging(e1, e2) => {
            v.mut_visit_exp(e1);
            v.mut_visit_exp(e2);
        }
        Exp::Abs(e) => v.mut_visit_exp(e),
        Exp::LetIn(_, e1, e2) => {
            v.mut_visit_exp(e1);
            v.mut_visit_exp(e2);
        }
    }
}

pub fn super_mut_visit_trigger<V: MutVisitor>(v: &mut V, trigger: &Trigger) {
    for exp in &trigger.exp {
        v.mut_visit_exp(exp);
    }
}

pub fn super_mut_visit_index_op<V: MutVisitor>(v: &mut V, index_op: &IndexOp) {
    match index_op {
        IndexOp::Index(e) | IndexOp::LowerBound(e) | IndexOp::UpperBound(e) => v.mut_visit_exp(e),
        IndexOp::Range(e1, e2) | IndexOp::Assign(e1, e2) => {
            v.mut_visit_exp(e1);
            v.mut_visit_exp(e2);
        }
    }
}

pub fn super_mut_visit_set_constructor<V: MutVisitor>(v: &mut V, set_constructor: &SetConstructor) {
    match set_constructor {
        SetConstructor::Empty(t) | SetConstructor::MultisetEmpty(t) => v.mut_visit_type(t),
        SetConstructor::NonEmpty(exps) | SetConstructor::MultisetNonEmpty(exps) => {
            for exp in exps {
                v.mut_visit_exp(exp);
            }
        }
    }
}

pub fn super_mut_visit_seq_constructor<V: MutVisitor>(v: &mut V, seq_constructor: &SeqConstructor) {
    match seq_constructor {
        SeqConstructor::Empty(t) => v.mut_visit_type(t),
        SeqConstructor::NonEmpty(exps) => {
            for exp in exps {
                v.mut_visit_exp(exp);
            }
        }
        SeqConstructor::Range(e1, e2) => {
            v.mut_visit_exp(e1);
            v.mut_visit_exp(e2);
        }
    }
}

pub fn super_mut_visit_map_constructor<V: MutVisitor>(v: &mut V, map_constructor: &MapConstructor) {
    match map_constructor {
        MapConstructor::Empty(t1, t2) => {
            v.mut_visit_type(t1);
            v.mut_visit_type(t2);
        }
        MapConstructor::NonEmpty(pairs) => {
            for (e1, e2) in pairs {
                v.mut_visit_exp(e1);
                v.mut_visit_exp(e2);
            }
        }
    }
}

pub fn super_mut_visit_acc_exp<V: MutVisitor>(v: &mut V, acc_exp: &AccExp) {
    match acc_exp {
        AccExp::Acc(loc_access, exp_opt) => {
            super_mut_visit_loc_access(v, loc_access);
            if let Some(exp) = exp_opt {
                v.mut_visit_exp(exp);
            }
        }
        AccExp::PredicateAccess(exp) => v.mut_visit_exp(exp),
    }
}

pub fn super_mut_visit_res_access<V: MutVisitor>(v: &mut V, res_access: &ResAccess) {
    match res_access {
        ResAccess::Loc(loc_access) => super_mut_visit_loc_access(v, loc_access),
        ResAccess::Exp(exp) => v.mut_visit_exp(exp),
    }
}

pub fn super_mut_visit_block<V: MutVisitor>(v: &mut V, block: &Block) {
    for statement in &block.statements {
        v.mut_visit_statement(statement);
    }
}

pub fn super_mut_visit_invariant<V: MutVisitor>(v: &mut V, invariant: &Invariant) {
    v.mut_visit_exp(&invariant.0);
}

pub fn super_mut_visit_loc_access<V: MutVisitor>(v: &mut V, loc_access: &LocAccess) {
    v.mut_visit_exp(&loc_access.loc);
}

pub fn super_mut_visit_declaration<V: MutVisitor>(v: &mut V, decl: &Declaration) {
    match decl {
        Declaration::Import(_) => (),
        Declaration::Define(define) => super_mut_visit_define(v, define),
        Declaration::Domain(domain) => super_mut_visit_domain(v, domain),
        Declaration::Field(field) => super_mut_visit_field(v, field),
        Declaration::Function(function) => super_mut_visit_function(v, function),
        Declaration::Predicate(predicate) => super_mut_visit_predicate(v, predicate),
        Declaration::Method(method) => super_mut_visit_method(v, method),
        Declaration::Adt(adt) => super_mut_visit_adt(v, adt),
    }
}

pub fn super_mut_visit_exp_or_block<V: MutVisitor>(v: &mut V, exp_or_block: &ExpOrBlock) {
    match exp_or_block {
        ExpOrBlock::Exp(exp) => v.mut_visit_exp(exp),
        ExpOrBlock::Block(block) => super_mut_visit_block(v, block),
    }
}

pub fn super_mut_visit_define<V: MutVisitor>(v: &mut V, define: &Define) {
    super_mut_visit_exp_or_block(v, &define.body);
}

pub fn super_mut_visit_domain<V: MutVisitor>(v: &mut V, domain: &Domain) {
    for element in &domain.elements {
        super_mut_visit_domain_element(v, element);
    }
}

pub fn super_mut_visit_field<V: MutVisitor>(v: &mut V, field: &Field) {
    for (_, ty) in &field.fields {
        v.mut_visit_type(ty);
    }
}

pub fn super_mut_visit_function<V: MutVisitor>(v: &mut V, function: &Function) {
    super_mut_visit_signature(v, &function.signature);
    super_mut_visit_contract(v, &function.contract);
    if let Some(body) = &function.body {
        v.mut_visit_exp(body);
    }
}

pub fn super_mut_visit_predicate<V: MutVisitor>(v: &mut V, predicate: &Predicate) {
    super_mut_visit_signature(v, &predicate.signature);
    if let Some(body) = &predicate.body {
        v.mut_visit_exp(body);
    }
}

pub fn super_mut_visit_method<V: MutVisitor>(v: &mut V, method: &Method) {
    super_mut_visit_signature(v, &method.signature);
    super_mut_visit_contract(v, &method.contract);
    if let Some(body) = &method.body {
        super_mut_visit_block(v, body);
    }
}

pub fn super_mut_visit_adt<V: MutVisitor>(v: &mut V, adt: &Adt) {
    for arg in &adt.args {
        v.mut_visit_type(arg);
    }
    for variant in &adt.variants {
        super_mut_visit_variant(v, variant);
    }
}

pub fn super_mut_visit_domain_element<V: MutVisitor>(v: &mut V, element: &DomainElement) {
    match element {
        DomainElement::DomainFunction(domain_function) => {
            super_mut_visit_signature(v, &domain_function.signature)
        }
        DomainElement::Axiom(axiom) => v.mut_visit_exp(&axiom.exp),
    }
}

pub fn super_mut_visit_domain_function<V: MutVisitor>(v: &mut V, domain_function: &DomainFunction) {
    super_mut_visit_signature(v, &domain_function.signature);
}

pub fn super_mut_visit_arg_or_type<V: MutVisitor>(v: &mut V, arg_or_type: &ArgOrType) {
    match arg_or_type {
        ArgOrType::Arg((_, ty)) | ArgOrType::Type(ty) => v.mut_visit_type(ty),
    }
}

pub fn super_mut_visit_contract<V: MutVisitor>(v: &mut V, contract: &Contract) {
    for precondition in &contract.preconditions {
        v.mut_visit_exp(precondition);
    }
    for postcondition in &contract.postconditions {
        v.mut_visit_exp(postcondition);
    }
    for decreases in &contract.decreases {
        super_mut_visit_decreases(v, decreases);
    }
}
pub fn super_mut_visit_decreases<V: MutVisitor>(v: &mut V, decreases: &Decreases) {
    if let Some(DecreasesKind::Exp(exps)) = &decreases.kind {
        for exp in exps {
            v.mut_visit_exp(exp);
        }
    }
    if let Some(guard) = &decreases.guard {
        v.mut_visit_exp(guard);
    }
}

pub fn super_mut_visit_signature<V: MutVisitor>(v: &mut V, signature: &Signature) {
    for arg in &signature.args {
        super_mut_visit_arg_or_type(v, arg);
    }
    for ret in &signature.ret {
        super_mut_visit_arg_or_type(v, ret);
    }
}

pub fn super_mut_visit_variant<V: MutVisitor>(v: &mut V, variant: &Variant) {
    for (_, ty) in &variant.fields {
        v.mut_visit_type(ty);
    }
}

pub fn super_mut_visit_while_spec<V: MutVisitor>(v: &mut V, while_spec: &WhileSpec) {
    match while_spec {
        WhileSpec::Inv(invariant) => v.mut_visit_exp(&invariant.0),

        WhileSpec::Dec(decreases) => super_mut_visit_decreases(v, decreases),
    }
}

pub fn super_mut_visit_statement<V: MutVisitor>(v: &mut V, stmt: &Statement) {
    match stmt {
        Statement::Assert(exp)
        | Statement::Refute(exp)
        | Statement::Assume(exp)
        | Statement::Inhale(exp)
        | Statement::Exhale(exp) => v.mut_visit_exp(exp),
        Statement::Fold(acc_exp) | Statement::Unfold(acc_exp) => {
            super_mut_visit_acc_exp(v, acc_exp)
        }
        Statement::Goto(_) => (),
        Statement::Label(_, invariants) => {
            for inv in invariants {
                super_mut_visit_invariant(v, inv);
            }
        }
        Statement::Havoc(loc_access) => super_mut_visit_loc_access(v, loc_access),
        Statement::QuasiHavoc(exp_opt, exp) => {
            if let Some(e) = exp_opt {
                v.mut_visit_exp(e);
            }
            v.mut_visit_exp(exp);
        }
        Statement::QuasiHavocAll(bindings, exp_opt, exp) => {
            for (_, ty) in bindings {
                v.mut_visit_type(ty);
            }
            if let Some(e) = exp_opt {
                v.mut_visit_exp(e);
            }
            v.mut_visit_exp(exp);
        }
        Statement::Var(bindings, exp_opt) => {
            for (_, ty) in bindings {
                v.mut_visit_type(ty);
            }
            if let Some(e) = exp_opt {
                v.mut_visit_exp(e);
            }
        }
        Statement::While(exp, while_specs, block) => {
            v.mut_visit_exp(exp);
            for spec in while_specs {
                super_mut_visit_while_spec(v, spec);
            }
            super_mut_visit_block(v, block);
        }
        Statement::If(exp, block, else_ifs, else_block) => {
            v.mut_visit_exp(exp);
            super_mut_visit_block(v, block);
            for (else_if_exp, else_if_block) in else_ifs {
                v.mut_visit_exp(else_if_exp);
                super_mut_visit_block(v, else_if_block);
            }
            if let Some(else_b) = else_block {
                super_mut_visit_block(v, else_b);
            }
        }
        Statement::Wand(_, exp) => {
            v.mut_visit_exp(exp);
        }
        Statement::Package(exp, block_opt) => {
            v.mut_visit_exp(exp);
            if let Some(block) = block_opt {
                super_mut_visit_block(v, block);
            }
        }
        Statement::Apply(exp) => v.mut_visit_exp(exp),
        Statement::Assign(exps, exp) => {
            for e in exps {
                v.mut_visit_exp(e);
            }
            v.mut_visit_exp(exp);
        }
        Statement::Fresh(_) => {}
        Statement::Constraining(_, block) => {
            super_mut_visit_block(v, block);
        }
        Statement::Block(block) => super_mut_visit_block(v, block),
        Statement::New(_, _) => {}
    }
}

pub fn super_mut_visit_type<V: MutVisitor>(v: &mut V, ty: &Type) {
    match ty {
        Type::Int | Type::Bool | Type::Perm | Type::Ref | Type::Rational => {
            // These are primitive types, so we don't need to visit anything
        }
        Type::Seq(inner_type) | Type::Set(inner_type) => {
            super_mut_visit_type(v, inner_type);
        }
        Type::Map(key_type, value_type) => {
            super_mut_visit_type(v, key_type);
            super_mut_visit_type(v, value_type);
        }
        Type::User(_, type_args) => {
            for arg in type_args {
                super_mut_visit_type(v, arg);
            }
        }
    }
}
