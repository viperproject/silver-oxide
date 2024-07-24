use crate::ast::*;

/// Immutable visitors for Silver Oxide
///
/// These visitors will recursively traverse the given AST, providing read-only access
/// at each level. For mutable access see [`MutVisitor`].
///
/// To implement a visitor, implement the relevant methods, handle the cases of interest
/// and call the appropriate `super_visit_*` method to recursively traverse the type.
pub trait Visitor: Sized {
    fn visit_exp(&mut self, exp: &Exp) {
        super_visit_exp(self, exp)
    }

    fn visit_type(&mut self, ty: &Type) {
        super_visit_type(self, ty)
    }

    fn visit_statement(&mut self, stmt: &Statement) {
        super_visit_statement(self, stmt)
    }

    fn visit_declaration(&mut self, decl: &Declaration) {
        super_visit_declaration(self, decl)
    }
}

pub fn super_visit_exp<V: Visitor>(v: &mut V, exp: &Exp) {
    match exp {
        Exp::True => (),
        Exp::False => (),
        Exp::Int(_) => (),
        Exp::Null => (),
        Exp::Result => (),
        Exp::At(_, e) => v.visit_exp(e),
        Exp::Old(_, e) => v.visit_exp(e),
        Exp::Lhs(e) => v.visit_exp(e),
        Exp::None => (),
        Exp::Write => (),
        Exp::Epsilon => (),
        Exp::Wildcard => (),
        Exp::Ascribe(e, t) => {
            v.visit_exp(e);
            v.visit_type(t);
        }
        Exp::Perm(loc_access) => super_visit_loc_access(v, loc_access),
        Exp::Unfolding(acc_exp, e) => {
            super_visit_acc_exp(v, acc_exp);
            v.visit_exp(e);
        }
        Exp::Folding(acc_exp, e) => {
            super_visit_acc_exp(v, acc_exp);
            v.visit_exp(e);
        }
        Exp::Forall(bindings, triggers, e) => {
            for (_, t) in bindings {
                v.visit_type(t);
            }
            for trigger in triggers {
                super_visit_trigger(v, trigger);
            }
            v.visit_exp(e);
        }
        Exp::Exists(bindings, triggers, e) => {
            for (_, t) in bindings {
                v.visit_type(t);
            }
            for trigger in triggers {
                super_visit_trigger(v, trigger);
            }
            v.visit_exp(e);
        }
        Exp::SeqConstructor(seq_constructor) => super_visit_seq_constructor(v, seq_constructor),
        Exp::SetConstructor(set_constructor) => super_visit_set_constructor(v, set_constructor),
        Exp::MapConstructor(map_constructor) => super_visit_map_constructor(v, map_constructor),
        Exp::ForPerm(bindings, res_access, e) => {
            for (_, t) in bindings {
                v.visit_type(t);
            }
            super_visit_res_access(v, res_access);
            v.visit_exp(e);
        }
        Exp::Acc(acc_exp) => super_visit_acc_exp(v, acc_exp),
        Exp::Index(e, index_op) => {
            v.visit_exp(e);
            super_visit_index_op(v, index_op);
        }
        Exp::FuncApp(e, args) => {
            v.visit_exp(e);
            for arg in args {
                v.visit_exp(arg);
            }
        }
        Exp::Ident(_) => (),
        Exp::BinOp(_, e1, e2) => {
            v.visit_exp(e1);
            v.visit_exp(e2);
        }
        Exp::Ternary(e1, e2, e3) => {
            v.visit_exp(e1);
            v.visit_exp(e2);
            v.visit_exp(e3);
        }
        Exp::Field(e, _) => v.visit_exp(e),
        Exp::Neg(e) => v.visit_exp(e),
        Exp::Not(e) => v.visit_exp(e),
        Exp::InhaleExhale(e1, e2) => {
            v.visit_exp(e1);
            v.visit_exp(e2);
        }
        Exp::Applying(e1, e2) => {
            v.visit_exp(e1);
            v.visit_exp(e2);
        }
        Exp::Packaging(e1, e2) => {
            v.visit_exp(e1);
            v.visit_exp(e2);
        }
        Exp::Abs(e) => v.visit_exp(e),
        Exp::LetIn(_, e1, e2) => {
            v.visit_exp(e1);
            v.visit_exp(e2);
        }
    }
}

pub fn super_visit_trigger<V: Visitor>(v: &mut V, trigger: &Trigger) {
    for exp in &trigger.exp {
        v.visit_exp(exp);
    }
}

pub fn super_visit_index_op<V: Visitor>(v: &mut V, index_op: &IndexOp) {
    match index_op {
        IndexOp::Index(e) | IndexOp::LowerBound(e) | IndexOp::UpperBound(e) => v.visit_exp(e),
        IndexOp::Range(e1, e2) | IndexOp::Assign(e1, e2) => {
            v.visit_exp(e1);
            v.visit_exp(e2);
        }
    }
}

pub fn super_visit_set_constructor<V: Visitor>(v: &mut V, set_constructor: &SetConstructor) {
    match set_constructor {
        SetConstructor::Empty(t) | SetConstructor::MultisetEmpty(t) => v.visit_type(t),
        SetConstructor::NonEmpty(exps) | SetConstructor::MultisetNonEmpty(exps) => {
            for exp in exps {
                v.visit_exp(exp);
            }
        }
    }
}

pub fn super_visit_seq_constructor<V: Visitor>(v: &mut V, seq_constructor: &SeqConstructor) {
    match seq_constructor {
        SeqConstructor::Empty(t) => v.visit_type(t),
        SeqConstructor::NonEmpty(exps) => {
            for exp in exps {
                v.visit_exp(exp);
            }
        }
        SeqConstructor::Range(e1, e2) => {
            v.visit_exp(e1);
            v.visit_exp(e2);
        }
    }
}

pub fn super_visit_map_constructor<V: Visitor>(v: &mut V, map_constructor: &MapConstructor) {
    match map_constructor {
        MapConstructor::Empty(t1, t2) => {
            v.visit_type(t1);
            v.visit_type(t2);
        }
        MapConstructor::NonEmpty(pairs) => {
            for (e1, e2) in pairs {
                v.visit_exp(e1);
                v.visit_exp(e2);
            }
        }
    }
}

pub fn super_visit_acc_exp<V: Visitor>(v: &mut V, acc_exp: &AccExp) {
    match acc_exp {
        AccExp::Acc(loc_access, exp_opt) => {
            super_visit_loc_access(v, loc_access);
            if let Some(exp) = exp_opt {
                v.visit_exp(exp);
            }
        }
        AccExp::PredicateAccess(exp) => v.visit_exp(exp),
    }
}

pub fn super_visit_res_access<V: Visitor>(v: &mut V, res_access: &ResAccess) {
    match res_access {
        ResAccess::Loc(loc_access) => super_visit_loc_access(v, loc_access),
        ResAccess::Exp(exp) => v.visit_exp(exp),
    }
}

pub fn super_visit_block<V: Visitor>(v: &mut V, block: &Block) {
    for statement in &block.statements {
        v.visit_statement(statement);
    }
}

pub fn super_visit_invariant<V: Visitor>(v: &mut V, invariant: &Invariant) {
    v.visit_exp(&invariant.0);
}

pub fn super_visit_loc_access<V: Visitor>(v: &mut V, loc_access: &LocAccess) {
    v.visit_exp(&loc_access.loc);
}

pub fn super_visit_declaration<V: Visitor>(v: &mut V, decl: &Declaration) {
    match decl {
        Declaration::Import(_) => (),
        Declaration::Define(define) => super_visit_define(v, define),
        Declaration::Domain(domain) => super_visit_domain(v, domain),
        Declaration::Field(field) => super_visit_field(v, field),
        Declaration::Function(function) => super_visit_function(v, function),
        Declaration::Predicate(predicate) => super_visit_predicate(v, predicate),
        Declaration::Method(method) => super_visit_method(v, method),
        Declaration::Adt(adt) => super_visit_adt(v, adt),
    }
}

pub fn super_visit_exp_or_block<V: Visitor>(v: &mut V, exp_or_block: &ExpOrBlock) {
    match exp_or_block {
        ExpOrBlock::Exp(exp) => v.visit_exp(exp),
        ExpOrBlock::Block(block) => super_visit_block(v, block),
    }
}

pub fn super_visit_define<V: Visitor>(v: &mut V, define: &Define) {
    super_visit_exp_or_block(v, &define.body);
}

pub fn super_visit_domain<V: Visitor>(v: &mut V, domain: &Domain) {
    for element in &domain.elements {
        super_visit_domain_element(v, element);
    }
}

pub fn super_visit_field<V: Visitor>(v: &mut V, field: &Field) {
    for (_, ty) in &field.fields {
        v.visit_type(ty);
    }
}

pub fn super_visit_function<V: Visitor>(v: &mut V, function: &Function) {
    super_visit_signature(v, &function.signature);
    super_visit_contract(v, &function.contract);
    if let Some(body) = &function.body {
        v.visit_exp(body);
    }
}

pub fn super_visit_predicate<V: Visitor>(v: &mut V, predicate: &Predicate) {
    super_visit_signature(v, &predicate.signature);
    if let Some(body) = &predicate.body {
        v.visit_exp(body);
    }
}

pub fn super_visit_method<V: Visitor>(v: &mut V, method: &Method) {
    super_visit_signature(v, &method.signature);
    super_visit_contract(v, &method.contract);
    if let Some(body) = &method.body {
        super_visit_block(v, body);
    }
}

pub fn super_visit_adt<V: Visitor>(v: &mut V, adt: &Adt) {
    for arg in &adt.args {
        v.visit_type(arg);
    }
    for variant in &adt.variants {
        super_visit_variant(v, variant);
    }
}

pub fn super_visit_domain_element<V: Visitor>(v: &mut V, element: &DomainElement) {
    match element {
        DomainElement::DomainFunction(domain_function) => {
            super_visit_signature(v, &domain_function.signature)
        }
        DomainElement::Axiom(axiom) => v.visit_exp(&axiom.exp),
    }
}

pub fn super_visit_domain_function<V: Visitor>(v: &mut V, domain_function: &DomainFunction) {
    super_visit_signature(v, &domain_function.signature);
}

pub fn super_visit_arg_or_type<V: Visitor>(v: &mut V, arg_or_type: &ArgOrType) {
    match arg_or_type {
        ArgOrType::Arg((_, ty)) | ArgOrType::Type(ty) => v.visit_type(ty),
    }
}

pub fn super_visit_contract<V: Visitor>(v: &mut V, contract: &Contract) {
    for precondition in &contract.preconditions {
        v.visit_exp(precondition);
    }
    for postcondition in &contract.postconditions {
        v.visit_exp(postcondition);
    }
    for decreases in &contract.decreases {
        super_visit_decreases(v, decreases);
    }
}
pub fn super_visit_decreases<V: Visitor>(v: &mut V, decreases: &Decreases) {
    if let Some(DecreasesKind::Exp(exps)) = &decreases.kind {
        for exp in exps {
            v.visit_exp(exp);
        }
    }
    if let Some(guard) = &decreases.guard {
        v.visit_exp(guard);
    }
}

pub fn super_visit_signature<V: Visitor>(v: &mut V, signature: &Signature) {
    for arg in &signature.args {
        super_visit_arg_or_type(v, arg);
    }
    for ret in &signature.ret {
        super_visit_arg_or_type(v, ret);
    }
}

pub fn super_visit_variant<V: Visitor>(v: &mut V, variant: &Variant) {
    for (_, ty) in &variant.fields {
        v.visit_type(ty);
    }
}

pub fn super_visit_while_spec<V: Visitor>(v: &mut V, while_spec: &WhileSpec) {
    match while_spec {
        WhileSpec::Inv(invariant) => v.visit_exp(&invariant.0),

        WhileSpec::Dec(decreases) => super_visit_decreases(v, decreases),
    }
}

pub fn super_visit_statement<V: Visitor>(v: &mut V, stmt: &Statement) {
    match stmt {
        Statement::Assert(exp)
        | Statement::Refute(exp)
        | Statement::Assume(exp)
        | Statement::Inhale(exp)
        | Statement::Exhale(exp) => v.visit_exp(exp),
        Statement::Fold(acc_exp) | Statement::Unfold(acc_exp) => super_visit_acc_exp(v, acc_exp),
        Statement::Goto(_) => (),
        Statement::Label(_, invariants) => {
            for inv in invariants {
                super_visit_invariant(v, inv);
            }
        }
        Statement::Havoc(loc_access) => super_visit_loc_access(v, loc_access),
        Statement::QuasiHavoc(exp_opt, exp) => {
            if let Some(e) = exp_opt {
                v.visit_exp(e);
            }
            v.visit_exp(exp);
        }
        Statement::QuasiHavocAll(bindings, exp_opt, exp) => {
            for (_, ty) in bindings {
                v.visit_type(ty);
            }
            if let Some(e) = exp_opt {
                v.visit_exp(e);
            }
            v.visit_exp(exp);
        }
        Statement::Var(bindings, exp_opt) => {
            for (_, ty) in bindings {
                v.visit_type(ty);
            }
            if let Some(e) = exp_opt {
                v.visit_exp(e);
            }
        }
        Statement::While(exp, while_specs, block) => {
            v.visit_exp(exp);
            for spec in while_specs {
                super_visit_while_spec(v, spec);
            }
            super_visit_block(v, block);
        }
        Statement::If(exp, block, else_ifs, else_block) => {
            v.visit_exp(exp);
            super_visit_block(v, block);
            for (else_if_exp, else_if_block) in else_ifs {
                v.visit_exp(else_if_exp);
                super_visit_block(v, else_if_block);
            }
            if let Some(else_b) = else_block {
                super_visit_block(v, else_b);
            }
        }
        Statement::Wand(_, exp) => {
            v.visit_exp(exp);
        }
        Statement::Package(exp, block_opt) => {
            v.visit_exp(exp);
            if let Some(block) = block_opt {
                super_visit_block(v, block);
            }
        }
        Statement::Apply(exp) => v.visit_exp(exp),
        Statement::Assign(exps, exp) => {
            for e in exps {
                v.visit_exp(e);
            }
            v.visit_exp(exp);
        }
        Statement::Fresh(_) => {}
        Statement::Constraining(_, block) => {
            super_visit_block(v, block);
        }
        Statement::Block(block) => super_visit_block(v, block),
        Statement::New(_, _) => {}
    }
}

pub fn super_visit_type<V: Visitor>(v: &mut V, ty: &Type) {
    match ty {
        Type::Int | Type::Bool | Type::Perm | Type::Ref | Type::Rational => {
            // These are primitive types, so we don't need to visit anything
        }
        Type::Seq(inner_type) | Type::Set(inner_type) => {
            super_visit_type(v, inner_type);
        }
        Type::Map(key_type, value_type) => {
            super_visit_type(v, key_type);
            super_visit_type(v, value_type);
        }
        Type::User(_, type_args) => {
            for arg in type_args {
                super_visit_type(v, arg);
            }
        }
    }
}
