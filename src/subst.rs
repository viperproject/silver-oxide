use std::collections::HashMap;

use crate::{
    ast::{Exp, Ident},
    mut_visitor::{
        super_mut_visit_exp, super_mut_visit_res_access, super_mut_visit_trigger, MutVisitor,
    },
};

#[derive(Clone, Default)]
struct Environment {
    scopes: Vec<HashMap<Ident, Exp>>,
}

impl Environment {
    fn push_scope(&mut self, bound: impl IntoIterator<Item = (Ident, Exp)>) {
        self.scopes.push(bound.into_iter().collect());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn lookup(&self, name: &Ident) -> Option<&Exp> {
        for scope in self.scopes.iter().rev() {
            if let Some(term) = scope.get(name) {
                return Some(term);
            }
        }
        None
    }
}

/// Perform capture-avoiding substitution inside an expression
pub fn subst(exp: &mut Exp, subst: HashMap<Ident, Exp>) {
    let env = Environment {
        scopes: vec![subst],
    };

    Subst { env }.mut_visit_exp(exp)
}

struct Subst {
    env: Environment,
}

impl MutVisitor for Subst {
    fn mut_visit_exp(&mut self, exp: &mut Exp) {
        match exp {
            Exp::Ident(id) => {
                if let Some(e) = self.env.lookup(id) {
                    *exp = e.clone()
                }
            }
            Exp::LetIn(v, arg, e) => {
                self.mut_visit_exp(arg);

                self.env.push_scope([(v.clone(), Exp::Ident(v.clone()))]);
                self.mut_visit_exp(e);
                self.env.pop_scope();
            }
            Exp::Forall(vs, t, e) => {
                self.env
                    .push_scope(vs.iter().map(|v| (v.0.clone(), Exp::Ident(v.0.clone()))));
                for t in t {
                    super_mut_visit_trigger(self, t);
                }
                self.mut_visit_exp(e);
                self.env.pop_scope();
            }
            Exp::Exists(vs, t, e) => {
                self.env
                    .push_scope(vs.iter().map(|v| (v.0.clone(), Exp::Ident(v.0.clone()))));
                for t in t {
                    super_mut_visit_trigger(self, t);
                }
                self.mut_visit_exp(e);
                self.env.pop_scope();
            }
            Exp::ForPerm(vs, res, e) => {
                self.env
                    .push_scope(vs.iter().map(|v| (v.0.clone(), Exp::Ident(v.0.clone()))));
                super_mut_visit_res_access(self, res);
                self.mut_visit_exp(e);
                self.env.pop_scope();
            }

            _ => super_mut_visit_exp(self, exp),
        }
    }
}
