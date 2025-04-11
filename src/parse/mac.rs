use crate::{HashMap, HashSet};

use crate::{
    parse::ast::*,
    parse::walk::{AstWalkable, AstWalkerMut},
};

pub struct Macro<'a> {
    pub macros: &'a HashMap<Ident, Define>,
    pub substs: HashMap<Ident, ExpKind>,
    pub seen: &'a mut HashSet<Ident>,
}

impl Macro<'_> {
    pub fn inline_macros(program: &mut Program) {
        let macros = program.get_macros();
        let mut self_ = Macro {
            macros: &macros,
            substs: HashMap::default(),
            seen: &mut HashSet::default(),
        };
        self_.walk_mut_program(program);
    }

    pub fn get_subst(&self, id: &Ident) -> Option<&ExpKind> {
        self.macros
            .get(id)
            .filter(|d| d.args.is_empty())
            .map(|d| match &d.body {
                ExpOrBlock::Exp(e) => &**e,
                _ => panic!("Expected expression macro"),
            })
            .or_else(|| self.substs.get(id))
    }
    pub fn get_call(&self, id: &Ident) -> Option<&Define> {
        self.macros.get(id).filter(|d| !d.args.is_empty())
    }

    pub fn apply_substs(&mut self, id: &Ident, substs: HashMap<Ident, ExpKind>, exp: &mut ExpKind) {
        if !self.seen.insert(id.clone()) {
            panic!("Macro recursion detected on `{}`", id.0);
        }
        let mut self_ = Macro {
            macros: self.macros,
            substs,
            seen: self.seen,
        };
        self_.walk_mut_exp_kind(exp);
        self.seen.swap_remove(id);
    }
}

impl AstWalkerMut<'_> for Macro<'_> {
    fn walk_mut_exp_kind(&mut self, ast: &mut ExpKind) {
        let new = match ast {
            ExpKind::Ident(id) if self.get_subst(id).is_some() => {
                let mut body = self.get_subst(id).unwrap().clone();
                self.apply_substs(id, HashMap::default(), &mut body);
                body
            }
            ExpKind::FuncApp(id, args) if self.get_call(id).is_some() => {
                args.walk_mut(self);
                let d = self.get_call(id).unwrap();
                assert_eq!(
                    d.args.len(),
                    args.len(),
                    "Expected {} arguments, got {}",
                    d.args.len(),
                    args.len()
                );
                let ExpOrBlock::Exp(e) = &d.body else {
                    panic!("Expected expression macro")
                };
                let mut body = (**e).clone();
                let substs = d
                    .args
                    .iter()
                    .map(|a| a.0.clone())
                    .zip(args.iter().map(|e| (**e).clone()))
                    .collect();
                self.apply_substs(id, substs, &mut body);
                body
            }
            ast => return ast.walk_mut_children(self),
        };
        *ast = new;
    }
}

impl Program {
    fn get_macros(&mut self) -> HashMap<Ident, Define> {
        let mut macros = HashMap::default();
        self.0.retain(|d| match d {
            Declaration::Define(m) => {
                macros.insert(m.name.0.clone(), m.clone());
                false
            }
            _ => true,
        });
        macros
    }
}
