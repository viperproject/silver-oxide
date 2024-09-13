use fxhash::{FxHashMap, FxHashSet};

use crate::{ast::*, walk::{AstWalkable, AstWalkerMut}};

pub struct Macro<'a> {
    pub macros: &'a FxHashMap<Ident, Define>,
    pub substs: FxHashMap<Ident, Exp>,
    pub seen: &'a mut FxHashSet<Ident>,
}

impl Macro<'_> {
    pub fn inline_macros(program: &mut Program) {
        let macros = program.get_macros();
        let mut self_ = Macro { macros: &macros, substs: FxHashMap::default(), seen: &mut FxHashSet::default() };
        self_.walk_mut_program(program);
    }

    pub fn get_subst(&self, id: &Ident) -> Option<&Exp> {
        self.macros.get(id).filter(|d| d.args.is_empty()).map(|d|
            match &d.body {
                ExpOrBlock::Exp(e) => e,
                _ => panic!("Expected expression macro"),
            }
        ).or_else(|| self.substs.get(id))
    }
    pub fn get_call(&self, id: &Ident) -> Option<&Define> {
        self.macros.get(id).filter(|d| !d.args.is_empty())
    }

    pub fn apply_substs(&mut self, id: &Ident, substs: FxHashMap<Ident, Exp>, exp: &mut Exp) {
        if !self.seen.insert(id.clone()) {
            panic!("Macro recursion detected on `{}`", id.0);
        }
        let mut self_ = Macro { macros: self.macros, substs, seen: self.seen };
        self_.walk_mut_expr(exp);
        self.seen.remove(id);
    }
}

impl AstWalkerMut<'_> for Macro<'_> {
    fn walk_mut_expr(&mut self, ast: &mut Exp) {
        let new = match ast {
            Exp::Ident(id) if self.get_subst(id).is_some() => {
                let mut body = self.get_subst(id).unwrap().clone();
                self.apply_substs(id, FxHashMap::default(), &mut body);
                body
            }
            Exp::FuncApp(id, args) if self.get_call(id).is_some() => {
                args.walk_mut(self);
                let d = self.get_call(id).unwrap();
                assert_eq!(d.args.len(), args.len(), "Expected {} arguments, got {}", d.args.len(), args.len());
                let ExpOrBlock::Exp(e) = &d.body else {
                    panic!("Expected expression macro")
                };
                let mut body = e.clone();
                let substs = d.args.iter().cloned().zip(args.iter().cloned()).collect();
                self.apply_substs(id, substs, &mut body);
                body
            }
            ast => return ast.walk_mut_children(self),
        };
        *ast = new;
    }
}

impl Program {
    fn get_macros(&mut self) -> FxHashMap<Ident, Define> {
        let mut macros = FxHashMap::default();
        self.0.retain(|d| match d {
            Declaration::Define(m) => {
                macros.insert(m.name.clone(), m.clone());
                false
            }
            _ => true,
        });
        macros
    }
}
