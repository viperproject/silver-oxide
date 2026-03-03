use core::ops::Index;

use super::*;

impl Program {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = (MemberId, &Declaration)> {
        self.0
            .iter()
            .enumerate()
            .map(|(id, decl)| (id.into(), decl))
    }

    pub fn iter_mut(
        &mut self,
    ) -> impl Iterator<Item = (MemberId, &mut Declaration)> {
        self.0
            .iter_mut()
            .enumerate()
            .map(|(id, decl)| (id.into(), decl))
    }
}

impl Declaration {
    pub fn contract(&self) -> Option<&Contract> {
        match self {
            Declaration::Function(f) => Some(&f.contract),
            Declaration::Method(m) => Some(&m.contract),
            _ => None,
        }
    }

    pub fn signature(&self) -> Option<&Signature> {
        use Declaration::*;
        match self {
            Function(f) => Some(&f.signature),
            Method(m) => Some(&m.signature),
            Predicate(p) => Some(&p.signature),
            DomainElement(e) => match &e.kind {
                DomainElementKind::Function(f) => Some(&f.signature),
                DomainElementKind::Axiom(_) => None,
            },
            AdtConstructor(a) => Some(&a.signature),
            Field(f) => Some(&f.0),
            Import(_) | Define(_) | Domain(_) | Adt(_) => None,
        }
    }

    pub fn idn_decl(&self) -> Option<&IdnDecl> {
        use Declaration::*;
        match self {
            Function(f) => Some(&f.signature.name),
            Method(m) => Some(&m.signature.name),
            Predicate(p) => Some(&p.signature.name),
            DomainElement(e) => match &e.kind {
                DomainElementKind::Function(f) => Some(&f.signature.name),
                DomainElementKind::Axiom(a) => a.name.as_ref(),
            },
            Import(_) => None,
            Define(d) => Some(&d.name),
            Domain(d) => Some(&d.name),
            Field(f) => Some(&f.0.name),
            Adt(a) => Some(&a.name),
            AdtConstructor(a) => Some(&a.signature.name),
        }
    }
}

impl Adt {
    pub fn identity(&self) -> Type {
        Type::Domain(self.name.0.clone(), self.params.iter().map(|p| Type::Domain(p.0.clone(), Vec::new())).collect())
    }
}

impl Variant {
    pub fn destructors<'a>(&'a self) -> impl Iterator<Item = &'a IdnDeclTyped> + 'a {
        expect_args(&self.fields)
    }
}

impl AdtConstructor {
    pub fn destructors<'a>(&'a self) -> impl Iterator<Item = &'a IdnDeclTyped> + 'a {
        expect_args(&self.signature.args)
    }

    pub fn adt(&self) -> &Ident {
        let Type::Domain(adt, ..) = &self.signature.ret[0].ty() else {
            unreachable!()
        };
        adt
    }
}

impl HeapExp {
    pub(crate) fn new(exp: Exp) -> Self {
        Self { res: vec![], exp }
    }

    pub(super) fn conjoin(exp: Vec<Exp>) -> Option<Self> {
        let exp = exp.into_iter().fold(None, ExpKind::conjoin);
        exp.map(Self::new)
    }
}

impl From<Vec<PrePostDec>> for Contract {
    fn from(value: Vec<PrePostDec>) -> Self {
        let mut precondition = None;
        let mut decreases = vec![];
        for p in value {
            match p {
                PrePostDec::Pre(e) => precondition = ExpKind::conjoin(precondition, e),
                PrePostDec::Decreases(d) => decreases.push(d),
                _ => {}
            }
        }
        Self {
            precondition: precondition.map(HeapExp::new),
            postcondition: None,
            decreases,
        }
    }
}

impl Contract {
    pub(super) fn add_posts(mut self, posts: Vec<PrePostDec>) -> Self {
        for p in posts {
            match p {
                PrePostDec::Post(e) => {
                    let new = match self.postcondition {
                        Some(mut post) => {
                            post.exp = Box::new(ExpKind::BinOp(BinOp::And, post.exp, e));
                            post
                        }
                        None => HeapExp::new(e),
                    };
                    self.postcondition = Some(new);
                }
                PrePostDec::Decreases(d) => self.decreases.push(d),
                _ => {}
            }
        }
        self
    }
}

impl Field {
    pub fn ty(&self) -> &Type {
        self.0.ret[0].ty()
    }
}

impl<T> Block<T> {
    pub(super) fn map<U>(self, f: impl FnOnce(T) -> U) -> Block<U> {
        Block(f(self.0))
    }
}

impl ExpKind {
    pub fn is_true(&self) -> bool {
        matches!(self, ExpKind::Const(ConstKind::Bool(true)))
    }

    fn conjoin(acc: Option<Exp>, new: Exp) -> Option<Exp> {
        Some(match acc {
            None => new,
            Some(t) if matches!(*t, ExpKind::Const(ConstKind::Bool(true))) => new,
            Some(other) => Box::new(ExpKind::BinOp(BinOp::And, other, new)),
        })
    }
}

impl HeapExp {
    pub fn is_pure(&self) -> bool {
        self.res.is_empty()
    }

    pub fn is_true(&self) -> bool {
        self.is_pure() && self.exp.is_true()
    }
}

impl ResourceExp {
    pub fn loc(&self) -> Result<&Ident, (&HeapExp, &HeapExp)> {
        match &*self.acc.acc.loc {
            ExpKind::FuncApp(callee, ..) => Ok(callee),
            ExpKind::MagicWand(lhs, rhs) => Err((lhs, rhs)),
            _ => unreachable!(),
        }
    }
}

impl Index<MemberId> for Program {
    type Output = Declaration;
    fn index(&self, index: MemberId) -> &Self::Output {
        &self.0[index]
    }
}

fn expect_args<'a>(args: &'a [ArgOrType]) -> impl Iterator<Item = &'a IdnDeclTyped> + 'a {
    args.iter().map(|arg| {
        let ArgOrType::Arg(arg) = arg else {
            panic!("expected argument, got type");
        };
        arg
    })
}
