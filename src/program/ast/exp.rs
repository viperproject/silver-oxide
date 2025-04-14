use core::{fmt, hash};

use crate::{
    parse::{BinOp, HeapUpdateOp, QuantifierKind, UnOp},
    program::{Const, DefId, Ty, TyList},
    TiVec,
};

use super::{
    body::{Operand, OperandKind},
    idx::*,
    newline,
};

#[derive(Clone, Default, PartialEq, Eq, Hash)]
pub struct Exp<'tcx> {
    pub lines: TiVec<ExpLocal, ExpLine<'tcx>>,
}

#[derive(Clone)]
pub struct ExpLine<'tcx> {
    pub ty: Ty<'tcx>,
    pub cond: Option<Box<[ExpCond<'tcx>]>>,
    pub kind: ExpLineKind<'tcx>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExpCond<'tcx> {
    pub cond: ExpOperand<'tcx>,
    pub neg: bool,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum ExpLineKind<'tcx> {
    Call(DefId, Vec<ExpOperand<'tcx>>),
    /// A deref or perm
    Heap(HeapOp, [ExpOperand<'tcx>; 2]),
    HeapUpdate(HeapUpdateOp, [ExpOperand<'tcx>; 3]),
    Quantifier(QuantifierKind, TyList<'tcx>, Vec<Vec<Exp<'tcx>>>, Exp<'tcx>),

    // The following are conditionless (with the exception of the division binops)
    Use(ExpOperand<'tcx>),
    UnOp(UnOp, ExpOperand<'tcx>),
    BinOp(BinOp, [ExpOperand<'tcx>; 2]),
    Ternary([ExpOperand<'tcx>; 3]),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExpOperand<'tcx> {
    pub ty: Ty<'tcx>,
    pub kind: ExpOperandKind<'tcx>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExpOperandKind<'tcx> {
    Const(Const<'tcx>),
    ExpLocal(u16, ExpLocal),
    /// A function/method argument/result or a local variable
    Local(Local),
    /// A quantified variable
    QuantLocal(u16, QuantLocal),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HeapOp {
    Deref,
    Perm,
}

impl<'tcx> Exp<'tcx> {
    pub fn new_use(op: ExpOperandKind<'tcx>, ty: Ty<'tcx>) -> Self {
        let op = ExpOperand { ty, kind: op };
        Self::new_simple(ty, ExpLineKind::Use(op))
    }

    pub fn new_simple(ty: Ty<'tcx>, kind: ExpLineKind<'tcx>) -> Self {
        let mut self_ = Self::default();
        let line = ExpLine {
            ty,
            cond: Some(Default::default()),
            kind,
        };
        self_.lines.push(line);
        self_
    }

    pub fn result(&self) -> ExpOperand<'tcx> {
        let (l, line) = self.lines.last_key_value().unwrap();
        ExpOperand {
            ty: line.ty,
            kind: ExpOperandKind::ExpLocal(0, l),
        }
    }

    pub fn result_ty(&self) -> Ty<'tcx> {
        self.lines.last().unwrap().ty
    }

    pub fn as_operand(&self) -> Option<Operand<'tcx>> {
        if self.lines.len() != 1 {
            return None;
        }
        let use_ = self.lines[ExpLocal::ZERO].kind.as_use()?;
        let kind = match use_.kind {
            ExpOperandKind::Const(c) => OperandKind::Const(c),
            ExpOperandKind::Local(l) => OperandKind::Local(l),
            _ => unreachable!(),
        };
        Some(Operand { ty: use_.ty, kind })
    }

    pub fn as_const(&self) -> Option<Const<'tcx>> {
        self.as_operand().and_then(Operand::as_const)
    }

    pub fn walk<'a>(&'a self) -> ExpLineWalker<'a, 'tcx> {
        let mut walker = ExpLineWalker::default();
        walker.add_exp(self);
        walker
    }
}

impl ExpLine<'_> {
    pub fn is_conditionless(&self) -> bool {
        self.cond.as_ref().is_some_and(|c| c.is_empty())
    }
}

impl<'tcx> ExpLineKind<'tcx> {
    pub fn as_use(&self) -> Option<ExpOperand<'tcx>> {
        match self {
            ExpLineKind::Use(op) => Some(*op),
            _ => None,
        }
    }

    pub fn operands(&self) -> &[ExpOperand<'tcx>] {
        use ExpLineKind::*;
        match self {
            Call(_, nds) => nds,
            Heap(_, nds) => nds,
            HeapUpdate(_, nds) => nds,
            Quantifier(..) => &[],
            Use(op) | UnOp(_, op) => core::slice::from_ref(op),
            BinOp(_, nds) => nds,
            Ternary(nds) => nds,
        }
    }
}

impl<'tcx> ExpOperand<'tcx> {
    pub fn as_const(self) -> Option<Const<'tcx>> {
        match self.kind {
            ExpOperandKind::Const(c) => Some(c),
            _ => None,
        }
    }

    pub fn as_exp_local(self) -> Option<(u16, ExpLocal)> {
        match self.kind {
            ExpOperandKind::ExpLocal(n, el) => Some((n, el)),
            _ => None,
        }
    }
}

#[derive(Default)]
pub struct ExpLineWalker<'a, 'tcx> {
    stack: Vec<core::slice::Iter<'a, ExpLine<'tcx>>>,
}

impl<'a, 'tcx> Iterator for ExpLineWalker<'a, 'tcx> {
    type Item = &'a ExpLine<'tcx>;
    fn next(&mut self) -> Option<Self::Item> {
        let next = loop {
            let line = self.stack.last_mut()?;
            let Some(next) = line.next() else {
                self.stack.pop();
                continue;
            };
            break next;
        };

        if let ExpLineKind::Quantifier(_, _, triggers, exp) = &next.kind {
            for trigger in triggers {
                for subtrigger in trigger {
                    self.add_exp(subtrigger);
                }
            }
            self.add_exp(exp);
        }
        Some(next)
    }
}

impl<'a, 'tcx> ExpLineWalker<'a, 'tcx> {
    pub(super) fn add_exp(&mut self, exp: &'a Exp<'tcx>) {
        self.stack.push(exp.lines.iter());
    }
}

impl<'tcx> From<Operand<'tcx>> for ExpOperand<'tcx> {
    fn from(op: Operand<'tcx>) -> Self {
        let kind = match op.kind {
            OperandKind::Const(c) => ExpOperandKind::Const(c),
            OperandKind::Local(l) => ExpOperandKind::Local(l),
        };
        ExpOperand { ty: op.ty, kind }
    }
}

impl PartialEq for ExpLine<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.cond == other.cond && self.kind == other.kind
    }
}

impl Eq for ExpLine<'_> {}

impl hash::Hash for ExpLine<'_> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.cond.hash(state);
        self.kind.hash(state);
    }
}

// fmt

impl fmt::Debug for Exp<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let indent = f.precision().unwrap_or_default();
        for (l, line) in self.lines.iter_enumerated() {
            if l != ExpLocal::ZERO {
                newline(f)?;
            }
            if l != self.lines.last_key().unwrap() {
                let kind = ExpOperandKind::ExpLocal(indent as u16, l);
                ExpOperand { ty: line.ty, kind }.fmt(f)?;
                write!(f, ": ")?;
                line.ty.fmt(f)?;
                write!(f, " := ")?;
            }
            line.fmt(f)?;
        }
        Ok(())
    }
}

impl fmt::Debug for ExpLine<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.cond.as_deref() {
            None => write!(f, "false ? ")?,
            Some([]) => (),
            Some(c) => {
                write!(f, "[")?;
                for (i, c) in c.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    c.fmt(f)?;
                }
                write!(f, "] ? ")?;
            }
        }
        self.kind.fmt(f)
    }
}

impl fmt::Debug for ExpCond<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.neg {
            write!(f, "!")?;
        }
        self.cond.fmt(f)
    }
}

impl fmt::Debug for ExpLineKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ExpLineKind::*;
        match self {
            Use(nd) => nd.fmt(f),
            Call(did, nds) => {
                write!(f, "{did:?}(")?;
                for (i, nd) in nds.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    nd.fmt(f)?;
                }
                write!(f, ")")
            }
            Heap(op, [hnd, rnd]) => {
                write!(f, "{op}⟦{hnd:?}⟧ {rnd:?}")
            }
            HeapUpdate(op, [hnd, rnd, pnd]) => {
                write!(f, "{op}⟦{hnd:?}⟧ acc({rnd:?}, {pnd:?})")
            }
            Ternary([c, t, e]) => {
                write!(f, "{c:?} ? {t:?} : {e:?}")
            }
            Quantifier(kind, tys, triggers, body) => {
                let indent = f.width().unwrap_or_default() + 1;
                let si = f.precision().unwrap_or_default() + 1;
                match kind {
                    QuantifierKind::Forall => write!(f, "∀ ")?,
                    QuantifierKind::Exists => write!(f, "∃ ")?,
                }
                for (i, ty) in tys.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    let kind = ExpOperandKind::QuantLocal((si - 1) as u16, QuantLocal::from(i));
                    let operand = ExpOperand { ty: *ty, kind };
                    write!(f, "{operand:?}: {ty:?}")?;
                }
                write!(f, ".")?;
                for trigger in triggers {
                    write!(f, "{{")?;
                    for (i, subtrigger) in trigger.iter().enumerate() {
                        if i > 0 {
                            write!(f, ",")?;
                        }
                        if let Some(subtrigger) = subtrigger.as_operand() {
                            write!(f, " {subtrigger:?}")?;
                        } else {
                            newline(f)?;
                            write!(f, "  {subtrigger:indent$.si$?}")?;
                        }
                    }
                    write!(f, " }}")?;
                }
                newline(f)?;
                write!(f, "  {body:indent$.si$?}")
            }
            UnOp(op, nd) => write!(f, "{op} {nd:?}"),
            BinOp(op, [lnd, rnd]) => write!(f, "{lnd:?} {op} {rnd:?}"),
        }
    }
}

impl fmt::Debug for ExpOperand<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            ExpOperandKind::Const(c) => c.fmt(f),
            ExpOperandKind::ExpLocal(n, el) => {
                el.fmt(f)?;
                if n != 0 {
                    write!(f, "↑{n}")?;
                }
                Ok(())
            }
            ExpOperandKind::QuantLocal(n, ql) => {
                ql.fmt(f)?;
                if n != 0 {
                    write!(f, "↑{n}")?;
                }
                Ok(())
            }
            ExpOperandKind::Local(l) => l.fmt(f),
        }
    }
}

impl fmt::Display for HeapOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            HeapOp::Deref => write!(f, "*"),
            HeapOp::Perm => write!(f, "$"),
        }
    }
}

impl fmt::Display for HeapUpdateOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            HeapUpdateOp::Unfold => write!(f, "unfold"),
            HeapUpdateOp::Fold => write!(f, "fold"),
            HeapUpdateOp::Apply => write!(f, "apply"),
            HeapUpdateOp::Package => write!(f, "package"),
        }
    }
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            UnOp::Neg => write!(f, "-"),
            UnOp::Not => write!(f, "!"),
            UnOp::Deref => write!(f, "*"),
            UnOp::Perm => write!(f, "$"),
            UnOp::IntToReal => write!(f, "itr"),
            UnOp::Abs => write!(f, "abs"),
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            BinOp::Implies => write!(f, "==>"),
            BinOp::Or => write!(f, "||"),
            BinOp::And => write!(f, "&&"),
            BinOp::Iff => write!(f, "<==>"),
            BinOp::Eq => write!(f, "=="),
            BinOp::Neq => write!(f, "!="),
            BinOp::Lt => write!(f, "<"),
            BinOp::Le => write!(f, "<="),
            BinOp::Gt => write!(f, ">"),
            BinOp::Ge => write!(f, ">="),
            BinOp::In => write!(f, "in"),
            BinOp::Plus => write!(f, "+"),
            BinOp::Minus => write!(f, "-"),
            BinOp::Mult => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Mod => write!(f, "%"),
            BinOp::IntDiv => write!(f, "//"),
            BinOp::Union => write!(f, "+"),
            BinOp::SetMinus => write!(f, "-"),
            BinOp::Intersection => write!(f, "&"),
            BinOp::Subset => write!(f, "<="),
            BinOp::Concat => write!(f, "++"),
            BinOp::MagicWand => todo!(),
            BinOp::Range => todo!(),
            BinOp::InhaleExhale => todo!(),
        }
    }
}
