use core::fmt;

use crate::{parse::{BinOp, HeapUpdateOp, QuantifierKind, UnOp}, program::{Const, DefId, Ty, TyList}, TiVec};

use super::{idx::*, newline};

#[derive(Clone, Default, PartialEq, Eq, Hash)]
pub struct Exp<'tcx> {
    pub lines: TiVec<ExpLocal, ExpLine<'tcx>>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ExpLine<'tcx> {
    pub ty: Ty<'tcx>,
    pub kind: ExpLineKind<'tcx>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum ExpLineKind<'tcx> {
    Use(ExpOperand<'tcx>),
    Call(DefId, Vec<ExpOperand<'tcx>>),
    /// A deref or perm
    Heap(HeapOp, ExpOperand<'tcx>, ExpOperand<'tcx>),
    HeapUpdate(HeapUpdateOp, ExpOperand<'tcx>, ExpOperand<'tcx>, ExpOperand<'tcx>),
    Ternary(ExpOperand<'tcx>, Exp<'tcx>, Exp<'tcx>),
    Quantifier(QuantifierKind, TyList<'tcx>, Vec<Vec<Exp<'tcx>>>, Exp<'tcx>),

    UnOp(UnOp, ExpOperand<'tcx>),
    BinOp(BinOp, ExpOperand<'tcx>, ExpOperand<'tcx>),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExpOperand<'tcx> {
    Const(Const<'tcx>),
    /// A local from an earlier line, the u16 specifies how much nesting to go
    /// back up in the stack.
    ExpLocal(u16, ExpLocal),
    /// A function/method argument/result or a local variable
    Local(Local),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HeapOp {
    Deref,
    Perm,
}

impl<'tcx> Exp<'tcx> {
    pub fn result_ty(&self) -> Ty<'tcx> {
        self.lines.last().unwrap().ty
    }

    pub fn as_operand(&self) -> Option<ExpOperand<'tcx>> {
        if self.lines.len() == 1 {
            self.lines[ExpLocal::ZERO].kind.as_use()
        } else {
            None
        }
    }

    pub fn as_const(&self) -> Option<Const<'tcx>> {
        self.as_operand().and_then(|op| match op {
            ExpOperand::Const(c) => Some(c),
            _ => None,
        })
    }

    pub fn walk_operands(&mut self, f: &mut impl FnMut(&mut ExpOperand<'tcx>)) {
        self.lines.iter_mut().for_each(|l| l.kind.walk_operands(f));
    }
}

impl<'tcx> ExpLineKind<'tcx> {
    pub fn as_use(&self) -> Option<ExpOperand<'tcx>> {
        match self {
            ExpLineKind::Use(op) => Some(*op),
            _ => None,
        }
    }

    pub fn walk_operands(&mut self, f: &mut impl FnMut(&mut ExpOperand<'tcx>)) {
        use ExpLineKind::*;
        match self {
            Use(op) => f(op),
            Call(_, nds) => nds.iter_mut().for_each(f),
            Heap(_, hnd, rnd) => {
                f(hnd);
                f(rnd);
            }
            HeapUpdate(_, hnd, rnd, pnd) => {
                f(hnd);
                f(rnd);
                f(pnd);
            }
            Ternary(c, t, e) => {
                f(c);
                t.walk_operands(f);
                e.walk_operands(f);
            }
            Quantifier(..) => todo!(),
            UnOp(_, nd) => f(nd),
            BinOp(_, lnd, rnd) => {
                f(lnd);
                f(rnd);
            }
        }
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
                ExpOperand::ExpLocal(indent as u16, l).fmt(f)?;
                write!(f, ": ")?;
                line.ty.fmt(f)?;
                write!(f, " := ")?;
            }
            line.kind.fmt(f)?;
        }
        Ok(())
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
            Heap(op, hnd, rnd) => {
                write!(f, "{op}⟦{hnd:?}⟧ {rnd:?}")
            }
            HeapUpdate(op, hnd, rnd, pnd) => {
                write!(f, "{op}⟦{hnd:?}⟧ acc({rnd:?}, {pnd:?})")
            }
            Ternary(c, t, e) => match (t.as_operand(), e.as_operand()) {
                (Some(t), Some(e)) => write!(f, "{c:?} ? {t:?} : {e:?}"),
                _ => {
                    let indent = f.width().unwrap_or_default() + 1;
                    let si = f.precision().unwrap_or_default() + 1;
                    write!(f, "{c:?} ?")?;
                    newline(f)?;
                    write!(f, "  {t:indent$.si$?}")?;
                    newline(f)?;
                    write!(f, "  :")?;
                    newline(f)?;
                    write!(f, "  {e:indent$.si$?}")
                }
            },
            Quantifier(kind, tys, triggers, body) => {
                let indent = f.width().unwrap_or_default() + 1;
                let si = f.precision().unwrap_or_default() + 2;
                match kind {
                    QuantifierKind::Forall => write!(f, "∀ ")?,
                    QuantifierKind::Exists => write!(f, "∃ ")?,
                }
                for (i, ty) in tys.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    let operand = ExpOperand::ExpLocal((si - 1) as u16, ExpLocal::from(i));
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
            BinOp(op, lnd, rnd) => write!(f, "{lnd:?} {op} {rnd:?}"),
        }
    }
}

impl fmt::Debug for ExpOperand<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            ExpOperand::Const(c) => c.fmt(f),
            ExpOperand::ExpLocal(0, el) => el.fmt(f),
            ExpOperand::ExpLocal(n, el) => {
                el.fmt(f)?;
                write!(f, "↑{n}")
            }
            ExpOperand::Local(l) => l.fmt(f),
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
