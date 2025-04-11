use core::fmt;

use crate::{parse::{BinOp, HeapUpdateOp, UnOp}, program::{Const, DefId, Ty}, TiVec};

use super::{idx::*, newline};

#[derive(Default)]
pub struct Exp<'tcx> {
    pub lines: TiVec<ExpLocal, ExpLine<'tcx>>,
}

impl<'tcx> Exp<'tcx> {
    pub fn result_ty(&self) -> Ty<'tcx> {
        self.lines.last().unwrap().ty
    }
}

pub struct ExpLine<'tcx> {
    pub ty: Ty<'tcx>,
    pub kind: ExpLineKind<'tcx>,
}

pub enum ExpLineKind<'tcx> {
    Use(ExpOperand<'tcx>),
    Call(DefId, Vec<ExpOperand<'tcx>>),
    /// A deref or perm
    Heap(HeapOp, ExpOperand<'tcx>, ExpOperand<'tcx>),
    HeapUpdate(HeapUpdateOp, ExpOperand<'tcx>, ExpOperand<'tcx>, ExpOperand<'tcx>),
    Ternary(ExpOperand<'tcx>, Exp<'tcx>, Exp<'tcx>),
    Quantifier((), Exp<'tcx>), // TODO: I'm thinking something like a closure

    UnOp(UnOp, ExpOperand<'tcx>),
    BinOp(BinOp, ExpOperand<'tcx>, ExpOperand<'tcx>),
}

#[derive(Clone, Copy)]
pub enum ExpOperand<'tcx> {
    Const(Const<'tcx>),
    /// A local from an earlier line, the u16 specifies how much nesting to go
    /// back up in the stack.
    ExpLocal(u16, ExpLocal),
    /// A function/method argument/result or a local variable
    Local(Local),
}

#[derive(Debug, Clone, Copy)]
pub enum HeapOp {
    Deref,
    Perm,
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
            Ternary(c, t, e) => {
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
            Quantifier(..) => todo!(),
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
