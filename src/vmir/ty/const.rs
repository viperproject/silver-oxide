use core::{fmt, ops::*};

use crate::parse::{BinOp, ConstHeapKind, ConstKind, UnOp};

use super::{Interned, TyCtxt};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Const<'tcx>(pub(crate) Interned<'tcx, ConstKind>);

impl<'tcx> Const<'tcx> {
    pub fn kind(self) -> &'tcx ConstKind {
        self.0 .0
    }

    pub fn as_bool(self) -> Option<bool> {
        match self.kind() {
            ConstKind::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_int(self) -> Option<&'tcx num::BigInt> {
        match self.kind() {
            ConstKind::Int(i) => Some(i),
            _ => None,
        }
    }

    pub fn as_real(self) -> Option<&'tcx num::BigRational> {
        match self.kind() {
            ConstKind::Real(r) => Some(r),
            _ => None,
        }
    }

    pub fn as_heap(self) -> Option<ConstHeapKind> {
        match self.kind() {
            ConstKind::Heap(h) => Some(*h),
            _ => None,
        }
    }

    pub fn is_wildcard(self) -> bool {
        matches!(self.kind(), ConstKind::Wildcard)
    }

    pub fn is_epsilon(self) -> bool {
        matches!(self.kind(), ConstKind::Epsilon)
    }

    pub fn is_heap(self) -> bool {
        self.as_heap().is_some()
    }

    pub fn is_imprecise(self) -> bool {
        self.is_wildcard() || self.is_heap()
    }
}

impl<'tcx> TyCtxt<'tcx> {
    pub fn eval_unop(&self, op: UnOp, c: Const<'tcx>) -> Option<Const<'tcx>> {
        if c.is_imprecise() {
            return None;
        }
        use ConstKind::*;
        use UnOp::*;
        match op {
            Not => Some(self.interner.mk_const(Bool(!c.as_bool().unwrap()))),
            Neg => match c.kind() {
                Int(i) => Some(self.interner.mk_const(Int(-i))),
                Real(r) => Some(self.interner.mk_const(Real(-r))),
                Epsilon => None,
                _ => unreachable!(),
            },
            IntToReal => Some(
                self.interner
                    .mk_const(Real(c.as_int().unwrap().clone().into())),
            ),
            Abs => todo!(),
            Deref | Perm => unreachable!(),
        }
    }

    pub fn eval_binop(&self, op: BinOp, lhs: Const<'tcx>, rhs: Const<'tcx>) -> Option<Const<'tcx>> {
        use BinOp::*;
        // These do not appear in the expressions we construct
        assert!(!matches!(op, Implies | Or | And | Gt | Ge));
        self.eval_binop_full(op, lhs, rhs)
    }

    pub fn eval_binop_full(
        &self,
        op: BinOp,
        lhs: Const<'tcx>,
        rhs: Const<'tcx>,
    ) -> Option<Const<'tcx>> {
        use BinOp::*;
        use ConstKind::*;
        let for_compare = |c: Const<'tcx>| {
            if c.is_epsilon() {
                self.interner.mk_const(ConstKind::none()).kind()
            } else {
                c.kind()
            }
        };
        let op_bool = |bool_: fn(bool, bool) -> bool| {
            self.interner
                .mk_const(Bool(bool_(lhs.as_bool().unwrap(), rhs.as_bool().unwrap())))
        };
        let op_both = |int: fn(&'tcx num::BigInt, &'tcx num::BigInt) -> num::BigInt,
                       real: fn(
            &'tcx num::BigRational,
            &'tcx num::BigRational,
        ) -> num::BigRational| match (lhs.kind(), rhs.kind()) {
            (Int(l), Int(r)) => Some(self.interner.mk_const(Int(int(l, r)))),
            (Real(l), Real(r)) => Some(self.interner.mk_const(Real(real(l, r)))),
            _ => None,
        };
        match op {
            Iff => Some(op_bool(|a, b| a == b)),
            Eq => match (lhs.kind(), rhs.kind()) {
                (Wildcard, Real(r)) | (Real(r), Wildcard) => {
                    (*r <= ConstKind::rational(0)).then(|| self.interner.mk_const(Bool(false)))
                }
                _ if lhs.is_imprecise() || rhs.is_imprecise() => None,
                _ => Some(self.interner.mk_const(Bool(lhs == rhs))),
            },
            Neq => {
                let eq = self.eval_binop(Eq, lhs, rhs)?;
                self.eval_unop(UnOp::Not, eq)
            }
            Lt => match (for_compare(lhs), for_compare(rhs)) {
                (Int(l), Int(r)) => Some(self.interner.mk_const(Bool(l < r))),
                (Real(l), Real(r)) => Some(self.interner.mk_const(Bool(l < r))),
                (Wildcard, Real(r)) => {
                    (*r <= ConstKind::rational(0)).then(|| self.interner.mk_const(Bool(false)))
                }
                (Real(r), Wildcard) => {
                    (*r <= ConstKind::rational(0)).then(|| self.interner.mk_const(Bool(true)))
                }
                _ => None,
            },
            Le => {
                let lt = self.eval_binop(Lt, lhs, rhs)?;
                let eq = self.eval_binop(Eq, lhs, rhs)?;
                Some(
                    self.interner
                        .mk_const(Bool(lt.as_bool().unwrap() || eq.as_bool().unwrap())),
                )
            }
            In => todo!(),
            Plus => op_both(<&num::BigInt as Add>::add, <&num::BigRational as Add>::add),
            Minus => op_both(<&num::BigInt as Sub>::sub, <&num::BigRational as Sub>::sub),
            Mult => op_both(<&num::BigInt as Mul>::mul, <&num::BigRational as Mul>::mul),
            Div => op_both(
                <&num::BigInt as core::ops::Div>::div,
                <&num::BigRational as core::ops::Div>::div,
            ),
            Mod => op_both(
                <&num::BigInt as Rem>::rem,
                <&num::BigRational as core::ops::Rem>::rem,
            ),
            IntDiv => op_both(<&num::BigInt as core::ops::Div>::div, |_, _| unreachable!()),
            Union => todo!(),
            SetMinus => todo!(),
            Intersection => todo!(),
            Subset => todo!(),
            Concat => todo!(),
            MagicWand => todo!(),
            Range => todo!(),
            InhaleExhale => todo!(),

            Implies => self.eval_binop(Or, self.eval_unop(UnOp::Not, lhs)?, rhs),
            Or => Some(op_bool(core::ops::BitOr::bitor)),
            And => Some(op_bool(core::ops::BitAnd::bitand)),
            Gt => self.eval_binop(Lt, rhs, lhs),
            Ge => self.eval_binop(Le, rhs, lhs),
        }
    }
}

// fmt

impl fmt::Display for Const<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind().fmt(f)
    }
}

impl fmt::Display for ConstKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use num::{One, Zero};
        match self {
            ConstKind::Bool(b) => write!(f, "{b}"),
            ConstKind::Int(i) => write!(f, "{i}"),
            ConstKind::Real(r) if r.is_zero() => write!(f, "none"),
            ConstKind::Real(r) if r.is_one() => write!(f, "write"),
            ConstKind::Real(r) => write!(f, "{} / {}", r.numer(), r.denom()),
            ConstKind::Null => write!(f, "null"),
            ConstKind::Epsilon => write!(f, "epsilon"),
            ConstKind::Wildcard => write!(f, "wildcard"),
            ConstKind::Heap(ConstHeapKind::Old) => write!(f, "□"),
            ConstKind::Heap(ConstHeapKind::SelfFraming) => write!(f, "▣"),
        }
    }
}
