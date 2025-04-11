use core::ops::*;

use crate::parse::{BinOp, ConstKind, UnOp};
use crate::program::{exp::{ExpLine, ExpLineKind, ExpOperand}, Const};

use super::exp::ExpTranslator;

impl<'tcx> ExpTranslator<'_, '_, 'tcx> {
    pub(super) fn optimise_line(&mut self, mut line: ExpLine<'tcx>) -> Result<ExpOperand<'tcx>, ExpLine<'tcx>> {
        use ExpLineKind::*;
        match &mut line.kind {
            Use(op) => Ok(*op),
            Ternary(ExpOperand::Const(c), t, e) => if c.as_bool().unwrap() {
                Ok(self.inline_exp(core::mem::take(t)))
            } else {
                Ok(self.inline_exp(core::mem::take(e)))
            }
            Ternary(c, t, e) => match (t.as_const().and_then(Const::as_bool), e.as_const().and_then(Const::as_bool)) {
                (Some(b@true), Some(true)) | (Some(b@false), Some(false)) =>
                    Ok(ExpOperand::Const(self.tcx.tcx.interner.mk_const(ConstKind::bool(b)))),
                (Some(true), Some(false)) => Ok(*c),
                (Some(false), Some(true)) => Ok(self.negate(*c)),
                _ => Err(line),
            }
            UnOp(op, ExpOperand::Const(c)) =>
                self.optimise_unop(*op, *c).map(ExpOperand::Const).ok_or(line),
            BinOp(op, ExpOperand::Const(lhs), ExpOperand::Const(rhs)) =>
                self.optimise_binop(*op, *lhs, *rhs).map(ExpOperand::Const).ok_or(line),
            // TODO?
            // BinOp(op, ExpOperand::Const(lhs), rhs) if lhs.as_bool().is_some() => {
            //     todo!()
            // }
            // BinOp(op, lhs, ExpOperand::Const(rhs)) if rhs.as_bool().is_some() => {
            //     todo!()
            // }
            _ => Err(line),
        }
    }

    fn optimise_unop(&mut self, op: UnOp, c: Const<'tcx>) -> Option<Const<'tcx>> {
        if c.is_imprecise() {
            return None;
        }
        use UnOp::*;
        use ConstKind::*;
        match op {
            Not => Some(self.tcx.tcx.interner.mk_const(Bool(!c.as_bool().unwrap()))),
            Neg => match c.kind() {
                Int(i) => Some(self.tcx.tcx.interner.mk_const(Int(-i))),
                Real(r) => Some(self.tcx.tcx.interner.mk_const(Real(-r))),
                Epsilon => None,
                _ => unreachable!(),
            }
            IntToReal => Some(self.tcx.tcx.interner.mk_const(Real(c.as_int().unwrap().clone().into()))),
            Abs => todo!(),
            Deref | Perm => unreachable!(),
        }
    }

    fn optimise_binop(&mut self, op: BinOp, lhs: Const<'tcx>, rhs: Const<'tcx>) -> Option<Const<'tcx>> {
        use BinOp::*;
        use ConstKind::*;
        let for_compare = |c: Const<'tcx>| if c.is_epsilon() {
            self.tcx.tcx.interner.mk_const(ConstKind::none()).kind()
        } else {
            c.kind()
        };
        let op_both = |
            int: fn(&'tcx num::BigInt, &'tcx num::BigInt) -> num::BigInt,
            real: fn(&'tcx num::BigRational, &'tcx num::BigRational) -> num::BigRational,
        | match (lhs.kind(), rhs.kind()) {
            (Int(l), Int(r)) => Some(self.tcx.tcx.interner.mk_const(Int(int(l, r)))),
            (Real(l), Real(r)) => Some(self.tcx.tcx.interner.mk_const(Real(real(l, r)))),
            _ => None,
        };
        match op {
            Implies | Or | And | Gt | Ge => unreachable!(),
            Iff => Some(self.tcx.tcx.interner.mk_const(Bool(lhs.as_bool() == rhs.as_bool()))),
            Eq => match (lhs.kind(), rhs.kind()) {
                (Wildcard, Real(r)) | (Real(r), Wildcard) =>
                    (*r <= ConstKind::rational(0)).then(|| self.tcx.tcx.interner.mk_const(Bool(false))),
                _ if lhs.is_imprecise() || rhs.is_imprecise() => None,
                _ => Some(self.tcx.tcx.interner.mk_const(Bool(lhs == rhs))),
            },
            Neq => {
                let eq = self.optimise_binop(Eq, lhs, rhs)?;
                self.optimise_unop(UnOp::Not, eq)
            }
            Lt => match (for_compare(lhs), for_compare(rhs)) {
                (Int(l), Int(r)) =>
                    Some(self.tcx.tcx.interner.mk_const(Bool(l < r))),
                (Real(l), Real(r)) =>
                    Some(self.tcx.tcx.interner.mk_const(Bool(l < r))),
                (Wildcard, Real(r)) =>
                    (*r <= ConstKind::rational(0)).then(|| self.tcx.tcx.interner.mk_const(Bool(false))),
                (Real(r), Wildcard) =>
                    (*r <= ConstKind::rational(0)).then(|| self.tcx.tcx.interner.mk_const(Bool(true))),
                _ => None,
            }
            Le => {
                let lt = self.optimise_binop(Lt, lhs, rhs)?;
                let eq = self.optimise_binop(Eq, lhs, rhs)?;
                Some(self.tcx.tcx.interner.mk_const(Bool(lt.as_bool().unwrap() || eq.as_bool().unwrap())))
            }
            In => todo!(),
            Plus => op_both(<&num::BigInt as Add>::add, <&num::BigRational as Add>::add),
            Minus => op_both(<&num::BigInt as Sub>::sub, <&num::BigRational as Sub>::sub),
            Mult => op_both(<&num::BigInt as Mul>::mul, <&num::BigRational as Mul>::mul),
            Div => op_both(<&num::BigInt as core::ops::Div>::div, <&num::BigRational as core::ops::Div>::div),
            Mod => op_both(<&num::BigInt as Rem>::rem, <&num::BigRational as core::ops::Rem>::rem),
            IntDiv => op_both(<&num::BigInt as core::ops::Div>::div, |_, _| unreachable!()),
            Union => todo!(),
            SetMinus => todo!(),
            Intersection => todo!(),
            Subset => todo!(),
            Concat => todo!(),
            MagicWand => todo!(),
            Range => todo!(),
            InhaleExhale => todo!(),
        }
    }

    fn negate(&mut self, c: ExpOperand<'tcx>) -> ExpOperand<'tcx> {
        self.new_line(ExpLine { ty: self.tcx.tcx.types.bool_, kind: ExpLineKind::UnOp(UnOp::Not, c) }).0
    }
}
