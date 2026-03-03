use crate::parse::{BinOp, ConstKind};
use crate::translate::member::tychk::{ExpLineIfx, ExpOperandIfx};
use crate::vmir::middle::*;
use crate::vmir::ty::Const;
use crate::{HashMap, HashSet, TiVec};

use super::exp::ExpTranslator;

struct ExpOptimiser<'tcx> {
    evaluated: HashMap<ExpLine<'tcx>, ExpOperand<'tcx>>,
    pub(super) used: HashSet<ExpLocal>,

    pub(super) conditionless: Vec<TiVec<ExpLocal, bool>>,
}

impl<'tcx> ExpTranslator<'_, '_, 'tcx> {
    pub(super) fn optimise_exp(&self, mut e: Exp<'tcx>, mut used: HashSet<ExpLocal>) -> Exp<'tcx> {
        let nd_as_use = |nd: ExpOperand<'tcx>| match nd.as_exp_local() {
            Some((nest, used)) if nest == self.curr_nest => Some(used),
            _ => None,
        };
        if let Some(r) = nd_as_use(e.result) {
            assert!(used.insert(r));
        };
        if let Some(&last) = used.last() {
            assert!(used.iter().all(|&u| u <= last));
            // Not always true: assert!(used.iter().is_sorted());
            assert!(usize::from(last) < e.lines.len());
            e.lines.truncate(usize::from(last) + 1);
        } else {
            e.lines.clear();
        }
        e
    }

    pub(super) fn optimise_line(
        &mut self,
        mut line: ExpLineIfx<'tcx>,
    ) -> Result<ExpOperandIfx<'tcx>, ExpLineIfx<'tcx>> {
        use ExpLineKind::*;
        match line.kind_mut() {
            Ternary([c, t, e]) => match (
                c.as_const(),
                t.as_const().and_then(Const::as_bool),
                e.as_const().and_then(Const::as_bool),
            ) {
                (Some(..), ..) => unreachable!(),
                (_, Some(b @ true), Some(true)) | (_, Some(b @ false), Some(false)) => {
                    let b = self.tcx.tcx.interner.mk_const(ConstKind::bool(b));
                    Ok(ExpOperand {
                        ty: self.tcx.tcx.types.bool_,
                        kind: OperandKind::Const(b).into() 
                    }.into())
                }
                (_, Some(true), Some(false)) => Ok((*c).into()),
                (_, Some(false), Some(true)) => Ok(self.negate(*c).into()),
                _ => Err(line),
            },
            UnOp(op, nd) => match nd.as_const() {
                Some(c) => self
                    .tcx
                    .tcx
                    .eval_unop(*op, c)
                    .map(OperandKind::Const)
                    .map(|kind| ExpOperandIfx::new(line.ty(), kind))
                    .ok_or(line),
                None => Err(line),
            },
            BinOp(op, [lhs, rhs]) => match (lhs.as_const(), rhs.as_const()) {
                (Some(lhs), Some(rhs)) => self
                    .tcx
                    .tcx
                    .eval_binop(*op, lhs, rhs)
                    .map(OperandKind::Const)
                    .map(|kind| ExpOperandIfx::new(line.ty(), kind))
                    .ok_or(line),
                // TODO?
                // (Some(lhs), rhs) if lhs.as_bool().is_some() => {
                //     todo!()
                // }
                // (lhs, Some(rhs)) if rhs.as_bool().is_some() => {
                //     todo!()
                // }
                _ => Err(line),
            },
            _ => Err(line),
        }
    }

    pub(super) fn get_condition(&self, kind: &ExpLineKind) -> ExpConds<'tcx> {
        let cless = match kind {
            // The `Snapshot` before the call checks the function's precondition
            ExpLineKind::Call(..) | ExpLineKind::UnOp(..) | ExpLineKind::Ternary(..) => true,
            ExpLineKind::BinOp(op, ..) => !matches!(op, BinOp::Div | BinOp::IntDiv | BinOp::Mod),
            _ => false,
        };
        let cless = cless
            && kind.operands().all(|o| {
                o.as_exp_local()
                    .is_none_or(|(lvl, eo)| self.conditionless[lvl as usize][eo])
            });
        if cless {
            Default::default()
        } else {
            self.curr_cond.clone()
        }
    }
}
