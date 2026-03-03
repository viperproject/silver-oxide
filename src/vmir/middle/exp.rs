use core::{fmt, hash};

use crate::{
    parse::{BinOp, HeapUpdateOp, QuantifierKind, UnOp}, vmir::{
        ty::{Const, Ty, TyCtxt, TyList},
        DefId,
    }, AsBrackets, TiVec
};

use super::{
    body::{Operand, OperandKind},
    idx::*,
    newline,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Exp<'tcx> {
    pub lines: TiVec<ExpLocal, ExpLine<'tcx>>,
    pub result: ExpOperand<'tcx>,
}

#[derive(Debug, Clone)]
pub struct ExpLine<'tcx> {
    pub ty: Ty<'tcx>,
    pub cond: ExpConds<'tcx>,
    pub kind: ExpLineKind<'tcx>,
}

pub type ExpConds<'tcx> = ExpCondsAny<ExpOperand<'tcx>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExpCondsAny<O>(Option<Vec<ExpCondAny<O>>>);

pub type ExpCond<'tcx> = ExpCondAny<ExpOperand<'tcx>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExpCondAny<O> {
    pub cond: O,
    pub neg: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExpLineKind<'tcx> {
    Snapshot(ResourceId, Vec<ExpOperand<'tcx>>),
    /// A deref or perm
    Heap(HeapOp, [ExpOperand<'tcx>; 2]),
    Calling(CallKind, MethodId, [ExpOperand<'tcx>; 2], Vec<ExpOperand<'tcx>>),
    Quantifier(QuantifierKind, TyList<'tcx>, Vec<Vec<Exp<'tcx>>>, Exp<'tcx>),

    // The following are conditionless (with the exception of the division binops)
    Call(FunctionId, Vec<ExpOperand<'tcx>>),
    Adt(AdtId, AdtOp<'tcx>),
    UnOp(UnOp, ExpOperand<'tcx>),
    BinOp(BinOp, [ExpOperand<'tcx>; 2]),
    Ternary([ExpOperand<'tcx>; 3]),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExpOperand<'tcx> {
    pub ty: Ty<'tcx>,
    pub kind: ExpOperandKind<'tcx>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExpOperandKind<'tcx> {
    /// A function/method argument/result or a local variable
    Operand(OperandKind<'tcx>),
    ExpLocal(u16, ExpLocal),
    /// A quantified variable
    QuantLocal(QuantLocal),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AdtOp<'tcx> {
    /// Cons(...)
    Construct(VariantIdx, Vec<ExpOperand<'tcx>>),
    /// adt.field
    Destructor(ExpOperand<'tcx>, VariantIdx, FieldIdx),
    /// adt.isCons
    Discriminator(ExpOperand<'tcx>, VariantIdx),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HeapOp {
    Deref,
    Perm,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CallKind {
    Call,
    UnCall,
}

impl<'tcx> Exp<'tcx> {
    pub fn new_use<O>(op: O) -> Self
    where
        ExpOperand<'tcx>: From<O>,
    {
        let result = ExpOperand::from(op);
        Self {
            lines: Default::default(),
            result,
        }
    }

    pub fn new_simple(ty: Ty<'tcx>, kind: ExpLineKind<'tcx>) -> Self {
        let mut lines = TiVec::default();
        let line = ExpLine {
            ty,
            cond: Default::default(),
            kind,
        };
        lines.push(line);
        let result = ExpOperand {
            ty,
            kind: ExpOperandKind::ExpLocal(0, ExpLocal::ZERO),
        };
        Self { lines, result }
    }

    pub fn result_ty(&self) -> Ty<'tcx> {
        self.result.ty
    }

    pub fn as_operand(&self) -> Option<Operand<'tcx>> {
        if !self.lines.is_empty() {
            return None;
        }
        let kind = match self.result.kind {
            ExpOperandKind::Operand(o) => o,
            _ => unreachable!(),
        };
        Some(Operand {
            ty: self.result.ty,
            kind,
        })
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
        self.cond.is_true()
    }
}

impl<'tcx> ExpLineKind<'tcx> {
    pub fn operands(&self) -> impl Iterator<Item = ExpOperand<'tcx>> + '_ {
        let (a, b) = self.operands_slice();
        a.iter().copied().chain(b.iter().copied())
    }

    pub fn operands_slice(&self) -> (&[ExpOperand<'tcx>], &[ExpOperand<'tcx>]) {
        use ExpLineKind::*;
        match self {
            Snapshot(_, nds) => (nds, &[]),
            Heap(_, nds) => (nds, &[]),
            Calling(_, _, nds, args) => (nds, args),
            Quantifier(..) => (&[], &[]),
            Call(_, nds) => (nds, &[]),
            Adt(_, op) => match op {
                AdtOp::Construct(_, args) => (args, &[]),
                AdtOp::Destructor(op, _, _) | AdtOp::Discriminator(op, _)
                    => (core::slice::from_ref(op), &[]),
            },
            UnOp(_, op) => (core::slice::from_ref(op), &[]),
            BinOp(_, nds) => (nds, &[]),
            Ternary(nds) => (nds, &[]),
        }
    }
}

pub struct ExpCondsPopToken<O>(ExpCondAny<O>, usize);

impl<O> ExpCondsAny<O> {
    pub fn is_false(&self) -> bool {
        self.0.is_none()
    }

    pub fn is_true(&self) -> bool {
        self.0.as_ref().is_some_and(|c| c.is_empty())
    }

    pub fn cond_count(&self) -> Option<usize> {
        self.0.as_ref().map(|c| c.len())
    }

    pub fn push_cond(&mut self, cond: O, neg: bool) -> ExpCondsPopToken<O>
    where
        O: Clone + Eq + fmt::Debug,
    {
        let cond = ExpCondAny { cond, neg };
        let conds = self.0.as_mut().unwrap();
        conds.push(cond.clone());
        ExpCondsPopToken(cond, conds.len())
    }

    pub fn negate_cond(&mut self, token: &mut ExpCondsPopToken<O>)
    where
        O: Eq + fmt::Debug,
    {
        let conds = self.0.as_mut().unwrap();
        assert_eq!(token.1, conds.len());
        let last = conds.last_mut().unwrap();
        assert_eq!(last.cond, token.0.cond);
        assert_eq!(last.neg, token.0.neg);
        last.neg = !last.neg;
        token.0.neg = !token.0.neg;
    }

    pub fn pop_cond(&mut self, token: ExpCondsPopToken<O>)
    where
        O: Eq + fmt::Debug,
    {
        let conds = self.0.as_mut().unwrap();
        assert_eq!(token.1, conds.len());
        let last = conds.pop().unwrap();
        assert_eq!(last.cond, token.0.cond);
        assert_eq!(last.neg, token.0.neg);
    }

    pub fn extend(&mut self, i: impl Iterator<Item = ExpCondAny<O>>) {
        self.0.as_mut().unwrap().extend(i);
    }

    pub fn retain(&mut self, p: impl FnMut(&ExpCondAny<O>) -> bool) {
        self.0.as_mut().unwrap().retain(p);
    }

    pub fn set_false(&mut self) {
        self.0 = None;
    }

    /// Resets the condition to `true` and returns the old condition.
    pub fn take(&mut self) -> Self {
        core::mem::take(self)
    }

    pub fn map<P>(&self, mut f: impl FnMut(&ExpCondAny<O>) -> P) -> ExpCondsAny<P> {
        ExpCondsAny(self.0.as_ref().map(|cs| {
            cs.iter()
                .map(|c| {
                    let cond = f(c);
                    ExpCondAny { cond, neg: c.neg }
                })
                .collect()
        }))
    }

    pub fn iter(&self) -> Option<impl Iterator<Item = &ExpCondAny<O>>> {
        self.0.as_deref().map(|c| c.iter())
    }

    pub fn into_iter(self) -> Option<impl Iterator<Item = ExpCondAny<O>>> {
        self.0.map(|c| c.into_iter())
    }
}

impl<'tcx> ExpOperand<'tcx> {
    pub fn as_const(self) -> Option<Const<'tcx>> {
        match self.kind {
            ExpOperandKind::Operand(o) => o.as_const(),
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

impl<O> Default for ExpCondsAny<O> {
    fn default() -> Self {
        Self(Some(Default::default()))
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

impl<'tcx> From<OperandKind<'tcx>> for ExpOperandKind<'tcx> {
    fn from(op: OperandKind<'tcx>) -> Self {
        ExpOperandKind::Operand(op)
    }
}

impl<'tcx> From<Operand<'tcx>> for ExpOperand<'tcx> {
    fn from(op: Operand<'tcx>) -> Self {
        ExpOperand {
            ty: op.ty,
            kind: ExpOperandKind::Operand(op.kind),
        }
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

impl fmt::Display for Exp<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_range(f, ..)
    }
}

impl Exp<'_> {
    pub fn fmt_range(
        &self,
        f: &mut fmt::Formatter<'_>,
        range: impl core::ops::RangeBounds<ExpLocal>,
    ) -> fmt::Result {
        use core::fmt::Display;
        use core::ops::Bound::*;
        let start = match range.start_bound() {
            Unbounded => 0,
            Included(&start) => usize::from(start),
            Excluded(..) => unreachable!(),
        };
        let end = match range.end_bound() {
            Unbounded => self.lines.raw.len() + 1,
            Included(&end) => usize::from(end) + 1,
            Excluded(&end) => usize::from(end),
        };
        assert!(end <= self.lines.raw.len() + 1);
        if start == end {
            return Ok(());
        }
        let lines_end = core::cmp::min(end, self.lines.raw.len());

        let last_line = self.lines.last_key();
        let result_last_line = end >= self.lines.raw.len() && self.result.as_exp_local().is_some_and(|(i, l)| {
            assert_eq!(i, 0);
            l == last_line.unwrap()
        });
        let indent = f.precision().unwrap_or_default();
        for (l, line) in self.lines.raw[start..lines_end].iter().enumerate() {
            if l != 0 {
                newline(f)?;
            }
            let l = ExpLocal::from(l + start);
            if !(result_last_line && l == last_line.unwrap()) {
                let kind = ExpOperandKind::ExpLocal(indent as u16, l);
                ExpOperand { ty: line.ty, kind }.fmt(f)?;
                write!(f, ": ")?;
                line.ty.fmt(f)?;
                write!(f, " := ")?;
            } else {
                write!(f, "🏁 ")?;
            }
            line.fmt(f)?;
        }
        if end == self.lines.raw.len() + 1 && !result_last_line {
            if start != self.lines.raw.len() {
                newline(f)?;
            }
            write!(f, "🏁 ")?;
            self.result.fmt(f)?;
        }
        Ok(())
    }
}

impl fmt::Display for ExpLine<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.cond.fmt(f)?;
        self.kind.fmt(f)
    }
}

impl<O: fmt::Display> fmt::Display for ExpCondsAny<O> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0.as_deref() {
            None => write!(f, "false ? ")?,
            Some([]) => (),
            Some(c) => write!(f, "{} ? ", c.bracketed())?,
        }
        Ok(())
    }
}

impl<O: fmt::Display> fmt::Display for ExpCondAny<O> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.neg {
            write!(f, "!")?;
        }
        self.cond.fmt(f)
    }
}

impl fmt::Display for ExpLineKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ExpLineKind::*;
        match self {
            Snapshot(rid, nds) => {
                write!(f, "*{rid}")?;
                nds.parenthesised().fmt(f)
            }
            Call(did, nds) => {
                write!(f, "{did}")?;
                nds.parenthesised().fmt(f)
            }
            Heap(op, [hnd, rnd]) => {
                write!(f, "{op}⟦{hnd}⟧ {rnd}")
            }
            Calling(kind, method, [hnd, pnd], args) => {
                write!(f, "{kind} acc⟦{hnd}⟧({method}{}, {pnd})", args.parenthesised())
            }
            Ternary([c, t, e]) => {
                write!(f, "{c} ? {t} : {e}")
            }
            Quantifier(kind, tys, triggers, body) => {
                let indent = f.width().unwrap_or_default() + 1;
                let si = f.precision().unwrap_or_default();
                match kind {
                    QuantifierKind::Forall => write!(f, "∀ ")?,
                    QuantifierKind::Exists => write!(f, "∃ ")?,
                }
                for (i, ty) in tys.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    let kind = ExpOperandKind::QuantLocal(QuantLocal::from(si + i));
                    let operand = ExpOperand { ty: *ty, kind };
                    write!(f, "{operand}: {ty:?}")?;
                }
                write!(f, ".")?;
                let si = si + tys.len();
                for trigger in triggers {
                    write!(f, "{{")?;
                    for (i, subtrigger) in trigger.iter().enumerate() {
                        if i > 0 {
                            write!(f, ",")?;
                        }
                        if let Some(subtrigger) = subtrigger.as_operand() {
                            write!(f, " {subtrigger}")?;
                        } else {
                            newline(f)?;
                            write!(f, "  {subtrigger:indent$.si$}")?;
                        }
                    }
                    write!(f, " }}")?;
                }
                newline(f)?;
                write!(f, "  {body:indent$.si$}")
            }
            Adt(adt, op) => {
                let ctx = unsafe { TyCtxt::global_ref_unchecked() };
                let adt = ctx.interner.get_adt_def(*adt);
                drop(ctx);
                match op {
                    AdtOp::Construct(vid, nds) => {
                        if let Some(name) = adt.data().variants[*vid].name {
                            write!(f, "{name}")?;
                        } else {
                            write!(f, "{adt:?}::{vid:?}")?;
                        }
                        nds.parenthesised().fmt(f)
                    }
                    AdtOp::Destructor(nd, vid, fid) => {
                        let variant = &adt.data().variants[*vid];
                        if let Some(name) = variant.fields[*fid].name {
                            write!(f, "{nd}.{name}")
                        } else {
                            write!(f, "{nd}.{adt:?}::{vid:?}::{fid:?}")
                        }
                    }
                    AdtOp::Discriminator(nd, vid) => {
                        if let Some(name) = adt.data().variants[*vid].name {
                            write!(f, "{nd}.is{name}")
                        } else {
                            write!(f, "{nd}.{adt:?}::is{vid:?}")
                        }
                    }
                }
            }
            UnOp(op, nd) => write!(f, "{op} {nd}"),
            BinOp(op, [lnd, rnd]) => write!(f, "{lnd} {op} {rnd}"),
        }
    }
}

impl fmt::Display for ExpOperand<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl fmt::Display for ExpOperandKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            ExpOperandKind::Operand(o) => o.fmt(f),
            ExpOperandKind::ExpLocal(n, el) => {
                el.fmt(f)?;
                if n != 0 {
                    write!(f, "↑{n}")?;
                }
                Ok(())
            }
            ExpOperandKind::QuantLocal(ql) => ql.fmt(f),
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

impl fmt::Display for CallKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            CallKind::Call => write!(f, "call"),
            CallKind::UnCall => write!(f, "uncall"),
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
            BinOp::Range => todo!(),
            BinOp::InhaleExhale => todo!(),
        }
    }
}
