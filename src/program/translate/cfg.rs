use core::{fmt, ops::Deref};
use std::ops::Index;
// use petgraph::algo::dominators;

use petgraph::visit::{DfsPostOrder, Walker};

use crate::parse::{ExpKind, Ident, Invariant};
use crate::program::{CanDot, Loop};
use crate::{HashMap, HashSet};

use crate::{parse::{Statement, StmtBlock}, program::{BasicBlock, Symbol, TyCtxt}, TiVec};

pub struct Cfg<'a, 'tcx> {
    data: CfgData<'a, 'tcx>,
    postorder: Box<[BasicBlock]>,
    loop_data: TiVec<Loop, LoopData<'tcx>>,
    // pub dominators: dominators::Dominators<BasicBlock>,
    // pub scc: Vec<Vec<BasicBlock>>,
}

pub struct CfgData<'a, 'tcx> {
    pub(crate) name: Symbol<'tcx>,
    pub(crate) blocks: TiVec<BasicBlock, BasicBlockData<'a>>,
    labels: HashMap<Symbol<'tcx>, BasicBlock>,
}

pub struct LoopData<'tcx> {
    pub head: LoopHead,
    pub modifies: HashSet<Symbol<'tcx>>,
}

pub struct BasicBlockData<'a> {
    pub bb: BasicBlock,
    pub kind: BasicBlockKind<'a>,

    pub preorder: usize,
    pub idom: Option<BasicBlock>,
    pub predecessors: Vec<BasicBlock>,

    // `Ok(false)` non-dead branch, `Ok(true)` all succ are dead, `Err(b)` only
    // the `b` succ is dead. We use this to ensure petgraph iterates over dead
    // branches first.
    pub dead_branch: Result<bool, bool>,
    pub loop_member: Result<LoopHeadData, LoopMember>,

    pub pcs: PathConditions,
}

#[derive(Debug, Clone, Copy)]
pub enum BasicBlockKind<'a> {
    Return,
    Block(&'a [Statement], BasicBlock),
    Branch(&'a Statement, [BasicBlock; 2]),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LoopHead(BasicBlock);

impl LoopHead {
    pub fn bb(self) -> BasicBlock {
        self.0
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LoopHeadData {
    loop_: Loop,
    nested: Option<LoopHead>,
}

#[derive(Debug)]
pub struct LoopMember {
    pub in_loop: Option<LoopHead>,
    pub loop_exit: Vec<LoopHead>,
}

impl<'a> BasicBlockData<'a> {
    pub fn edges(&self) -> &[BasicBlock] {
        self.kind.edges()
    }

    pub fn member_of_loop(&self) -> Option<LoopHead> {
        self.loop_member.as_ref().err().map_or(
            Some(LoopHead(self.bb)),
            |lm| lm.in_loop,
        )
    }

    pub fn loop_exit(&self) -> &[LoopHead] {
        self.loop_member.as_ref().err().map_or(
            &[],
            |lm| lm.loop_exit.as_slice()
        )
    }

    pub fn as_loop_head(&self) -> Option<LoopHead> {
        self.loop_member.as_ref().ok().map(|_| LoopHead(self.bb))
    }

    pub fn back_edge_to(&self) -> Option<LoopHead> {
        self.member_of_loop().filter(|lh| &[lh.bb()] == self.edges())
    }

    pub(crate) fn member_of_loop_non_framing(&self) -> Option<LoopHead> {
        self.loop_member.as_ref().map_or_else(
            |lm| lm.in_loop,
            |lh| lh.nested
        )
    }

    pub fn is_dead(&self) -> bool {
        self.dead_branch.is_ok_and(|dead| dead)
    }
}

impl<'a, 'tcx> Cfg<'a, 'tcx> {
    pub fn new(tcx: &TyCtxt<'tcx>, name: Symbol<'tcx>, labels: impl Iterator<Item = Symbol<'tcx>>, body: &'a StmtBlock) -> Self {
        let mut data = CfgData::new(tcx, name, labels, body);
        let start = data.start();
        
        // TODO: is this useful?
        let dominators = petgraph::algo::dominators::simple_fast(&data, start);

        // Note that `kosaraju_scc` seems to be broken and give incorrect results?
        // let scc = petgraph::algo::tarjan_scc(&data);

        let postorder = DfsPostOrder::new(&data, start);
        let postorder: Box<[_]> = postorder.iter(&data).collect();

        // Initialise predecessors
        for (preorder, &from) in postorder.iter().rev().enumerate() {
            let bb = &mut data.blocks[from];
            bb.preorder = preorder;
            bb.idom = dominators.immediate_dominator(from);
            let kind = bb.kind;

            for &to in kind.edges() {
                data.blocks[to].predecessors.push(from);
            }
        }

        // Initialise loop data
        let loop_data = Self::initialise_loops(tcx, &mut data, &postorder);

        let postorder = DfsPostOrder::new(&data, start);
        let postorder: Box<[_]> = postorder.iter(&data).collect();

        // Initialise path conditions
        let mut expect_alive = true;
        for (preorder, &bb) in postorder.iter().rev().enumerate() {
            let from = &mut data.blocks[bb];
            from.preorder = preorder;
            assert!(expect_alive != from.is_dead());

            let pcs = PathConditions::new(&data, bb);
            if let PathConditions::Ref(from) = &pcs {
                // We want to have the nice property that any "Ref" can be
                // merged with the previous bb in the preorder.
                let prev_in_preorder = postorder[postorder.len() - preorder];
                assert!(*from == prev_in_preorder || pcs == data[prev_in_preorder].pcs)
            }
            let from = &mut data.blocks[bb];
            from.pcs = pcs;
            expect_alive = from.dead_branch.is_ok_and(|dead| !dead) || from.back_edge_to().is_some();
            if matches!(from.kind, BasicBlockKind::Return) {
                assert_eq!(preorder, postorder.len() - 1);
                assert!(data.pcs(bb).is_none());
            }
        }

        data.dump_dot(false);
        Self {
            data,
            postorder,
            loop_data,
            // dominators,
            // scc,
        }
    }

    pub fn preorder(&self) -> impl Iterator<Item = (BasicBlock, &'_ BasicBlockData<'a>)> + '_ {
        self.postorder.iter().rev().map(|&bb| (bb, &self.data[bb]))
    }

    pub fn loop_head(&self, loop_: Loop) -> LoopHead {
        self.loop_data[loop_].head
    }

    pub fn loop_data(&self, lh: LoopHead) -> &LoopData<'tcx> {
        &self.loop_data[self.data.get_loop(lh)]
    }

    fn initialise_loops(tcx: &TyCtxt<'tcx>, data: &mut CfgData, postorder: &[BasicBlock]) -> TiVec<Loop, LoopData<'tcx>> {
        let mut loop_data = TiVec::<Loop, LoopData<'tcx>>::new();
        let mut loop_members = TiVec::<BasicBlock, HashSet<LoopHead>>::with_capacity(data.blocks.len());
        loop_members.resize(data.blocks.len(), HashSet::new());

        for &from_bb in postorder {
            data.set_dead_branch(from_bb);
            let from = &data[from_bb];
            let (preorder, kind) = (from.preorder, from.kind);
            let mut loop_member = HashSet::new();
            for &to_bb in kind.edges() {
                let to = &data[to_bb];
                let to_loop_member = &loop_members[to_bb];
                if to.preorder <= preorder {
                    // The only edge out of this node is this back edge
                    assert_eq!(kind.edges().len(), 1);
                    // Have not visited yet
                    assert_eq!(to_loop_member.len(), 0);
                    let head = LoopHead(to_bb);
                    data.blocks[to_bb].loop_member = Ok(LoopHeadData { loop_: Loop::MAX, nested: None });
                    loop_member.insert(head);
                } else {
                    // Have visited already
                    let to_loops = to_loop_member.iter().copied();
                    loop_member.extend(to_loops);
                    // This doesn't remove loops which `from` jumped into
                    // (without going through the loop head), thus we need a
                    // second pass after this.
                    if let Some(to_lh) = to.as_loop_head() {
                        loop_member.swap_remove(&to_lh);
                    }
                }
            }
            loop_members[from_bb] = loop_member;
        }
        let mut errors = Vec::new();
        for &to in postorder.iter().rev() {
            let in_loop = Self::select_innermost_loop(data, to, &mut errors);

            let mut loop_exit = Vec::new();
            let to_lm = &loop_members[to];
            let in_loop = in_loop.and_then(|lh|
                data.member_of_loops(lh.bb()).find(|(_, l)| {
                    if to_lm.contains(l) {
                        true
                    } else {
                        loop_exit.push(*l);
                        false
                    }
                })
            ).map(|(_, l)| l);
            if !loop_exit.is_empty() {
                assert_eq!(data[to].predecessors.len(), 1);
            }

            let to_bb = &mut data.blocks[to];
            match &mut to_bb.loop_member {
                Ok(to_lh) => {
                    let head = LoopHead(to);
                    let ld = LoopData { head, modifies: Default::default() };
                    let loop_ = loop_data.push_and_get_key(ld);

                    assert_eq!(loop_exit.len(), 0);
                    to_lh.loop_ = loop_;
                    to_lh.nested = in_loop;
                }
                Err(lm) => {
                    lm.in_loop = in_loop;
                    lm.loop_exit = loop_exit;
                }
            }
            if let Some(lh) = to_bb.member_of_loop() {
                let loop_ = data.get_loop(lh);
                let modifies = data[to].modifies();
                loop_data[loop_].modifies.extend(modifies.map(|m| tcx.interner.mk_symbol(m)));
            }
        }
        for (loop_, other) in errors {
            let path = data.dump_dot(true).unwrap();
            let lh_kind = data[loop_.bb()].kind;
            match (lh_kind.loop_head().0, data[other].kind.loop_head().0) {
                (None, None) => unreachable!(),
                (Some(label), None) | (None, Some(label)) =>
                    panic!("while loop has a forbidden second entry at label {label}, see {path:?}"),
                (Some(l1), Some(l2)) =>
                    panic!("label {l1} and {l2} are both entry points into the same loop, which is not allowed, see {path:?}"),
            }
        }
        for l in loop_data.keys().rev() {
            let ld = &loop_data[l];
            let bb = &data[ld.head.bb()];
            let lhd = bb.loop_member.as_ref().ok().unwrap();
            let Some(nested) = lhd.nested else {
                continue;
            };
            let nested = data.get_loop(nested);
            assert_ne!(l, nested);
            // Safety: the indices differ
            let ld = unsafe {
                &*(ld as *const LoopData<'tcx>)
            };
            let nested = &mut loop_data[nested];
            nested.modifies.extend(ld.modifies.iter().copied());
        }
        loop_data
    }

    fn select_innermost_loop(data: &CfgData, to: BasicBlock, errors: &mut Vec<(LoopHead, BasicBlock)>) -> Option<LoopHead> {
        let preds = data[to].preorder_preds(data);
        let mut preds = preds.map(|pred| data[pred].member_of_loop());

        let mut in_loop = preds.next()?;
        let mut error = false;

        for pred in preds {
            if pred == in_loop {
                continue;
            }
            error = true;
            match (&mut in_loop, pred) {
                (il@None, pred) => *il = pred,
                (_, None) => (),
                (Some(il), Some(pred)) => {
                    let more_inner = data.member_of_loops(pred.bb()).any(|(_, l)| l == *il);
                    if more_inner {
                        *il = pred;
                    } else {
                        assert!(data.member_of_loops(il.bb()).any(|(_, l)| l == pred));
                    }
                }
            }
        }
        if error {
            errors.push((in_loop.unwrap(), to));
        }
        in_loop
    }
}

impl<'a, 'tcx> CfgData<'a, 'tcx> {
    pub fn new(tcx: &TyCtxt<'tcx>, name: Symbol<'tcx>, labels: impl Iterator<Item = Symbol<'tcx>>, body: &'a StmtBlock) -> Self {
        let mut self_ = CfgData {
            name,
            labels: Default::default(),
            blocks: Default::default(),
        };
        self_.init(tcx, labels, body);
        self_
    }

    pub fn pcs(&self, bb: BasicBlock) -> Option<impl Iterator<Item = (&[Branch], Branch)> + '_> {
        self[bb].pcs.pcs().unwrap_or_else(|bb| {
            self[bb].pcs.pcs().unwrap()
        })
    }

    pub fn start(&self) -> BasicBlock {
        self.blocks.last_key().unwrap()
    }

    pub fn get_loop(&self, lh: LoopHead) -> Loop {
        self[lh.bb()].loop_member.as_ref().ok().unwrap().loop_
    }

    pub fn member_of_loops(&self, bb: BasicBlock) -> impl Iterator<Item = (Loop, LoopHead)> + '_ {
        let mut loop_head = self[bb].member_of_loop();
        core::iter::from_fn(move || {
            let lh = loop_head?;
            let loop_member = &self[lh.bb()].loop_member;
            let lhd = loop_member.as_ref().ok().unwrap();
            loop_head = lhd.nested;
            Some((lhd.loop_, lh))
        })
    }

    fn init(&mut self, tcx: &TyCtxt<'tcx>, labels: impl Iterator<Item = Symbol<'tcx>>, body: &'a StmtBlock) {
        let return_ = self.push_and_get_key(|bb| BasicBlockData::return_(bb));
        self.labels = labels.map(|l| {
            (l, self.push_and_get_key(|bb| BasicBlockData::placeholder(bb)))
        }).collect();
        let start = self.walk_block(tcx, body, return_);
        assert_eq!(self.start(), start);
    }

    fn walk_block(&mut self, tcx: &TyCtxt<'tcx>, block: &'a StmtBlock, mut succ: BasicBlock) -> BasicBlock {
        let mut last = block.0.len();
        for i in (0..block.0.len()).rev() {
            let stmt = &block.0[i];
            use Statement::*;
            match stmt {
                Goto(label) => {
                    // Do not make a block, cannot reach this point
                    let label = tcx.interner.mk_symbol(label);
                    succ = self.labels[&label];
                    last = i;
                }
                Label(label, ..) => {
                    let label = tcx.interner.mk_symbol(&label.0);
                    let Some(bb) = self.labels.get(&label).copied() else {
                        continue;
                    };
                    succ = self.mk_block(&block.0[i..last], succ, Some(bb));
                    last = i;
                }
                While(.., loop_) => {
                    let post_loop = self.mk_block(&block.0[i+1..last], succ, None);
                    succ = self.push_and_get_key(|bb| BasicBlockData::branch(bb, stmt, [BasicBlock::MAX, post_loop]));
                    last = i;

                    let loop_start = self.walk_block(tcx, loop_, succ);
                    let BasicBlockKind::Branch(_, [ls, _]) = &mut self.blocks[succ].kind else {
                        unreachable!()
                    };
                    *ls = loop_start;
                }
                If(_, then, else_) => {
                    let post_branch = self.mk_block(&block.0[i+1..last], succ, None);
                    let else_ = else_.as_ref().map(|e| self.walk_block(tcx, e, post_branch)).unwrap_or(post_branch);
                    let then = self.walk_block(tcx, then, post_branch);
                    succ = self.push_and_get_key(|bb| BasicBlockData::branch(bb, stmt, [then, else_]));
                    last = i;
                }
                // TODO: how should we handle this?
                Package(..) => (),
                Block(block) => {
                    let post = self.mk_block(&block.0[i+1..last], succ, None);
                    succ = self.walk_block(tcx, block, post);
                    last = i;
                }
                _ => (),
            }
        }
        self.mk_block(&block.0[0..last], succ, None)
    }

    fn mk_block(&mut self, block: &'a [Statement], succ: BasicBlock, replace: Option<BasicBlock>) -> BasicBlock {
        match replace {
            Some(bb) => {
                let BasicBlockKind::Block(b, s) = &mut self.blocks[bb].kind else {
                    unreachable!()
                };
                assert!(b.is_empty() && *s == BasicBlock::MAX);
                (*b, *s) = (block, succ);
                bb
            }
            None => self.push_and_get_key(|bb| BasicBlockData::block(bb, block, succ)),
        }
    }

    fn push_and_get_key(&mut self, data: impl FnOnce(BasicBlock) -> BasicBlockData<'a>) -> BasicBlock {
        let bb = self.blocks.next_key();
        self.blocks.push_and_get_key(data(bb));
        bb
    }
    
    fn set_dead_branch(&mut self, bb: BasicBlock) {
        let from = &mut self.blocks[bb];
        let preorder = from.preorder;
        match from.kind {
            BasicBlockKind::Return => (),
            BasicBlockKind::Block(.., to) => {
                let to = &self[to];
                if to.preorder <= preorder || to.is_dead() {
                    self.blocks[bb].dead_branch = Ok(true);
                }
            }
            BasicBlockKind::Branch(.., branch) => {
                let dead_branch = match (self[branch[0]].is_dead(), self[branch[1]].is_dead()) {
                    (true, true) => Ok(true),
                    (true, false) => Err(true),
                    (false, true) => Err(false),
                    (false, false) => return,
                };
                self.blocks[bb].dead_branch = dead_branch;
            }
        }
    }
}

impl<'a, 'tcx> Deref for Cfg<'a, 'tcx> {
    type Target = CfgData<'a, 'tcx>;
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<'a> Index<BasicBlock> for CfgData<'a, '_> {
    type Output = BasicBlockData<'a>;
    fn index(&self, index: BasicBlock) -> &Self::Output {
        &self.blocks[index]
    }
}

impl<'a> BasicBlockData<'a> {
    pub fn preorder_preds<'r>(&'r self, data: &'r CfgData) -> impl Iterator<Item = BasicBlock> + 'r {
        self.predecessors.iter().copied()
            .filter(|&bb| data[bb].preorder < self.preorder)
    }

    pub fn is_reachable(&self) -> bool {
        self.preorder != usize::MAX
    }

    fn new(bb: BasicBlock, kind: BasicBlockKind<'a>) -> Self {
        BasicBlockData {
            bb,
            kind,
            preorder: usize::MAX,
            idom: None,
            predecessors: Default::default(),
            dead_branch: Ok(false),
            loop_member: Err(LoopMember {
                in_loop: None,
                loop_exit: Vec::new(),
            }),
            pcs: Default::default(),
        }
    }

    fn placeholder(bb: BasicBlock) -> Self {
        BasicBlockData::new(bb, BasicBlockKind::Block(&[], BasicBlock::MAX))
    }

    fn return_(bb: BasicBlock) -> Self {
        BasicBlockData::new(bb, BasicBlockKind::Return)
    }

    fn block(bb: BasicBlock, block: &'a [Statement], succ: BasicBlock) -> Self {
        BasicBlockData::new(bb, BasicBlockKind::Block(block, succ))
    }

    fn branch(bb: BasicBlock, stmt: &'a Statement, succ: [BasicBlock; 2]) -> Self {
        BasicBlockData::new(bb, BasicBlockKind::Branch(stmt, succ))
    }

    fn modifies(&self) -> impl Iterator<Item = &'a Ident> + '_ {
        let stmts = match self.kind {
            BasicBlockKind::Block(stmts, ..) => stmts,
            _ => &[],
        };
        stmts.iter().flat_map(|stmt| {
            let tgts = match stmt {
                Statement::Assign(tgts, ..) => tgts.as_slice(),
                _ => &[],
            };
            tgts.iter().filter_map(|tgt| {
                match &**tgt {
                    ExpKind::Ident(tgt) => Some(tgt),
                    _ => None,
                }
            })
        })
    }
}

impl<'a> BasicBlockKind<'a> {
    fn edges(&self) -> &[BasicBlock] {
        match self {
            BasicBlockKind::Return => &[],
            BasicBlockKind::Block(_, succ) => core::slice::from_ref(succ),
            BasicBlockKind::Branch(_, succ) => succ,
        }
    }

    fn branch(&self, bb: BasicBlock) -> Option<bool> {
        match self {
            BasicBlockKind::Return => unreachable!(),
            BasicBlockKind::Block(..) => None,
            BasicBlockKind::Branch(_, succ) => {
                assert!(succ[0] == bb || succ[1] == bb, "{bb:?} vs {succ:?}");
                Some(succ[0] == bb)
            },
        }
    }

    fn branch_bool(&self, true_: bool) -> BasicBlock {
        match self {
            BasicBlockKind::Branch(_, succ) if true_ => succ[0],
            BasicBlockKind::Branch(_, succ) => succ[1],
            _ => unreachable!(),
        }
    }

    pub fn loop_head(&self) -> (Option<&'a String>, &'a Invariant) {
        match self {
            BasicBlockKind::Block([Statement::Label(label, inv), ..], ..) => (Some(&label.0.0), inv),
            BasicBlockKind::Branch(Statement::While(_, inv, ..), _) => (None, inv),
            _ => unreachable!(),
        }
    }
}

type Pcs = HashMap<Vec<Branch>, Branch>;

#[derive(Debug, PartialEq, Eq)]
pub enum PathConditions {
    /// The same `PathConditions` as the BasicBlock specified. The preorder
    /// guarantees that all refs to the same block will be immediately after the
    /// block itself.
    Ref(BasicBlock),
    Or(Pcs),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Branch(pub BasicBlock, pub bool);

impl PathConditions {
    pub fn pcs(&self) -> Result<Option<impl Iterator<Item = (&[Branch], Branch)> + '_>, BasicBlock> {
        let pcs = match self {
            PathConditions::Or(pcs) => pcs,
            PathConditions::Ref(bb) => return Err(*bb),
        };
        let has_values = !pcs.is_empty();
        Ok(has_values.then(|| pcs.iter().map(|(ks, v)| (ks.as_slice(), *v))))
    }

    fn new<'a>(cfg: &CfgData, bb: BasicBlock) -> Self {
        let incoming = match Self::predecessors(cfg, bb) {
            Ok(incoming) => incoming,
            Err(bb) => {
                let pred = &cfg[bb].pcs;
                let bb = pred.same_as().unwrap_or(bb);
                return PathConditions::Ref(bb);
            }
        };
        let mut pcs: Pcs = HashMap::default();
        for (pred, branch) in incoming {
            let branch = branch.map(|b| Branch(pred, b));
            let Some(pred) = cfg.pcs(pred) else {
                if let Some(branch) = branch {
                    Self::insert(&mut pcs, Vec::new(), branch);
                }
                continue;
            };

            for (path, mut last) in pred {
                let mut path = path.to_vec();
                if let Some(branch) = branch {
                    path.push(last);
                    last = branch;
                }
                Self::insert(&mut pcs, path, last);
            }
        }
        PathConditions::Or(pcs)
    }

    fn insert(pcs: &mut Pcs, mut path: Vec<Branch>, mut last: Branch) {
        loop {
            use indexmap::map::Entry;
            match pcs.entry(path) {
                Entry::Occupied(o) => {
                    assert_eq!(o.get().1, !last.1);
                    path = o.shift_remove_entry().0;
                    let Some(l) = path.pop() else {
                        return;
                    };
                    last = l;
                }
                Entry::Vacant(v) => {
                    v.insert(last);
                    return;
                }
            }
        }
    }

    fn predecessors<'r>(cfg: &'r CfgData, curr: BasicBlock) -> Result<impl Iterator<Item = (BasicBlock, Option<bool>)> + 'r, BasicBlock> {
        let curr_bb = &cfg[curr];
        let predecessors = curr_bb.preorder_preds(cfg);
        let mut pre = predecessors.map(move |pre| {
            let bb = &cfg[pre];
            let branch = bb.kind.branch(curr);
            // We have already executed the dead branch by the time we're at
            // `curr` and have assumed `false` in that branch.
            let nd_branch = branch.filter(|&b| Err(!b) != bb.dead_branch);
            assert!(branch == nd_branch || cfg[bb.kind.branch_bool(!branch.unwrap())].preorder < curr_bb.preorder);
            (pre, nd_branch, branch.is_some())
        });
        match (pre.next(), pre.next()) {
            (None, Some(..)) => unreachable!(),
            (Some((pre, None, false)), None) => Err(pre),
            (first, second) => {
                let pre = first.into_iter().chain(second).chain(pre);
                Ok(pre.map(|(pre, branch, _)| (pre, branch)))
            }
        }
    }

    pub fn same_as(&self) -> Option<BasicBlock> {
        match self {
            PathConditions::Ref(bb) => Some(*bb),
            _ => None,
        }
    }

    pub fn or(&self) -> Option<&Pcs> {
        match self {
            PathConditions::Or(pcs) => Some(pcs),
            PathConditions::Ref(_) => None,
        }
    }

    pub fn expect_or(&self) -> &Pcs {
        self.or().expect("PathConditions::expect_or: not an Or")
    }
}

impl Default for PathConditions {
    fn default() -> Self {
        PathConditions::Ref(BasicBlock::MAX)
    }
}

impl fmt::Debug for BasicBlockData<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BasicBlockKind::*;
        write!(f, "{}. {:?} ", self.preorder, self.bb)?;
        match &self.kind {
            Return => write!(f, "🚪"),
            Block(block, ..) => {
                write!(f, "📝  x{}", block.len())?;
                if let Some(Statement::Label(label, ..)) = block.get(0) {
                    write!(f, "\n🏷️  {}", label.0.0)?;
                }
                Ok(())
            }
            Branch(s, ..) => match s {
                Statement::While(..) => write!(f, "⏳  while"),
                Statement::If(..) => write!(f, "🌳  if"),
                _ => unreachable!(),
            },
        }?;
        // write!(f, "\nImm dom: {:?}", self.idom)?;
        if let PathConditions::Or(pcs) = &self.pcs {
            if pcs.is_empty() {
                write!(f, "\n|| true")?;
            }
            for (path, last) in pcs.iter() {
                write!(f, "\n|| ")?;
                for branch in path {
                    write!(f, "{branch:?}, ")?;
                }
                write!(f, "{last:?}")?;
            }
        }
        if let Some(loop_) = self.member_of_loop() {
            let nfl = self.member_of_loop_non_framing();
            if nfl == Some(loop_) {
                write!(f, "\n🔄  in {:?}", loop_.bb())?;
            } else {
                write!(f, "\n🔄  head of {:?}", loop_.bb())?;
                if let Some(nfl) = nfl {
                    write!(f, ", nested {:?}", nfl.bb())?;
                }
            }
        }
        Ok(())
    }
}

impl fmt::Debug for Branch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)?;
        if self.1 {
            write!(f, ".T")
        } else {
            write!(f, ".F")
        }
    }
}
