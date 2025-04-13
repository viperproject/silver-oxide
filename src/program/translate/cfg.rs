use core::{fmt, ops::Deref};
use std::ops::Index;
// use petgraph::algo::dominators;

use crate::parse::Invariant;
use crate::program::Loop;
use crate::{HashMap, HashSet};

use crate::{parse::{Statement, StmtBlock}, program::{BasicBlock, Symbol, TyCtxt}, TiVec};

pub struct Cfg<'a, 'tcx> {
    data: CfgData<'a, 'tcx>,
    postorder: Box<[BasicBlock]>,
    loop_heads: TiVec<Loop, LoopHead>,
    // pub dominators: dominators::Dominators<BasicBlock>,
    // pub scc: Vec<Vec<BasicBlock>>,
}

#[derive(Default)]
pub struct CfgData<'a, 'tcx> {
    labels: HashMap<Symbol<'tcx>, BasicBlock>,
    blocks: TiVec<BasicBlock, BasicBlockData<'a>>,
}

pub struct BasicBlockData<'a> {
    pub bb: BasicBlock,
    pub kind: BasicBlockKind<'a>,

    pub predecessors: Vec<BasicBlock>,
    pub pcs: PathConditions,
    pub preorder: usize,
    pub idom: Option<BasicBlock>,

    pub loop_member: Result<LoopHeadData, LoopMember>,
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
        self.loop_member.as_ref().map_or_else(
            |lm| lm.in_loop,
            |_| Some(LoopHead(self.bb))
        )
    }

    pub fn as_loop_head(&self) -> Option<LoopHead> {
        self.loop_member.as_ref().ok().map(|_| LoopHead(self.bb))
    }

    pub fn back_edge_to(&self) -> Option<LoopHead> {
        self.loop_member.as_ref().err().and_then(|lm|
            lm.in_loop.filter(|lh| &[lh.bb()] == self.edges())
        )
    }

    fn member_of_loop_non_framing(&self) -> Option<LoopHead> {
        self.loop_member.as_ref().map_or_else(
            |lm| lm.in_loop,
            |lh| lh.nested
        )
    }
}

impl<'a, 'tcx> Cfg<'a, 'tcx> {
    pub fn new(tcx: &TyCtxt<'tcx>, labels: impl Iterator<Item = Symbol<'tcx>>, body: &'a StmtBlock) -> Self {
        let mut data = CfgData::new(tcx, labels, body);
        let start = data.start();
        
        // TODO: is this useful?
        let dominators = petgraph::algo::dominators::simple_fast(&data, start);

        // Note that `kosaraju_scc` seems to be broken and give incorrect results?
        // let scc = petgraph::algo::tarjan_scc(&data);

        let postorder = DfsPostOrder::new(&data, start);
        let postorder: Box<[_]> = postorder.iter(&data).collect();

        // Initialise predecessors and path conditions
        for (preorder, &from) in postorder.iter().rev().enumerate() {
            let pcs = PathConditions::new(&data, from);
            if let PathConditions::Ref(from) = &pcs {
                // We want to have the nice property that any "Ref" can be
                // merged with the previous bb in the preorder.
                let prev_in_preorder = postorder[postorder.len() - preorder];
                assert!(*from == prev_in_preorder || pcs == data[prev_in_preorder].pcs)
            }

            let bb = &mut data.blocks[from];
            bb.pcs = pcs;
            bb.preorder = preorder;
            bb.idom = dominators.immediate_dominator(from);
            let kind = bb.kind;

            for &to in kind.edges() {
                data.blocks[to].predecessors.push(from);
            }
        }

        // Initialised loop data
        let loop_heads = Self::initialise_loops(&mut data, &postorder);

        Self {
            data,
            postorder,
            loop_heads,
            // dominators,
            // scc,
        }
    }

    pub fn preorder(&self) -> impl Iterator<Item = (BasicBlock, &'_ BasicBlockData<'a>)> + '_ {
        self.postorder.iter().rev().map(|&bb| (bb, &self.data.blocks[bb]))
    }

    pub fn loop_head(&self, loop_: Loop) -> LoopHead {
        self.loop_heads[loop_]
    }

    fn initialise_loops(data: &mut CfgData, postorder: &[BasicBlock]) -> TiVec<Loop, LoopHead> {
        let mut loop_heads = TiVec::<Loop, LoopHead>::new();
        let mut loop_members = TiVec::<BasicBlock, HashSet<LoopHead>>::with_capacity(data.blocks.len());
        loop_members.resize(data.blocks.len(), HashSet::new());

        for &from_bb in postorder {
            let from = &data.blocks[from_bb];
            let (preorder, kind) = (from.preorder, from.kind);
            let mut loop_member = HashSet::new();
            for &to_bb in kind.edges() {
                let to = &data.blocks[to_bb];
                let to_loop_member = &loop_members[to_bb];
                if to.preorder <= preorder {
                    // The only edge out of this node is this back edge
                    assert_eq!(kind.edges().len(), 1);
                    // Have not visited yet
                    assert_eq!(to_loop_member.len(), 0);
                    let to_lh = LoopHead(to_bb);
                    if let lh@Err(..) = &mut data.blocks[to_bb].loop_member {
                        let loop_ = loop_heads.push_and_get_key(to_lh);
                        *lh = Ok(LoopHeadData { loop_, nested: None })
                    }
                    loop_member.insert(to_lh);
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
                assert_eq!(data.blocks[to].predecessors.len(), 1);
            }

            match &mut data.blocks[to].loop_member {
                Ok(to_lh) => {
                    assert_eq!(loop_exit.len(), 0);
                    to_lh.nested = in_loop;
                }
                Err(lm) => {
                    lm.in_loop = in_loop;
                    lm.loop_exit = loop_exit;
                }
            }
        }
        for (loop_, other) in errors {
            data.dump_dot("problem-cfg.dot");
            let lh_kind = data[loop_.bb()].kind;
            match (lh_kind.loop_head().0, data[other].kind.loop_head().0) {
                (None, None) => unreachable!(),
                (Some(label), None) | (None, Some(label)) =>
                    panic!("while loop has a forbidden second entry at label {label}"),
                (Some(l1), Some(l2)) =>
                    panic!("label {l1} and {l2} are both entry points into the same loop, which is not allowed"),
            }
        }
        loop_heads
    }

    fn select_innermost_loop(data: &CfgData, to: BasicBlock, errors: &mut Vec<(LoopHead, BasicBlock)>) -> Option<LoopHead> {
        let preds = data.blocks[to].preorder_preds(data);
        let mut preds = preds.map(|pred| data.blocks[pred].member_of_loop());

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
    pub fn new(tcx: &TyCtxt<'tcx>, labels: impl Iterator<Item = Symbol<'tcx>>, body: &'a StmtBlock) -> Self {
        let mut self_ = Self::default();
        self_.init(tcx, labels, body);
        self_
    }

    pub fn pcs(&self, bb: BasicBlock) -> Option<impl Iterator<Item = (&[Branch], Branch)> + '_> {
        let pcs = match &self.blocks[bb].pcs {
            PathConditions::Or(pcs) => pcs,
            PathConditions::Ref(bb) => self.blocks[*bb].pcs.expect_or(),
        };
        let has_values = !pcs.is_empty();
        has_values.then(|| pcs.iter().map(|(ks, v)| (ks.as_slice(), *v)))
    }

    pub fn start(&self) -> BasicBlock {
        self.blocks.last_key().unwrap()
    }

    pub fn member_of_loops(&self, bb: BasicBlock) -> impl Iterator<Item = (Loop, LoopHead)> + '_ {
        let mut loop_head = self.blocks[bb].member_of_loop();
        core::iter::from_fn(move || {
            let lh = loop_head?;
            let loop_member = &self.blocks[lh.bb()].loop_member;
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
                    self.mk_block(&block.0[i+1..last], succ, None);
                    last = i;

                    let label = tcx.interner.mk_symbol(label);
                    succ = self.labels[&label];
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
            .filter(|&bb| data.blocks[bb].preorder < self.preorder)
    }

    pub fn is_reachable(&self) -> bool {
        self.preorder != usize::MAX
    }

    fn new(bb: BasicBlock, kind: BasicBlockKind<'a>) -> Self {
        BasicBlockData {
            bb,
            kind,
            predecessors: Default::default(),
            pcs: Default::default(),
            preorder: usize::MAX,
            idom: None,
            loop_member: Err(LoopMember {
                in_loop: None,
                loop_exit: Vec::new(),
            }),
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
pub struct Branch(BasicBlock, bool);

impl PathConditions {
    pub fn new<'a>(cfg: &CfgData, bb: BasicBlock) -> Self {
        let incoming = match Self::predecessors(cfg, bb) {
            Ok(incoming) => incoming,
            Err(bb) => {
                let pred = &cfg.blocks[bb].pcs;
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
        let predecessors = cfg.blocks[curr].preorder_preds(cfg);
        let mut pre = predecessors.map(move |pre| (pre, cfg.blocks[pre].kind.branch(curr)));
        match (pre.next(), pre.next()) {
            (None, Some(..)) => unreachable!(),
            (Some((pre, None)), None) => Err(pre),
            (first, second) => {
                Ok(first.into_iter().chain(second).chain(pre))
            }
        }
    }

    pub fn same_as(&self) -> Option<BasicBlock> {
        match self {
            PathConditions::Ref(bb) => Some(*bb),
            _ => None,
        }
    }

    pub fn expect_or(&self) -> &Pcs {
        match self {
            PathConditions::Or(pcs) => pcs,
            _ => panic!("Expected Or"),
        }
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
        write!(f, "[{:?} / {}] ", self.bb, self.preorder)?;
        match &self.kind {
            Return => write!(f, "return"),
            Block(block, ..) => {
                write!(f, "block ({})", block.len())?;
                if let Some(Statement::Label(label, ..)) = block.get(0) {
                    write!(f, "\nlabel {}", label.0.0)?;
                }
                Ok(())
            }
            Branch(s, ..) => match s {
                Statement::While(..) => write!(f, "while-branch"),
                Statement::If(..) => write!(f, "if-branch"),
                _ => unreachable!(),
            },
        }?;
        // write!(f, "\nImm dom: {:?}", self.idom)?;
        if let PathConditions::Or(pcs) = &self.pcs {
            if pcs.is_empty() {
                write!(f, "\n||")?;
            }
            for (path, last) in pcs.iter() {
                write!(f, "\n|| ")?;
                for branch in path {
                    write!(f, "{branch:?}, ")?;
                }
                write!(f, "{last:?}")?;
            }
        }
        write!(f, "\n{:?}", self.loop_member)?;
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

// dot

use petgraph::visit::*;

impl GraphBase for &CfgData<'_, '_> {
    type EdgeId = (BasicBlock, BasicBlock);
    type NodeId = BasicBlock;
}
impl GraphRef for &CfgData<'_, '_> {}

impl IntoNodeIdentifiers for &CfgData<'_, '_> {
    type NodeIdentifiers = typed_index_collections::TiSliceKeys<BasicBlock>;
    fn node_identifiers(self) -> Self::NodeIdentifiers {
        self.blocks.keys()
    }
}
impl<'a> Data for &CfgData<'a, '_> {
    type NodeWeight = BasicBlockData<'a>;
    type EdgeWeight = ();
}

impl<'r, 'a> IntoNodeReferences for &'r CfgData<'a, '_> {
    type NodeRef = BasicBlockRef<'r, 'a>;
    type NodeReferences = core::iter::Map<typed_index_collections::TiEnumerated<core::slice::Iter<'r, BasicBlockData<'a>>, BasicBlock, &'r BasicBlockData<'a>>, fn((BasicBlock, &'r BasicBlockData<'a>)) -> BasicBlockRef<'r, 'a>>;

    fn node_references(self) -> Self::NodeReferences {
        self.blocks.iter_enumerated().map(BasicBlockRef)
    }
}

#[derive(Clone, Copy)]
pub struct BasicBlockRef<'r, 'a>((BasicBlock, &'r BasicBlockData<'a>));

impl<'r, 'a> NodeRef for BasicBlockRef<'r, 'a> {
    type NodeId = BasicBlock;
    type Weight = BasicBlockData<'a>;

    fn id(&self) -> Self::NodeId {
        self.0.0
    }

    fn weight(&self) -> &Self::Weight {
        self.0.1
    }
}

impl<'r, 'a> IntoEdgeReferences for &'r CfgData<'a, '_> {
    type EdgeRef = BasicBlockEdgeRef;
    type EdgeReferences = core::iter::FlatMap<typed_index_collections::TiEnumerated<core::slice::Iter<'r, BasicBlockData<'a>>, BasicBlock, &'r BasicBlockData<'a>>, EdgeIterator<'r>, fn((BasicBlock, &'r BasicBlockData<'a>)) -> EdgeIterator<'r>>;

    fn edge_references(self) -> Self::EdgeReferences {
        fn edges<'r>((i, b): (BasicBlock, &'r BasicBlockData)) -> EdgeIterator<'r> {
            EdgeIterator(i, b.edges().iter())
        }
        self.blocks.iter_enumerated().flat_map(edges)
    }
}

pub struct EdgeIterator<'r>(BasicBlock, core::slice::Iter<'r, BasicBlock>);

impl Iterator for EdgeIterator<'_> {
    type Item = BasicBlockEdgeRef;
    fn next(&mut self) -> Option<Self::Item> {
        self.1.next().map(|&to| BasicBlockEdgeRef((self.0, to)))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BasicBlockEdgeRef((BasicBlock, BasicBlock));

impl EdgeRef for BasicBlockEdgeRef {
    type NodeId = BasicBlock;
    type EdgeId = (BasicBlock, BasicBlock);
    type Weight = ();
    fn source(&self) -> Self::NodeId {
        self.0 .0
    }
    fn target(&self) -> Self::NodeId {
        self.0 .1
    }
    fn weight(&self) -> &Self::Weight {
        &()
    }
    fn id(&self) -> Self::EdgeId {
        self.0
    }
}

impl NodeIndexable for &CfgData<'_, '_> {
    fn node_bound(&self) -> usize {
        self.blocks.len()
    }
    fn to_index(&self, a: Self::NodeId) -> usize {
        a.into()
    }
    fn from_index(&self, i: usize) -> Self::NodeId {
        i.into()
    }
}

impl GraphProp for &CfgData<'_, '_> {
    type EdgeType = petgraph::Directed;
}

impl VisitMap<BasicBlock> for TiVec<BasicBlock, bool> {
    fn visit(&mut self, a: BasicBlock) -> bool {
        !core::mem::replace(&mut self[a], true)
    }
    fn is_visited(&self, a: &BasicBlock) -> bool {
        self[*a]
    }
}

impl Visitable for &CfgData<'_, '_> {
    type Map = TiVec<BasicBlock, bool>;
    fn visit_map(&self) -> Self::Map {
        self.blocks.iter().map(|_| false).collect()
    }
    fn reset_map(self: &Self, map: &mut Self::Map) {
        map.fill(false);
    }
}

impl<'r> IntoNeighbors for &'r CfgData<'_, '_> {
    type Neighbors = core::iter::Copied<core::slice::Iter<'r, BasicBlock>>;
    fn neighbors(self, a: BasicBlock) -> Self::Neighbors {
        self.blocks[a].edges().iter().copied()
    }
}

impl<'r> IntoNeighborsDirected for &'r CfgData<'_, '_> {
    type NeighborsDirected = core::iter::Copied<core::slice::Iter<'r, BasicBlock>>;
    fn neighbors_directed(self, a: BasicBlock, dir: petgraph::Direction) -> Self::NeighborsDirected {
        match dir {
            petgraph::Direction::Incoming => self.blocks[a].predecessors.iter().copied(),
            petgraph::Direction::Outgoing => self.neighbors(a),
        }
    }
}

impl<'a, 'tcx> CfgData<'a, 'tcx> {
    pub fn dump_dot(&self, path: &str) {
        use petgraph::dot::*;
        let graph = NodeFiltered::from_fn(self, |n| self.blocks[n].is_reachable());

        let gea = |_, er: BasicBlockEdgeRef| {
            let (from_bb, to_bb) = (er.source(), er.target());
            let (from, to) = (&self.blocks[from_bb], &self.blocks[to_bb]);
            let back_edge = from.preorder >= to.preorder;

            let from_loop = from.member_of_loop();
            let to_loop = to.member_of_loop();
            let loop_change = from_loop != to_loop;
            let loop_entry = loop_change && from_loop.is_none_or(|fl| self.member_of_loops(to_bb).any(|(_, tl)| fl == tl));
            let problem_entry = loop_entry && to.member_of_loop_non_framing() != from_loop;

            let style = if back_edge {
                "dashed"
            } else {
                "solid"
            };
            let constraint = if back_edge {
                "false"
            } else {
                "true"
            };
            let color = if problem_entry {
                "red"
            } else {
                "black"
            };
            format!("style={style:?} color={color:?} constraint={constraint:?}")
        };
        let gna = |_, bbr: BasicBlockRef<'_, '_>| {
            let bbd = bbr.0.1;
            let color = match &bbd.loop_member {
                Ok(..) => "forestgreen",
                Err(lm) if !lm.loop_exit.is_empty() => if bbd.back_edge_to().is_some() {
                    "purple"
                } else {
                    "blue"
                },
                Err(..) if bbd.back_edge_to().is_some() => "orange",
                Err(..) => "black",
            };
            format!("shape=box color={color:?}")
        };
        let dot = Dot::with_attr_getters(
            &graph,
            &[Config::EdgeNoLabel],
            &gea,
            &gna,
        );
        // Dump dot to file
        use std::fs::File;
        use std::io::Write;
        let path = std::path::Path::new(path);
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).unwrap();
        }
        let mut file = File::create(path).unwrap();
        write!(file, "{:?}", dot).unwrap();
    }
}
