use crate::HashMap;

use crate::{parse::{Statement, StmtBlock}, program::{BasicBlock, Symbol, TyCtxt}, TiVec};

pub struct Cfg<'a, 'tcx> {
    pub labels: HashMap<Symbol<'tcx>, BasicBlock>,
    pub blocks: TiVec<BasicBlock, BasicBlockData<'a>>,
}

pub enum BasicBlockData<'a> {
    Return,
    Block(&'a [Statement], BasicBlock),
    Branch(&'a Statement, [BasicBlock; 2]),
}

impl<'a> Default for BasicBlockData<'a> {
    fn default() -> Self {
        BasicBlockData::Block(&[], BasicBlock::MAX)
    }
}

impl<'a> BasicBlockData<'a> {
    pub fn edges(&self) -> &[BasicBlock] {
        match self {
            BasicBlockData::Return => &[],
            BasicBlockData::Block(_, succ) => core::slice::from_ref(succ),
            BasicBlockData::Branch(_, succ) => succ,
        }
    }
}

impl<'a, 'tcx> Cfg<'a, 'tcx> {
    pub fn new(tcx: &TyCtxt<'tcx>, labels: impl Iterator<Item = Symbol<'tcx>>, body: &'a StmtBlock) -> Self {
        let mut blocks = TiVec::default();
        let return_ = blocks.push_and_get_key(BasicBlockData::Return);
        let labels = labels.map(|l| {
            (l, blocks.push_and_get_key(BasicBlockData::default()))
        }).collect();
        let mut self_ = Self {
            labels,
            blocks,
        };
        self_.walk_block(tcx, body, return_);
        self_
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
                While(.., block) => {
                    let post_loop = self.mk_block(&block.0[i+1..last], succ, None);
                    succ = self.blocks.push_and_get_key(BasicBlockData::Branch(stmt, [BasicBlock::MAX, post_loop]));
                    last = i;

                    let loop_start = self.walk_block(tcx, block, succ);
                    let BasicBlockData::Branch(_, [ls, _]) = &mut self.blocks[succ] else {
                        unreachable!()
                    };
                    *ls = loop_start;
                }
                If(_, then, else_) => {
                    let post_branch = self.mk_block(&block.0[i+1..last], succ, None);
                    let else_ = else_.as_ref().map(|e| self.walk_block(tcx, e, post_branch)).unwrap_or(post_branch);
                    let then = self.walk_block(tcx, then, post_branch);
                    succ = self.blocks.push_and_get_key(BasicBlockData::Branch(stmt, [then, else_]));
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
                let BasicBlockData::Block(b, s) = &mut self.blocks[bb] else {
                    unreachable!()
                };
                assert!(b.is_empty() && *s == BasicBlock::MAX);
                (*b, *s) = (block, succ);
                bb
            }
            None => self.blocks.push_and_get_key(BasicBlockData::Block(block, succ)),
        }
    }
}

// dot

use core::fmt;
use petgraph::visit::*;

impl fmt::Debug for BasicBlockData<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BasicBlockData::*;
        match self {
            Return => write!(f, "return"),
            Block(block, ..) => write!(f, "block ({})", block.len()),
            Branch(..) => write!(f, "branch"),
        }
    }
}

impl GraphBase for &Cfg<'_, '_> {
    type EdgeId = (BasicBlock, BasicBlock);
    type NodeId = BasicBlock;
}
impl GraphRef for &Cfg<'_, '_> {}

impl IntoNodeIdentifiers for &Cfg<'_, '_> {
    type NodeIdentifiers = typed_index_collections::TiSliceKeys<BasicBlock>;
    fn node_identifiers(self) -> Self::NodeIdentifiers {
        self.blocks.keys()
    }
}
impl<'a> Data for &Cfg<'a, '_> {
    type NodeWeight = BasicBlockData<'a>;
    type EdgeWeight = ();
}

impl<'r, 'a> IntoNodeReferences for &'r Cfg<'a, '_> {
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

impl<'r, 'a> IntoEdgeReferences for &'r Cfg<'a, '_> {
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

#[derive(Clone, Copy)]
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

impl NodeIndexable for &Cfg<'_, '_> {
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

impl GraphProp for &Cfg<'_, '_> {
    type EdgeType = petgraph::Directed;
}

impl<'a, 'tcx> Cfg<'a, 'tcx> {
    pub fn dump_dot(&self, path: &str) {
        use petgraph::dot::*;
        let gna = |_, _bbr: BasicBlockRef<'_, '_>| {
            format!("shape=box")
        };
        let dot = Dot::with_attr_getters(
            self,
            &[Config::EdgeNoLabel],
            &|_, _| "".to_string(),
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
