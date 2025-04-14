use std::fmt::Debug;

use petgraph::visit::*;

use crate::TiVec;

use super::{
    member::DepGraph,
    translate::{BasicBlockData, CfgData},
    BasicBlock,
};

pub(crate) fn log_dir() -> String {
    let path = std::env::var("VIPER_LOG");
    path.ok().unwrap_or_else(|| "log".to_string())
}

pub(crate) trait CanDot {
    fn dump_dot(&self, force: bool) -> Option<String> {
        if !force && std::env::var("VIPER_DOT").is_err() {
            return None;
        }
        let mut path = log_dir();
        path += "/";
        path += &self.filename();
        path += ".dot";

        // Dump dot to file
        let file = std::path::Path::new(&path);
        std::fs::create_dir_all(file.parent().unwrap()).unwrap();
        let mut file = std::fs::File::create(file).unwrap();
        self.write_dot(&mut file).unwrap();
        Some(path)
    }

    fn filename(&self) -> String;
    fn write_dot(&self, f: &mut impl std::io::Write) -> std::io::Result<()>;
}

impl CanDot for DepGraph {
    fn filename(&self) -> String {
        "callgraph".to_string()
    }

    fn write_dot(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        use petgraph::dot::*;
        let gea = |_, _| "".to_string();
        let gna = |_, _| "".to_string();
        let dot = Dot::with_attr_getters(self, &[Config::EdgeNoLabel], &gea, &gna);
        write!(f, "{:?}", dot)
    }
}

impl CanDot for CfgData<'_, '_> {
    fn filename(&self) -> String {
        format!("cfg/{}", self.name.as_str())
    }

    fn write_dot(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        use petgraph::dot::*;
        let graph = NodeFiltered::from_fn(self, |n| self[n].is_reachable());

        let gea = |_, er: BasicBlockEdgeRef| {
            let (from_bb, to_bb) = (er.source(), er.target());
            let (from, to) = (&self[from_bb], &self[to_bb]);
            let back_edge = from.preorder >= to.preorder;

            let from_loop = from.member_of_loop();
            let to_loop = to.member_of_loop();
            let loop_change = from_loop != to_loop;
            let loop_entry = loop_change
                && from_loop.is_none_or(|fl| self.member_of_loops(to_bb).any(|(_, tl)| fl == tl));
            let problem_entry = loop_entry && to.member_of_loop_non_framing() != from_loop;

            let style = if back_edge { "dashed" } else { "solid" };
            let constraint = if back_edge { "false" } else { "true" };
            let color = if problem_entry { "red" } else { "black" };
            format!("style={style:?} color={color:?} constraint={constraint:?}")
        };
        let gna = |_, bbr: BasicBlockRef<'_, '_>| {
            let bbd = bbr.0 .1;
            let style = match bbd.is_dead() {
                true => "dashed",
                false => "solid",
            };
            let color = match &bbd.loop_member {
                Ok(..) => "forestgreen",
                Err(lm) if !lm.loop_exit.is_empty() => {
                    if bbd.back_edge_to().is_some() {
                        "purple"
                    } else {
                        "blue"
                    }
                }
                Err(..) if bbd.back_edge_to().is_some() => "orange",
                Err(..) => "black",
            };
            format!("shape=box color={color:?} style={style:?}")
        };
        let dot = Dot::with_attr_getters(&graph, &[Config::EdgeNoLabel], &gea, &gna);
        write!(f, "{dot:?}")
    }
}

impl BasicBlockData<'_> {
    fn edges_iter(&self) -> EdgesIter<'_> {
        if self.dead_branch == Err(false) {
            EdgesIter::FalseFirst(self.edges().iter().rev())
        } else {
            EdgesIter::TrueFirst(self.edges().iter())
        }
    }
}

pub enum EdgesIter<'a> {
    TrueFirst(core::slice::Iter<'a, BasicBlock>),
    FalseFirst(core::iter::Rev<core::slice::Iter<'a, BasicBlock>>),
}

impl Iterator for EdgesIter<'_> {
    type Item = BasicBlock;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            EdgesIter::TrueFirst(it) => it.next().copied(),
            EdgesIter::FalseFirst(it) => it.next().copied(),
        }
    }
}

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
    type NodeReferences = core::iter::Map<
        typed_index_collections::TiEnumerated<
            core::slice::Iter<'r, BasicBlockData<'a>>,
            BasicBlock,
            &'r BasicBlockData<'a>,
        >,
        fn((BasicBlock, &'r BasicBlockData<'a>)) -> BasicBlockRef<'r, 'a>,
    >;

    fn node_references(self) -> Self::NodeReferences {
        self.blocks.iter_enumerated().map(BasicBlockRef)
    }
}

#[derive(Clone, Copy)]
pub struct BasicBlockRef<'r, 'a>((BasicBlock, &'r BasicBlockData<'a>));

impl<'a> NodeRef for BasicBlockRef<'_, 'a> {
    type NodeId = BasicBlock;
    type Weight = BasicBlockData<'a>;

    fn id(&self) -> Self::NodeId {
        self.0 .0
    }

    fn weight(&self) -> &Self::Weight {
        self.0 .1
    }
}

impl<'r, 'a> IntoEdgeReferences for &'r CfgData<'a, '_> {
    type EdgeRef = BasicBlockEdgeRef;
    type EdgeReferences = core::iter::FlatMap<
        typed_index_collections::TiEnumerated<
            core::slice::Iter<'r, BasicBlockData<'a>>,
            BasicBlock,
            &'r BasicBlockData<'a>,
        >,
        EdgeRefs<'r>,
        fn((BasicBlock, &'r BasicBlockData<'a>)) -> EdgeRefs<'r>,
    >;

    fn edge_references(self) -> Self::EdgeReferences {
        fn edges<'r>((i, b): (BasicBlock, &'r BasicBlockData)) -> EdgeRefs<'r> {
            EdgeRefs(i, b.edges_iter())
        }
        self.blocks.iter_enumerated().flat_map(edges)
    }
}

pub struct EdgeRefs<'r>(BasicBlock, EdgesIter<'r>);

impl Iterator for EdgeRefs<'_> {
    type Item = BasicBlockEdgeRef;
    fn next(&mut self) -> Option<Self::Item> {
        self.1.next().map(|to| BasicBlockEdgeRef((self.0, to)))
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
    fn reset_map(&self, map: &mut Self::Map) {
        map.fill(false);
    }
}

impl<'r> IntoNeighbors for &'r CfgData<'_, '_> {
    type Neighbors = EdgesIter<'r>;
    fn neighbors(self, a: BasicBlock) -> Self::Neighbors {
        self[a].edges_iter()
    }
}

impl<'r> IntoNeighborsDirected for &'r CfgData<'_, '_> {
    type NeighborsDirected = EdgesIter<'r>;
    fn neighbors_directed(
        self,
        a: BasicBlock,
        dir: petgraph::Direction,
    ) -> Self::NeighborsDirected {
        match dir {
            petgraph::Direction::Incoming => EdgesIter::TrueFirst(self[a].predecessors.iter()),
            petgraph::Direction::Outgoing => self.neighbors(a),
        }
    }
}
