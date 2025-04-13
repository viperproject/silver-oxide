use crate::{program::LocalDefId, TiVec};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Program(pub(super) TiVec<LocalDefId, Declaration>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrePostDec {
    Pre(Exp),
    Post(Exp),
    Decreases(Decreases),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Decreases {
    pub kind: Option<DecreasesKind>,
    pub guard: Option<Exp>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DecreasesKind {
    Star,
    Underscore,
    Exp(Vec<Exp>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ident(pub String);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IdnDecl(pub Ident);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Declaration {
    Import(Import),
    Define(Define),
    Domain(Domain),
    DomainElement(DomainElement),
    Field(Field),
    Function(Function),
    Predicate(Predicate),
    Method(Method),
    Adt(Adt),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DomainElement {
    pub domain: Ident,
    pub kind: DomainElementKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DomainElementKind {
    Function(DomainFunction),
    Axiom(Axiom),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Axiom {
    pub name: Option<IdnDecl>,
    pub exp: ExpBlock,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Import {
    pub path: String,
    pub local: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Define {
    pub name: IdnDecl,
    pub args: Vec<IdnDecl>,
    pub body: ExpOrBlock,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExpOrBlock {
    Exp(Exp),
    Block(StmtBlock),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IdnDeclTyped {
    pub idn: IdnDecl,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ArgOrType {
    Arg(IdnDeclTyped),
    Type(Type),
}

impl ArgOrType {
    pub fn ty(&self) -> &Type {
        match self {
            ArgOrType::Arg(id) => &id.ty,
            ArgOrType::Type(ty) => ty,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block<T>(pub T);

pub type HeapExpBlock = Block<HeapExp>;
pub type ExpBlock = Block<Exp>;
pub type StmtBlock = Block<Vec<Statement>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResourceExp {
    pub cond: Vec<(bool, Exp)>,
    pub acc: AccExp,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HeapExp {
    /// Empty when parsed, later stages pull out all `acc` expressions here.
    pub res: Vec<ResourceExp>,
    pub exp: Exp,
}

pub type Exp = Box<ExpKind>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExpKind {
    Const(ConstKind),
    Result,
    // At(Ident, Exp),
    Old(Option<Ident>, Exp),
    // Lhs(Exp),
    Ascribe(Exp, Type),
    /// unfolding(e) in E
    HeapUpdate(HeapUpdateOp, AccExp, Exp),
    /// forall/exists x: T, y: U, ... :: { trigger } e
    Quantifier(QuantifierKind, Vec<IdnDeclTyped>, Vec<Trigger>, Exp),
    /// let x = e1 in e2
    LetIn(IdnDecl, Exp, Exp),
    /// Quantified permissions. forperm x: T, y: U, ... [Perm] :: e1
    ForPerm(Vec<IdnDeclTyped>, ResAccess, Exp),
    /// acc(e)
    /// Moved to `ResourceExp` after desugaring.
    Acc(AccExp),
    /// f(e1, e2, ..., en)
    FuncApp(Ident, Vec<Exp>),
    /// x
    Ident(Ident),
    /// e1 op e2
    BinOp(BinOp, Exp, Exp),
    /// c ? e1 : e2
    Ternary(Exp, Exp, Exp),
    /// e.f
    /// Replaced with `FuncApp` after desugaring.
    Field(Exp, Ident),
    /// e[e1]
    Index(Exp, IndexOp),
    /// op e
    UnOp(UnOp, Exp),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ConstKind {
    Bool(bool),
    Int(num::BigInt),
    Real(num::BigRational),
    Null,
    Epsilon,
    Wildcard,
    Heap(ConstHeapKind),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ConstHeapKind {
    /// The heap initialised from the precondition
    Old,
    /// An empty heap which has been initialised from a `HeapExp`
    SelfFraming,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HeapUpdateOp {
    Unfold,
    Fold,
    Apply,
    Package,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum QuantifierKind {
    Forall,
    Exists,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AccExp {
    pub acc: LocAccess,
    pub perm: Exp,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinOp {
    /// Replaced with `Ternary` after desugaring.
    Implies,
    /// Replaced with `Ternary` after desugaring.
    Or,
    /// Replaced with `Ternary` after desugaring.
    And,
    Iff,
    Eq,
    Neq,
    Lt,
    Le,
    /// Replaced with swapped `Lt` after desugaring.
    Gt,
    /// Replaced with swapped `Le` after desugaring.
    Ge,
    In,
    Plus,
    Minus,
    Mult,
    Div,
    Mod,
    IntDiv,
    Union,
    SetMinus,
    Intersection,
    Subset,
    Concat,
    MagicWand,
    Range,
    InhaleExhale,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnOp {
    Not,
    Neg,
    IntToReal,
    Abs,
    Deref,
    Perm,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Trigger {
    pub exp: Vec<Exp>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResAccess {
    Loc(LocAccess),
    Exp(AccExp),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Statement {
    Assume(Exp),
    Assert(Exp),
    Refute(Exp),
    Inhale(HeapExp),
    Exhale(HeapExp),
    Fold(AccExp),
    Unfold(AccExp),
    Goto(Ident),
    Label(IdnDecl, Invariant),
    Havoc(LocAccess),
    QuasiHavoc(Option<Exp>, Exp),
    QuasiHavocAll(Vec<IdnDeclTyped>, Option<Exp>, Exp),
    Var(Vec<IdnDeclTyped>, Option<AssignRhs>),
    While(Exp, Invariant, Vec<Decreases>, StmtBlock),
    If(Exp, StmtBlock, Option<StmtBlock>),
    Package(AccExp, Option<StmtBlock>),
    Apply(AccExp),
    Assign(Vec<Exp>, AssignRhs),
    Block(StmtBlock),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AssignRhs {
    Exp(Exp),
    Call(Ident, Vec<Exp>),
    New(StarOrNames),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StarOrNames {
    Star,
    Names(Vec<Ident>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IndexOp {
    Index(Exp),
    LowerBound(Exp),
    UpperBound(Exp),
    Range(Exp, Exp),
    Assign(Exp, Exp),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Invariant(pub HeapExp);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum WhileSpec {
    Inv(Invariant),
    Dec(Decreases),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LocAccess {
    /// Must be either `Exp::Field` or `Exp::FuncApp`.
    pub loc: Exp,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field(pub Signature);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Domain {
    pub name: IdnDecl,
    pub interpretation: Vec<(Ident, String)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub signature: Signature,
    pub contract: Contract,
    pub body: Option<ExpBlock>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Contract {
    pub precondition: HeapExp,
    pub postcondition: HeapExp,
    pub decreases: Vec<Decreases>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DomainFunction {
    pub unique: bool,
    pub signature: Signature,
    pub interpretation: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Signature {
    pub name: IdnDecl,
    pub args: Vec<ArgOrType>,
    pub ret: Vec<ArgOrType>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Bool,
    Int,
    Real,
    Ref,
    Domain(Ident, Vec<Type>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Predicate {
    pub signature: Signature,
    pub body: Option<HeapExpBlock>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Method {
    pub signature: Signature,
    pub contract: Contract,
    pub body: Option<StmtBlock>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Adt {
    pub name: IdnDecl,
    pub args: Vec<Type>,
    pub variants: Vec<Variant>,
    pub derives: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variant {
    pub name: IdnDecl,
    pub fields: Vec<IdnDeclTyped>,
}
