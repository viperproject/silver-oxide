use crate::{program::idx::LocalDefId, TiVec};

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
    Block(Block),
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
pub struct ExpBlock(pub Exp);

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
    Int(num_bigint::BigInt),
    Null,
    None,
    Write,
    Epsilon,
    Wildcard,
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
    /// `Ok` is of type `Real` and represents the actual permission, `Err` is of
    /// type `Bool` and represents a wildcard permission if true.
    pub perm: Result<Exp, Exp>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinOp {
    /// Replaced with `Ternary` after desugaring.
    Implies,
    /// Replaced with `Ternary` after desugaring.
    Or,
    And,
    Iff,
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
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
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Statement {
    Assume(Exp),
    Assert(Exp),
    Refute(Exp),
    Inhale(Exp),
    Exhale(Exp),
    Fold(Exp),
    Unfold(Exp),
    Goto(Ident),
    Label(IdnDecl, Vec<Invariant>),
    Havoc(LocAccess),
    QuasiHavoc(Option<Exp>, Exp),
    QuasiHavocAll(Vec<IdnDeclTyped>, Option<Exp>, Exp),
    Var(Vec<IdnDeclTyped>, Option<Exp>),
    While(Exp, Vec<WhileSpec>, Block),
    If(Exp, Block, Vec<(Exp, Block)>, Option<Block>),
    Wand(Ident, Exp),
    Package(AccExp, Option<Block>),
    Apply(AccExp),
    Assign(Vec<Exp>, Exp),
    Fresh(Vec<Ident>),
    Constraining(Vec<Ident>, Block),
    Block(Block),
    New(Ident, StarOrNames),
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
pub struct Invariant(pub Exp);

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
    pub precondition: Exp,
    pub postcondition: Exp,
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
    pub body: Option<ExpBlock>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Method {
    pub signature: Signature,
    pub contract: Contract,
    pub body: Option<Block>,
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
