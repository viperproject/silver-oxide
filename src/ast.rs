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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(pub String);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Declaration {
    Import(Import),
    Define(Define),
    Domain(Domain),
    Field(Field),
    Function(Function),
    Predicate(Predicate),
    Method(Method),
    Adt(Adt),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Axiom {
    pub name: Option<Ident>,
    pub exp: Exp,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Import {
    pub path: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Define {
    pub name: Ident,
    pub args: Vec<Ident>,
    pub body: ExpOrBlock,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExpOrBlock {
    Exp(()),
    Block(()),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ArgOrType {
    Arg((Ident, Type)),
    Type(Type),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Exp {
    True,
    False,
    Int,
    Null,
    Result,
    At(Ident, Box<Exp>),
    Old(Option<Ident>, Box<Exp>),
    Lhs(Box<Exp>),
    None,
    Write,
    Epsilon,
    Wildcard,
    Ascribe(Box<Exp>, Type),
    Perm(Box<LocAccess>),
    Unfolding(Box<AccExp>, Box<Exp>),
    Folding(Box<AccExp>, Box<Exp>),
    Applying(Box<Exp>, Box<Exp>),
    Packaging(Box<Exp>, Box<Exp>),
    Forall(Vec<(Ident, Type)>, Vec<Trigger>, Box<Exp>),
    Exists(Vec<(Ident, Type)>, Vec<Trigger>, Box<Exp>),
    SeqConstructor(SeqConstructor),
    SetConstructor(SetConstructor),
    MapConstructor(MapConstructor),
    Abs(Box<Exp>),
    LetIn(Ident, Box<Exp>, Box<Exp>),
    ForPerm(Vec<(Ident, Type)>, Box<ResAccess>, Box<Exp>),
    Acc(Box<AccExp>),
    FuncApp(Box<Exp>, Vec<Exp>),
    Ident(Ident),
    BinOp(BinOp, Box<Exp>, Box<Exp>),
    Ternary(Box<Exp>, Box<Exp>, Box<Exp>),
    Field(Box<Exp>, Ident),
    Index(Box<Exp>, Box<IndexOp>),
    Neg(Box<Exp>),
    Not(Box<Exp>),
    InhaleExhale(Box<Exp>, Box<Exp>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SetConstructor {
    Empty(Type),
    NonEmpty(Vec<Exp>),
    MultisetEmpty(Type),
    MultisetNonEmpty(Vec<Exp>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SeqConstructor {
    Empty(Type),
    NonEmpty(Vec<Exp>),
    Range(Box<Exp>, Box<Exp>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MapConstructor {
    Empty(Type, Type),
    NonEmpty(Vec<(Exp, Exp)>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AccExp {
    Acc(LocAccess, Option<Exp>),
    PredicateAccess(Exp),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOp {
    Implies,
    Iff,
    And,
    Or,
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
    PermDiv,
    Union,
    SetMinus,
    Intersection,
    Subset,
    Concat,
    MagicWand,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Trigger {
    pub exp: Vec<Exp>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResAccess {
    Loc(LocAccess),
    Exp(Exp),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Statement {
    Assert(Exp),
    Refute(Exp),
    Assume(Exp),
    Inhale(Exp),
    Exhale(Exp),
    Fold(AccExp),
    Unfold(AccExp),
    Goto(Ident),
    Label(Ident, Vec<Invariant>),
    Havoc(LocAccess),
    QuasiHavoc(Option<Exp>, Exp),
    QuasiHavocAll(Vec<(Ident, Type)>, Option<Exp>, Exp),
    Var(Vec<(Ident, Type)>, Option<Exp>),
    While(Exp, Vec<WhileSpec>, Block),
    If(Exp, Block, Vec<(Exp, Block)>, Option<Block>),
    Wand(Ident, Exp),
    Package(Exp, Option<Block>),
    Apply(Exp),
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
    pub loc: Exp,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
    pub fields: Vec<(Ident, Type)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Domain {
    pub name: Ident,
    pub interpretation: Vec<(Ident, String)>,
    pub elements: Vec<DomainElement>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub signature: Signature,
    pub contract: Contract,
    pub body: Option<Exp>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Contract {
    pub preconditions: Vec<Exp>,
    pub postconditions: Vec<Exp>,
    pub decreases: Vec<Decreases>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DomainElement {
    DomainFunction(DomainFunction),
    Axiom(Axiom),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DomainFunction {
    pub unique: bool,
    pub signature: Signature,
    pub interpretation: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Signature {
    pub name: Ident,
    pub args: Vec<ArgOrType>,
    pub ret: Vec<ArgOrType>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    Bool,
    Perm,
    Ref,
    Rational,
    Seq(Box<Type>),
    Set(Box<Type>),
    Map(Box<Type>, Box<Type>),
    User(Ident, Vec<Type>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Predicate {
    pub signature: Signature,
    pub body: Option<Exp>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Method {
    pub signature: Signature,
    pub contract: Contract,
    pub body: Option<Block>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Adt {
    pub name: Ident,
    pub args: Vec<Type>,
    pub variants: Vec<Variant>,
    pub derives: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variant {
    pub name: Ident,
    pub fields: Vec<(Ident, Type)>,
}
