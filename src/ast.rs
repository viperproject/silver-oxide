pub enum PrePostDec {
    Pre(Exp),
    Post(Exp),
    Decreases(Decreases),
}

pub struct Decreases {
    pub kind: Option<DecreasesKind>,
    pub guard: Option<Exp>,
}

pub enum DecreasesKind {
    Star,
    Underscore,
    Exp(Vec<Exp>),
}

pub struct Ident(pub String);

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

pub struct Axiom {
    pub name: Option<Ident>,
    pub exp: Exp,
}

pub struct Import {
    pub path: String,
}
pub struct Define {
    pub name: Ident,
    pub args: Vec<Ident>,
    pub body: ExpOrBlock,
}

pub enum ExpOrBlock {
    Exp(()),
    Block(()),
}

pub enum ArgOrType {
    Arg((Ident, Type)),
    Type(Type),
}

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

pub enum SetConstructor {
    Empty(Type),
    NonEmpty(Vec<Exp>),
    MultisetEmpty(Type),
    MultisetNonEmpty(Vec<Exp>),
}

pub enum SeqConstructor {
    Empty(Type),
    NonEmpty(Vec<Exp>),
    Range(Box<Exp>, Box<Exp>),
}

pub enum MapConstructor {
    Empty(Type, Type),
    NonEmpty(Vec<(Exp, Exp)>),
}

pub enum AccExp {
    Acc(LocAccess, Option<Exp>),
    PredicateAccess(Exp),
}

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

pub struct Trigger {
    pub exp: Vec<Exp>,
}

pub enum ResAccess {
    Loc(LocAccess),
    Exp(Exp),
}

pub struct Block {
    pub statements: Vec<Statement>,
}

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

pub enum StarOrNames {
    Star,
    Names(Vec<Ident>),
}

pub enum IndexOp {
    Index(Exp),
    LowerBound(Exp),
    UpperBound(Exp),
    Range(Exp, Exp),
    Assign(Exp, Exp),
}

pub struct Invariant(pub Exp);

pub enum WhileSpec {
    Inv(Invariant),
    Dec(Decreases),
}

pub struct LocAccess {
    pub loc: Exp,
}

pub struct Field {
    pub fields: Vec<(Ident, Type)>,
}

pub struct Domain {
    pub name: Ident,
    pub interpretation: Vec<(Ident, String)>,
    pub elements: Vec<DomainElement>,
}

pub struct Function {
    pub signature: Signature,
    pub contract: Contract,
    pub body: Option<Exp>,
}

pub struct Contract {
    pub preconditions: Vec<Exp>,
    pub postconditions: Vec<Exp>,
    pub decreases: Vec<Decreases>,
}

pub enum DomainElement {
    DomainFunction(DomainFunction),
    Axiom(Axiom),
}

pub struct DomainFunction {
    pub unique: bool,
    pub signature: Signature,
    pub interpretation: Option<String>,
}

pub struct Signature {
    pub name: Ident,
    pub args: Vec<ArgOrType>,
    pub ret: Vec<ArgOrType>,
}

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

pub struct Predicate {
    pub signature: Signature,
    pub body: Option<Exp>,
}

pub struct Method {
    pub signature: Signature,
    pub contract: Contract,
    pub body: Option<Block>,
}

pub struct Adt {
    pub name: Ident,
    pub args: Vec<Type>,
    pub variants: Vec<Variant>,
    pub derives: Vec<String>,
}

pub struct Variant {
    pub name: Ident,
    pub fields: Vec<(Ident, Type)>,
}
