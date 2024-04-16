peg::parser! {
    pub grammar silver_parser() for str {
        rule _ = quiet! { ___ __ ** ___ ___ }

        rule white_space() = quiet! { " " / "\t" / "\n" / "\r\n" } / expected!("whitespace")
        rule ___ = white_space()*

        rule __ = "//" (! "\n" [_])* / "/*" (! "*/" [_])* "*/"

        rule start_char() -> &'input str
            = $(['A'..='Z' | 'a'..='z'| '$' | '_' ])

        rule char() -> &'input str
            = $(['A'..='Z' | 'a'..='z'| '$' | '_' | '\'' | '0'..='9' ])

        rule reserved()
            = "havoc" / "true" / "false"
            / "Set" / "Seq" / "Map" / "Multiset" / "Int" / "Bool" / "Perm" / "Ref" / "Rational"
            / "forperm" / "let" / "in" / "if" / "else" / "elseif" / "while" / "do"
            / "assert" / "assume" / "havoc"
            / "return" / "continue" / "skip"
            / "forall" / "exists"
            / "inhale" / "exhale" / "unfold" / "fold" / "acc"
            / "none" / "wildcard" / "write" / "epsilon"
            / "requires" / "ensures" / "returns" / "decreases" / "result"

        rule ident() -> Ident
            = quiet! { !(reserved() white_space()) n:$(start_char() char()*) { Ident(n.to_string())} }
            / expected!("identifier")

        rule kw<R>(r: rule<R>) -> () = r() !char()

        rule integer() -> () = "-"? ['0'..='9']+

        rule comma() = _ "," _

        rule annotation() = quiet! { "@" annotation_ident() annotation_args() }  / expected!("annotation")

        rule annotation_ident() = ident() ++ "."

        rule annotation_args() = "(" _ string_lit() ** comma() _ ")"

        rule annotated<R>(r: rule<R>) = annotation() ** _ _ r()

        /// Types

        rule type_() -> Type
            =
             type_constr() 
            / "Int" { Type::Int }
            / "Bool" { Type::Bool }
            / "Perm" { Type::Perm }
            / "Ref" { Type::Ref }
            / "Rational"   { Type::Rational }
            / "Seq" _ "[" _ ty:type_() _ "]" { Type::Seq(Box::new(ty)) }
            / "Set" _ "[" _ ty:type_() _ "]" { Type::Set(Box::new(ty)) }
            / "Map" _ "[" _ a:type_() _ "," _ b:type_() _ "]" { Type::Map(Box::new(a), Box::new(b))}

        rule type_constr() -> Type = nm:ident() _ tys:("[" _ tys:(type_() ** comma()) _ "]" { tys })?
            {   let tys = tys.unwrap_or_default();
                Type::User(nm, tys) 
            }

        rule formal_arg() -> () = ident() _ ":" _ type_() / type_() { }

        /// Accessors

        rule predicate_access() -> () = func_app()

        // TODO: only accept expressions that end with a .field
        rule field_access() = suffix_exp()

        rule loc_access() = field_access() / predicate_access()

        rule res_access() = magic_wand_exp() / loc_access()

        // acc_exp = { "acc"~"("~loc_access~(","~exp)?~")" | predicate_access }
        rule acc_exp() -> () = "acc" _ "(" _ loc_access() _ ("," _ exp())? _ ")" / predicate_access()

        rule trigger() = "{" _ exp() ** comma() _ "}"


        /// Expressions

        rule seq_op_exp()
            = exp() _ "[" _ exp() _ "]"
            / exp() _ "[" _ ".." _ exp() _ "]"
            / exp() _ "[" _ exp() _ ".." _ "]"
            / exp() _ "[" _ exp() _ ".." _ exp() _ "]"
            / exp() _ "[" _ exp() _ ":=" _ exp() _ "]"

        rule set_constructor_exp() -> ()
            = "Set" _ "[" _ type_() _ "]" _ "(" _ ")"
            / "Set" _ "(" _ exp() ** comma() _ ")"
            / "Multiset" _ "[" _ type_() _ "]" _ "(" _ ")"
            / "Multiset" _ "(" _ exp() ** comma() _ ")"

        rule seq_constructor_exp() -> ()
            = "Seq" _ "[" _ type_() _ "]" _ "(" _ ")"
            / "Seq" _ "(" _ exp() ** comma() _ ")"
            / "[" _ exp() _ ".." _ exp() _ ")"

        rule map_constructor_exp() -> ()
            = "Map" _ "[" _ type_() _ "," _ type_() _ "]" _ "(" _ ")"
            / "Map" _ "(" _ (exp() _ ":=" _ exp()) ** comma() _ ")"

        rule forperm_exp() -> ()= "forperm" _ formal_arg() ++ comma() _ "[" _ res_access() _ "]" _ "::" _ exp()

        rule let_in_exp() -> () = "let" _ ident() _ "==" _ "(" _ exp() _ ")" _ "in" _ exp()

        rule magic_wand_exp() -> () = exp()

        rule func_app() -> () = ident() (" ")* "(" _ exp() ** comma() _ ")"

        rule atom() -> ()
            = kw(<"true">) / kw(<"false">)
            / integer()
            / kw(<"null">)
            / kw(<"result">)
            / "(" _ exp() _ (":" _ type_())? _ ")"
            / "old" _ ("[" _ ident() _ "]")? _ "(" _ exp() _ ")"
            / "[" _ ident() _ "]" _ "(" _ exp() _ ")"
            / "lhs" _ "(" _ exp() _ ")"
            / kw(<"none">) / kw(<"write">) / kw(<"epsilon">) / kw(<"wildcard">)
            / "perm" _ "(" _ loc_access() _ ")"
            / "[" _ exp() _ "," _ exp() _ "]"
            / "unfolding" _ acc_exp() _ "in" _ exp()
            / "folding" _ acc_exp() _ "in" _ exp()
            / "applying" _ "(" _ magic_wand_exp() _ ")" _ "in" _ exp()
            / "packaging" _ "(" _ magic_wand_exp() _ ")" _ "in" _ exp()
            / "forall" _ formal_arg() ++ comma() _ "::" _ trigger()**_ _ exp()
            / "exists" _ formal_arg() ++ comma() _ "::" _ trigger()**_ _ exp()
            / seq_constructor_exp()
            / set_constructor_exp()
            / map_constructor_exp()
            / "|" _ exp() _ "|"
            / let_in_exp()
            / forperm_exp()
            / acc_exp()
            / func_app()
            / ident()



        rule full_exp() -> () = precedence! {
            x:@ (_ "?" _ exp() _ ":" _) y:(@) {}
            --
            x:@ (_ "<==>" _) y:(@) {}
            --
            x:@ (_ "==>" _) y:(@) {}
            --
            x:@ (_ "--*" _) y:(@) {}
            --
            x:@ (_ "||" _) y:(@) {}
            --
            x:@ (_ "&&" _) y:(@) {}
            --
            x:@ (_ "!=" _) y:(@) {}
            x:@ (_ "==" _) y:(@) {}
            --
            x:@ (_ "<=" _) y:(@) {}
            x:@ (_ ">=" _) y:(@) {}
            x:@ (_ ">" _) y:(@) {}
            x:@ (_ "<" _) y:(@) {}
            x:@ (_ "in" &(white_space()/ "(") _) y:(@) {}
            --
            x:(@) (_ "-" _) y:@ {}
            x:(@) (_ "+" _) y:@ {}
            x:(@) (_ "++" _) y:@ {}
            x:(@) (_ "union" white_space() _) y:@ {}
            x:(@) (_ "setminus" white_space() _) y:@ {}
            x:(@) (_ "intersection" white_space() _) y:@ {}
            x:(@) (_ "subset" white_space() _) y:@ {}
            --
            x:(@) (_ "*" _) y:@ {}
            x:(@) (_ "/" _) y:@ {}
            x:(@) (_ "%" _) y:@ {}
            x:(@) (_ "\\" _) y:@ {}
            --
            x:@ (_ "." ident()) {}
            x:@ (_ "[" _ seq_op() _ "]" _) {}
            "-" _ x:@ {}
            "!" _ x:@ {}
            --
            annotated(<atom()>) {}
        }

        rule exp() -> () = annotated(<full_exp()>) {}

        rule suffix_exp() = atom() _ ("." ident() / "[" _ exp() _ "]") ** _

        rule seq_op() = ".." _ exp() / exp() _ (".." _ exp()? / ":=" _ exp())?

        rule paren_list<R>(r : rule<R>) -> () = "(" _ r() ** comma() _ ")" { () }

        /// Statements

        rule block() -> () = "{" _ (annotated(<statement()>) opt_semi())* "}"

        rule statement() -> ()
            = kw(<"assert">) _ exp()
            / kw(<"refute">) _ exp()
            / kw(<"assume">) _ exp()
            / kw(<"inhale">) _ exp()
            / kw(<"exhale">) _ exp()
            / kw(<"fold">) _ acc_exp()
            / kw(<"unfold">) _ acc_exp()
            / kw(<"goto">) _ ident()
            / kw(<"label">) _ ident() _ invariant() ** _
            / kw(<"havoc">) _ loc_access()
            / kw(<"quasihavoc">) _ (exp() _ "==>")? _ exp()
            / kw(<"quasihavocall">) _ formal_arg() ++ _ _ "::" _ (exp() _ "==>")? _ exp()
            / kw(<"var">) _ formal_arg() ** comma() _ (":=" _ exp())?
            / while_statement()
            / if_statement()
            / wand_statement()
            / ident() _ ":=" _ "new" _ "(" _ "*" _ ")"
            / ident() _ ":=" _ "new" _ "(" _ ident() ** comma() _ ")"
            / call_statement()
            / fresh_statement()
            / constraining_block()
            / block()


        rule semi() = _ ";" _
        rule opt_semi() = _ ";"? _

        rule tupled<R>(r: rule<R>) -> Vec<R> = "(" _ res:(r() ** comma()) _ ")" { res }
        rule bracketed<R>(r: rule<R>) -> Vec<R> = "[" _ res:(r() ** comma()) _ "]" { res }

        rule while_statement() -> () = "while" _ "(" _ exp() _ ")" _ ((invariant() / decreases())  opt_semi())* _ block()

        rule invariant() = "invariant" _ exp()

        rule if_statement() -> () = "if" _ "(" _ exp() _ ")" _ block() _ ("elseif" _ "(" _ exp() _ ")" _ block() _)** _ _ ("else" _ block())?

        // call_statement = { assign_target~((","~assign_target)*~":="~exp)? }
        rule call_statement() -> () = (assign_target() ++ comma() _ ":=")? _ exp()

        // rule assign_target() = field_access() / func_app() / ident()

        rule assign_target() =
             atom() _ ("(" _ exp() ** comma() _ ")")? _ ("." ident() / "[" _ exp() _ "]") ** _

        rule fresh_statement() -> () = "fresh" _ ident() ++ comma()

        rule wand_statement() -> () = "wand" _ ident() _ ":" _ exp()
            / "package" _ magic_wand_exp() _ block()?
            / "apply" _ magic_wand_exp()

        rule constraining_block() -> () = "constraining" _ "(" _ ident() ++ comma() _ ")" _ block()

        rule expression_or_block() -> ExpOrBlock = exp()  { ExpOrBlock::Exp(()) } / block() { ExpOrBlock::Block(()) }

        /// Declarations

        pub rule sil_program() = _ annotated(<declaration()>) ** opt_semi() _

        rule declaration() -> Declaration 
            = i:import() { Declaration::Import(i) } 
            / d:define() { Declaration::Define(d) }
            / d:domain()  { Declaration::Domain(d) }
            // / field() 
            // / function() 
            // / predicate() 
            // / method() 
            // / adt()

        rule import() -> Import = "import" _ r:("<" _ r:relative_path() _ ">" { r } / "\"" _ r:relative_path() _ "\"" { r })
            { Import { path: r } }

        rule define() -> Define = "define" _ nm:ident() _ ids:(tupled(<ident()>))? _ body:expression_or_block()
            { Define { name: nm, args: ids.unwrap_or_default(), body } }

        rule domain() -> Domain = 
            "domain" _ 
            name:ident() _ 
            params:(bracketed(<ident()>))? _
            interp:domain_interpretation()? _ 
            "{" _ annotated(<domain_element()>) ** opt_semi() _ "}" 
        { Domain { name, interpretation: interp.unwrap_or_default(), elements: todo!() } }

        rule domain_interpretation() -> Vec<(Ident, String)> = "interpretation" _ "(" _ rs:((i:ident() _ ":" _ s:string_lit() { (i, s)})++comma()) _ ")"
            { rs }

        rule domain_element() -> () = domain_function() {} / axiom() {}

        // TODO(xavier): factor out semi colon stuff
        rule domain_function() = "unique"? _ function_signature() _ func_interpretation()?

        rule function_signature() = "function" _ ident() _ "(" _ formal_arg() ** comma() _ ")" _ ":" _ type_()

        rule func_interpretation() = "interpretation" _ string_lit()

        rule field() -> () = "field" _( ident() _ ":" _ type_()) ** comma()

        rule function() -> () = function_signature() _ contract()  _ ("{" _ exp() _ "}")?

        rule predicate() -> () = "predicate" _ ident() _ paren_list(<formal_arg()>) _ ("{" _ exp() _ "}")?

        rule formal_returns() = "returns" _ paren_list(<formal_arg()>)
        rule method() -> () = "method" _ ident() _ paren_list(<formal_arg()>) _ formal_returns()? _ contract() _ block()?

        rule adt() -> () = "adt" _ type_constr() _ adt_variants() _ derives()?

        rule adt_variants() = "{" _ (ident() _ paren_list(<formal_arg()>) _)* _ "}"

        rule derives() = "derives" _ "{" _ (!"}" [_])* _ "}"

        rule relative_path() -> String = s:$(['~' | '.']? (['/']? ['.' | 'A'..='Z' | 'a'..='z' | '0'..='9' | '_' | '\\' | '-' | ' '])+) { s.to_string() }

        rule string_lit() -> String = str:$("\"" _ (!"\"" [_])* _ "\"") { str.to_string() }

        rule axiom() = "axiom" _ ident()? _ "{" _ exp() _ "}"

        rule precondition() = "requires" _ exp()

        rule postcondition() = "ensures" _ exp()

        rule decreases() = "decreases" _ ("*" / "_" / exp() ** comma())? _ ("if" _ exp())?

        rule contract() =
            ((precondition() / decreases()) opt_semi())* _ ((postcondition() / decreases()) opt_semi())*


    }
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

pub struct Import {
    path: String,
}
pub struct Define {
    name: Ident,
    args: Vec<Ident>,
    body: ExpOrBlock,
}

pub enum ExpOrBlock {
    Exp(()),
    Block(()),
}

pub enum Exp {

}

pub struct Block {
    statements: Vec<Statement>,
}

pub enum Statement {
    Assert(Exp),
    Refute(Exp),
    Assume(Exp),
    Inhale(Exp),
    Exhale(Exp),
    Fold(Exp),
    Unfold(Exp),
    Goto(Ident),
    Label(Ident, Vec<Invariant>),
    Havoc(LocAccess),
    QuasiHavoc(Option<Exp>, Exp),
    QuasiHavocAll(Vec<Ident>, Option<Exp>, Exp),
    Var(Vec<Ident>, Option<Exp>),
    While(Exp, Vec<Invariant>, Block),
    If(Exp, Block, Vec<(Exp, Block)>, Option<Block>),
    Wand(Ident, Exp),
    Package(Exp, Option<Block>),
    Apply(Exp),
    Fresh(Vec<Ident>),
    Constraining(Vec<Ident>, Block),
}

pub struct Invariant(Exp);

pub struct LocAccess {
    loc: Exp,
}


pub struct Field {
    fields: Vec<(String, String)>,
}

pub struct Domain {
    name: Ident,
    interpretation: Vec<(Ident, String)>,
    elements: Vec<DomainElement>,
}

pub struct Function {
    signature: Signature,
    contract: Contract,
    body: Option<String>,
}

pub struct Contract {
    preconditions: Vec<String>,
    postconditions: Vec<String>,
}

pub enum DomainElement {
    DomainFunction(DomainFunction),
    Axiom(String),
}

pub struct DomainFunction {
    unique: bool,
    signature: Signature,
    interpretation: Option<String>,
}

pub struct Signature {
    name: Ident,
    args: Vec<(Ident, Type)>,
    ret: Option<Type>,
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
    signature: Signature,
    body: String,
}

pub struct Method {
    signature: Signature,
    contract: Contract,
    body: Option<String>,
}

pub struct Adt {
    name: Ident,
    variants: Vec<(Ident, Vec<(String, String)>)>,
    derives: Vec<String>,
}