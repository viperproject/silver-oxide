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

        // TODO: insert annotations
        rule annotated<R>(r: rule<R>) -> R = annotation() ** _ _ r:r() { r }

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

        rule formal_arg() -> (Ident, Type) = id:ident() _ ":" _ ty:type_() { (id, ty) }

        rule decl_formal_arg() -> ArgOrType = a:formal_arg()  { ArgOrType::Arg(a) } / t:type_() { ArgOrType::Type(t) }
        /// Accessors

        rule predicate_access() -> Exp = func_app()

        // TODO: only accept expressions that end with a .field
        rule field_access() -> Exp = suffix_exp()

        rule loc_access() -> LocAccess = f:field_access() { LocAccess { loc: f } } / f:predicate_access() { LocAccess { loc: f } }

        rule res_access() -> ResAccess = e:magic_wand_exp() { ResAccess::Exp(e) } / l:loc_access() { ResAccess::Loc(l) }

        // acc_exp = { "acc"~"("~loc_access~(","~exp)?~")" | predicate_access }
        rule acc_exp() -> AccExp
            = "acc" _ "(" _ l:loc_access() _ e:("," _ e:exp() { e })? _ ")" { AccExp::Acc(l, e)}
            / p:predicate_access() { AccExp::PredicateAccess(p) }

        rule trigger() -> Trigger = "{" _ es:(exp() ** comma()) _ "}" { Trigger { exp: es } }


        /// Expressions

        rule set_constructor_exp() -> SetConstructor
            = "Set" _ "[" _ ty:type_() _ "]" _ "(" _ ")" { SetConstructor::Empty(ty) }
            / "Set" _ "(" _ es:(exp() ** comma()) _ ")" { SetConstructor::NonEmpty(es) }
            / "Multiset" _ "[" _ t:type_() _ "]" _ "(" _ ")"{ SetConstructor::MultisetEmpty(t) }
            / "Multiset" _ "(" _ es:(exp() ** comma()) _ ")" { SetConstructor::MultisetNonEmpty(es)  }

        rule seq_constructor_exp() -> SeqConstructor
            = "Seq" _ "[" _ ty:type_() _ "]" _ "(" _ ")" { SeqConstructor::Empty(ty) }
            / "Seq" _ "(" _ es:(exp() ** comma()) _ ")" { SeqConstructor::NonEmpty(es) }
            / "[" _ s:exp() _ ".." _ e:exp() _ ")" { SeqConstructor::Range(Box::new(s), Box::new(e)) }

        rule map_constructor_exp() -> MapConstructor
            = "Map" _ "[" _ a:type_() _ "," _ b:type_() _ "]" _ "(" _ ")" { MapConstructor::Empty(a, b) }
            / "Map" _ "(" _ es:((l:exp() _ ":=" _ r:exp() { (l, r)}) ** comma()) _ ")" { MapConstructor::NonEmpty(es) }

        rule forperm_exp() ->  Exp = "forperm" _ args:(formal_arg() ++ comma()) _ "[" _ res:res_access() _ "]" _ "::" _ exp:exp()
            { Exp::ForPerm(args, Box::new(res), Box::new(exp)) }

        rule let_in_exp() -> Exp = "let" _ id:ident() _ "==" _ "(" _ e:exp() _ ")" _ "in" _ body:exp()
            { Exp::LetIn(id, Box::new(e), Box::new(body)) }

        rule magic_wand_exp() -> Exp = exp()

        rule func_app() -> Exp = id:ident() (" ")* "(" _ es:(exp() ** comma()) _ ")" { Exp::FuncApp(Box::new(Exp::Ident(id)), es) }

        rule atom() -> Exp
            = kw(<"true">) { Exp::True } / kw(<"false">) { Exp::False }
            / integer() { Exp::Int }
            / kw(<"null">) { Exp::Null }
            / kw(<"result">) { Exp::Result }
            / "(" _ e:exp() _ ty:(":" _ ty:type_() { ty })? _ ")" { match ty {
                Some(ty) => Exp::Ascribe(Box::new(e), ty),
                None => e
                }
            }
            / kw(<"old">) _ i:("[" _ i:ident() _ "]" {i})? _ "(" _ e:exp() _ ")" { Exp::Old(i, Box::new(e)) }
            / "[" _ i:ident() _ "]" _ "(" _ e:exp() _ ")" { Exp::At(i, Box::new(e)) }
            / kw(<"lhs">) _ "(" _ e:exp() _ ")" { Exp::Lhs(Box::new(e)) }
            / kw(<"none">) { Exp::None }
            / kw(<"write">) { Exp::Write }
            / kw(<"epsilon">) { Exp::Epsilon}
            / kw(<"wildcard">) { Exp::Wildcard }
            / kw(<"perm">) _ "(" _ l:loc_access() _ ")" { Exp::Perm(Box::new(l)) }
            / "[" _ e:exp() _ "," _ f:exp() _ "]" { Exp::InhaleExhale(Box::new(e), Box::new(f))}

            / kw(<"unfolding">) _ acc:acc_exp() _ "in" _ e:exp() { Exp::Unfolding(Box::new(acc), Box::new(e)) }
            / kw(<"folding">) _ acc:acc_exp() _ "in" _ e:exp() { Exp::Folding(Box::new(acc), Box::new(e)) }

            / kw(<"applying">) _ "(" _ mwexp:magic_wand_exp() _ ")" _ "in" _ e:exp() { Exp::Applying(Box::new(mwexp), Box::new(e)) }
            / kw(<"packaging">) _ "(" _ mwexp:magic_wand_exp() _ ")" _ "in" _ e:exp() { Exp::Packaging(Box::new(mwexp), Box::new(e)) }
            / kw(<"forall">) _ args:(formal_arg() ++ comma()) _ "::" _ triggers:(trigger()**_) _ e:exp() { Exp::Forall(args, triggers, Box::new(e)) }
            / kw(<"exists">) _ args:(formal_arg() ++ comma()) _ "::" _ triggers:(trigger()**_) _ e:exp() { Exp::Exists(args, triggers, Box::new(e)) }

            / s:seq_constructor_exp() { Exp::SeqConstructor(s) }
            / s:set_constructor_exp() { Exp::SetConstructor(s) }
            / m:map_constructor_exp() { Exp::MapConstructor(m) }
            / "|" _ e:exp() _ "|" { Exp::Abs(Box::new(e)) }
            / let_in_exp()
            / forperm_exp()
            / a:acc_exp() { Exp::Acc(Box::new(a)) }
            / func_app()
            / i:ident() { Exp::Ident(i) }



        rule full_exp() -> Exp = precedence! {
            x:@ z:(_ "?" _ z:exp() _ ":" _ {z}) y:(@) { Exp::Ternary(Box::new(x), Box::new(z), Box::new(y)) }
            --
            x:@ (_ "<==>" _) y:(@) { Exp::BinOp(BinOp::Iff, Box::new(x), Box::new(y)) }
            --
            x:@ (_ "==>" _) y:(@) { Exp::BinOp(BinOp::Implies, Box::new(x), Box::new(y)) }
            --
            x:@ (_ "--*" _) y:(@) { Exp::BinOp(BinOp::MagicWand, Box::new(x), Box::new(y)) }
            --
            x:@ (_ "||" _) y:(@) { Exp::BinOp(BinOp::Or, Box::new(x), Box::new(y)) }
            --
            x:@ (_ "&&" _) y:(@) { Exp::BinOp(BinOp::And, Box::new(x), Box::new(y)) }
            --
            x:@ (_ "!=" _) y:(@) { Exp::BinOp(BinOp::Neq, Box::new(x), Box::new(y)) }
            x:@ (_ "==" _) y:(@) { Exp::BinOp(BinOp::Eq, Box::new(x), Box::new(y)) }
            --
            x:@ (_ "<=" _) y:(@) { Exp::BinOp(BinOp::Le, Box::new(x), Box::new(y)) }
            x:@ (_ ">=" _) y:(@) { Exp::BinOp(BinOp::Ge, Box::new(x), Box::new(y)) }
            x:@ (_ ">" _) y:(@) {  Exp::BinOp(BinOp::Gt, Box::new(x), Box::new(y)) }
            x:@ (_ "<" _) y:(@) { Exp::BinOp(BinOp::Lt, Box::new(x), Box::new(y)) }
            x:@ (_ "in" &(white_space() / "(") _) y:(@) { Exp::BinOp(BinOp::In, Box::new(x), Box::new(y))}
            --
            x:(@) (_ "-" _) y:@ { Exp::BinOp(BinOp::Minus, Box::new(x), Box::new(y)) }
            x:(@) (_ "+" _) y:@ { Exp::BinOp(BinOp::Plus, Box::new(x), Box::new(y)) }
            x:(@) (_ "++" _) y:@ { Exp::BinOp(BinOp::Concat, Box::new(x), Box::new(y)) }
            x:(@) (_ "union" white_space() _) y:@ { Exp::BinOp(BinOp::Union, Box::new(x), Box::new(y)) }
            x:(@) (_ "setminus" white_space() _) y:@ { Exp::BinOp(BinOp::SetMinus, Box::new(x), Box::new(y))}
            x:(@) (_ "intersection" white_space() _) y:@ { Exp::BinOp(BinOp::Intersection, Box::new(x), Box::new(y))}
            x:(@) (_ "subset" white_space() _) y:@ { Exp::BinOp(BinOp::Subset, Box::new(x), Box::new(y))}
            --
            x:(@) (_ "*" _) y:@ { Exp::BinOp(BinOp::Mult, Box::new(x), Box::new(y)) }
            x:(@) (_ "/" _) y:@ { Exp::BinOp(BinOp::Div, Box::new(x), Box::new(y)) }
            x:(@) (_ "%" _) y:@ { Exp::BinOp(BinOp::Mod, Box::new(x), Box::new(y)) }
            x:(@) (_ "\\" _) y:@ { Exp::BinOp(BinOp::PermDiv, Box::new(x), Box::new(y)) }
            --
            x:@ i:(_ "." i:ident() {i}) { Exp::Field(Box::new(x), i) }
            x:@ _ "[" _ s:seq_op() _ "]" _  { Exp::Index(Box::new(x), Box::new(s)) }
            "-" _ x:@ { Exp::Neg(Box::new(x)) }
            "!" _ x:@ { Exp::Not(Box::new(x)) }
            --
            a:annotated(<atom()>) {a}
        }

        rule exp() -> Exp = annotated(<full_exp()>)

        rule suffix_exp() -> Exp = a:atom() _ suff:(("." id:ident() { Ok(id) } / "[" _ e:exp() _ "]" { Err(e) }) ** _)
            {
                let mut res = a;
                for s in suff {
                    match s {
                        Ok(id) => res = Exp::Field(Box::new(res), id),
                        Err(e) => res = Exp::Index(Box::new(res), Box::new(IndexOp::Index(e)))
                    }
                }
                res
            }

        rule seq_op() -> IndexOp
            = ".." _ e:exp() { IndexOp::UpperBound(e) }
            / e:exp() _ ".." _ f:exp()? { match f {
                Some(f) => IndexOp::Range(e, f),
                None => IndexOp::LowerBound(e)
            } }
            / e:exp() _ ":=" _ f:exp() { IndexOp::Assign(e, f) }
            / e:exp() { IndexOp::Index(e) }

        /// Statements

        rule block() -> Block = "{" _ s:(s:annotated(<statement()>) opt_semi() { s})* "}" { Block { statements: s } }

        rule block_exp() -> Exp = "{" _ e:exp() _ "}" { e }

        rule statement() -> Statement
            = kw(<"assert">) _ e:exp() { Statement::Assert(e)}
            / kw(<"refute">) _ e:exp() { Statement::Refute(e)}
            / kw(<"assume">) _ e:exp() { Statement::Assume(e)}
            / kw(<"inhale">) _ e:exp() { Statement::Inhale(e)}
            / kw(<"exhale">) _ e:exp() { Statement::Exhale(e)}
            / kw(<"fold">) _ a:acc_exp() { Statement::Fold(a)}
            / kw(<"unfold">) _ a:acc_exp() { Statement::Unfold(a)}
            / kw(<"goto">) _ id:ident() { Statement::Goto(id)}
            / kw(<"label">) _ id:ident() _ invs:(invariant() ** _) { Statement::Label(id, invs)}
            / kw(<"havoc">) _ l:loc_access() { Statement::Havoc(l)}
            / kw(<"quasihavoc">) _ a:(e:exp() _ "==>" {e})? _ b:exp() { Statement::QuasiHavoc(a, b)}
            / kw(<"quasihavocall">) _ args:(formal_arg() ++ _) _ "::" _ a:(e:exp() _ "==>" {e})? _ b:exp() { Statement::QuasiHavocAll(args, a, b)}
            / kw(<"var">) _ args:(formal_arg() ** comma()) _ e:(":=" _ e:exp() {e})? { Statement::Var(args, e)}
            / while_statement()
            / if_statement()
            / wand_statement()
            / name:ident() _ ":=" _ "new" _ "(" _ "*" _ ")" { Statement::New(name, StarOrNames::Star) }
            / name:ident() _ ":=" _ "new" _ "(" _ args:(ident() ** comma()) _ ")" { Statement::New(name, StarOrNames::Names(args))}
            / assign_stmt()
            // Seem dead?
            // / fresh_statement()
            // / constraining_block()
            / b:block() { Statement::Block(b) }


        rule semi() = _ ";" _
        rule opt_semi() = _ ";"? _

        rule semied<R>(r: rule<R>) -> R  = r:r() opt_semi() { r }
        rule tupled<R>(r: rule<R>) -> Vec<R> = "(" _ res:(r() ** comma()) _ ")" { res }
        rule bracketed<R>(r: rule<R>) -> Vec<R> = "[" _ res:(r() ** comma()) _ "]" { res }
        rule braced<R>(r: rule<R>) -> Vec<R> = "{" _ res:(r() ** comma()) _ "}" { res }

        rule while_statement() -> Statement = "while" _ "(" _ cond:exp() _ ")" _ spec:semied(<while_spec_item()>)* _ block:block()
            {
                Statement::While(cond, spec, block)
            }

        rule while_spec_item() -> WhileSpec = i:invariant() { WhileSpec::Inv(i) } / d:decreases() { WhileSpec::Dec(d) }

        rule invariant() -> Invariant = "invariant" _ e:exp() { Invariant(e) }

        rule if_statement() -> Statement = "if" _ "(" _ cond:exp() _ ")" _ then:block() _ elsifs:(elsif_block()** _) _ else_:("else" _ else_:block() { else_})?
            { Statement::If(cond, then, elsifs, else_) }

        rule elsif_block() -> (Exp, Block) =
            "elseif" _ "(" _ exp:exp() _ ")" _ block:block() { (exp, block)}

        rule assign_stmt() -> Statement = tgts:(tgts:(assign_target() ++ comma()) _ ":=" { tgts })? _ call:exp()
            { Statement::Assign(tgts.unwrap_or_default(), call) }

        rule assign_target() -> Exp = suffix_exp()

        rule fresh_statement() -> () = "fresh" _ ident() ++ comma()

        rule wand_statement() -> Statement = "wand" _ name:ident() _ ":" _ exp:exp() { Statement::Wand(name, exp) }
            / "package" _ exp:magic_wand_exp() _ block:block()? { Statement::Package(exp, block) }
            / "apply" _ exp:magic_wand_exp() { Statement::Apply(exp) }

        rule constraining_block() -> () = "constraining" _ "(" _ ident() ++ comma() _ ")" _ block()

        rule expression_or_block() -> ExpOrBlock = exp()  { ExpOrBlock::Exp(()) } / block() { ExpOrBlock::Block(()) }

        /// Declarations

        pub rule sil_program() = _ annotated(<declaration()>) ** opt_semi() _

        rule declaration() -> Declaration
            = i:import() { Declaration::Import(i) }
            / d:define() { Declaration::Define(d) }
            / d:domain()  { Declaration::Domain(d) }
            / f:field()  { Declaration::Field(f) }
            / f:function() { Declaration::Function(f) }
            / p:predicate() { Declaration::Predicate(p) }
            / m:method() { Declaration::Method(m) }
            / a:adt() { Declaration::Adt(a) }

        rule import() -> Import = "import" _ r:("<" _ r:relative_path() _ ">" { r } / "\"" _ r:relative_path() _ "\"" { r })
            { Import { path: r } }

        rule define() -> Define = "define" _ nm:ident() _ ids:(tupled(<ident()>))? _ body:expression_or_block()
            { Define { name: nm, args: ids.unwrap_or_default(), body } }

        rule domain() -> Domain =
            "domain" _
            name:ident() _
            params:(bracketed(<ident()>))? _
            interp:domain_interpretation()? _
            "{" _ elements:(annotated(<domain_element()>) ** _) _ "}"
        { Domain { name, interpretation: interp.unwrap_or_default(), elements } }

        rule domain_interpretation() -> Vec<(Ident, String)> = "interpretation" _ rs:tupled(<interpretation_elt()>) { rs }

        rule interpretation_elt() -> (Ident, String) = i:ident() _ ":" _ s:string_lit() { (i, s)}

        rule domain_element() -> DomainElement = f:domain_function() { DomainElement::DomainFunction(f)} / a:axiom() { DomainElement::Axiom(a) }

        rule domain_function() -> DomainFunction = u:"unique"? _ sig:function_signature() _ interp:func_interpretation()?
            { DomainFunction { unique: u.is_some(), signature: sig, interpretation: interp } }

        rule function_signature() -> Signature = "function" _ id:ident() _ "(" _ args:(decl_formal_arg() ** comma()) _ ")" _ ":" _ ret:type_()
            { Signature { name: id, args: args, ret: vec!(ArgOrType::Type(ret)) } }

        rule func_interpretation() -> String = "interpretation" _ s:string_lit() { s }

        rule field() -> Field = "field" _ fields:(formal_arg() ** comma()) { Field { fields } }

        rule function() -> Function = sig:function_signature() _ cont:contract()  _ body:block_exp()?
            { Function { signature: sig, contract: cont, body } }

        rule predicate() -> Predicate = "predicate" _ id:ident() _ args:tupled(<decl_formal_arg()>) _ exp:block_exp()?
            { Predicate { signature: Signature { name: id, args, ret: vec![] }, body: exp } }

        rule formal_returns()  -> Vec<ArgOrType> = "returns" _ rets:tupled(<decl_formal_arg()>) { rets }

        rule method() -> Method = "method" _ id:ident() _ args:tupled(<decl_formal_arg()>) _ ret:formal_returns()? _ cont:contract() _ body:block()?
            { Method { signature: Signature { name: id, args, ret: ret.unwrap_or_default() }, contract: cont, body } }

        rule adt() -> Adt = "adt" _ ty:type_constr() _ vars:adt_variants() _ derives:derives()?
            {
                let Type::User(name, args) = ty else { unreachable!() };
                Adt { name, args, variants: vars , derives: derives.map(|s| vec![s]).unwrap_or_default()  }
            }

        rule adt_variant() -> Variant = name:ident() _ fields:tupled(<formal_arg()>) _
                { Variant { name, fields } }

        rule adt_variants() -> Vec<Variant> = "{" _ vars:adt_variant()* _ "}" { vars }

        rule derives() -> String = "derives" _ "{" _ str:$((!"}" [_])*) _ "}" { str.to_string() }

        rule relative_path() -> String = s:$(['~' | '.']? (['/']? ['.' | 'A'..='Z' | 'a'..='z' | '0'..='9' | '_' | '\\' | '-' | ' '])+) { s.to_string() }

        rule string_lit() -> String = str:$("\"" _ (!"\"" [_])* _ "\"") { str.to_string() }

        rule axiom() -> Axiom = "axiom" _ name :ident()? _ exp:block_exp()
            { Axiom {name, exp } }

        rule precondition() -> PrePostDec = "requires" _ e:exp() { PrePostDec::Pre(e) }

        rule postcondition() -> PrePostDec = "ensures" _ e:exp() { PrePostDec::Post(e) }

        rule decreases() -> Decreases = "decreases" _ d:decreases_kind()? _ e:("if" _ e:exp() { e })? { Decreases { kind: d, guard: e } }

        rule decreases_kind() -> DecreasesKind = "*" { DecreasesKind::Star } / "_" { DecreasesKind::Underscore } / e:(exp() ** comma()) { DecreasesKind::Exp(e) }

        rule contract() -> Contract =
            pres:(p:(precondition()  / d:decreases() { PrePostDec::Decreases(d) }) opt_semi() {p})* _ post:(p:(postcondition() / d:decreases() { PrePostDec::Decreases(d) } ) opt_semi() {p})*
            {
                let mut contract = Contract { preconditions: vec![], postconditions: vec![], decreases: vec![]};
                for p in pres {
                    match p {
                        PrePostDec::Pre(e) => contract.preconditions.push(e),
                        PrePostDec::Decreases(d) => contract.decreases.push(d),
                        _ => {}
                    }
                }

                for p in post {
                    match p {
                        PrePostDec::Post(e) => contract.postconditions.push(e),
                        PrePostDec::Decreases(d) => contract.decreases.push(d),
                        _ => {}
                    }
                }

                contract

             }


    }
}
enum PrePostDec {
    Pre(Exp),
    Post(Exp),
    Decreases(Decreases),
}

pub struct Decreases {
    kind: Option<DecreasesKind>,
    guard: Option<Exp>,
}

enum DecreasesKind {
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
    name: Option<Ident>,
    exp: Exp,
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

enum ArgOrType {
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
    exp: Vec<Exp>,
}

pub enum ResAccess {
    Loc(LocAccess),
    Exp(Exp),
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

enum IndexOp {
    Index(Exp),
    LowerBound(Exp),
    UpperBound(Exp),
    Range(Exp, Exp),
    Assign(Exp, Exp),
}

pub struct Invariant(Exp);

pub enum WhileSpec {
    Inv(Invariant),
    Dec(Decreases),
}

pub struct LocAccess {
    loc: Exp,
}

pub struct Field {
    fields: Vec<(Ident, Type)>,
}

pub struct Domain {
    name: Ident,
    interpretation: Vec<(Ident, String)>,
    elements: Vec<DomainElement>,
}

pub struct Function {
    signature: Signature,
    contract: Contract,
    body: Option<Exp>,
}

pub struct Contract {
    preconditions: Vec<Exp>,
    postconditions: Vec<Exp>,
    decreases: Vec<Decreases>,
}

pub enum DomainElement {
    DomainFunction(DomainFunction),
    Axiom(Axiom),
}

pub struct DomainFunction {
    unique: bool,
    signature: Signature,
    interpretation: Option<String>,
}

pub struct Signature {
    name: Ident,
    args: Vec<ArgOrType>,
    ret: Vec<ArgOrType>,
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
    body: Option<Exp>,
}

pub struct Method {
    signature: Signature,
    contract: Contract,
    body: Option<Block>,
}

pub struct Adt {
    name: Ident,
    args: Vec<Type>,
    variants: Vec<Variant>,
    derives: Vec<String>,
}

pub struct Variant {
    name: Ident,
    fields: Vec<(Ident, Type)>,
}
