use crate::parse::ast::*;

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

        rule idndecl() -> IdnDecl = n:ident() { IdnDecl(n) }

        rule label() -> Ident
            = quiet! {  n:$(start_char() char()*) { Ident(n.to_string())} }
            / expected!("label")

        rule kw<R>(r: rule<R>) -> () = r() !char()

        rule integer() -> num::BigInt = s:$("-"? ['0'..='9']+) {? s.parse().or(Err("invalid integer")) }

        rule comma() = _ "," _

        rule annotation() = quiet! { "@" annotation_ident() annotation_args() }  / expected!("annotation")

        rule annotation_ident() = ident() ++ "."

        rule annotation_args() = "(" _ string_lit() ** comma() _ ")"

        // TODO: insert annotations
        rule annotated<R>(r: rule<R>) -> R = annotation() ** _ _ r:r() { r }

        /// Types

        rule type_() -> Type
            =
              "Int" { Type::Int }
            / "Bool" { Type::Bool }
            / "Perm" { Type::Real }
            / "Ref" { Type::Ref }
            / "Rational"   { Type::Real }
            / "Seq" _ "[" _ ty:type_() _ "]" { Type::Domain(Ident::seq(), vec![ty]) }
            / "Set" _ "[" _ ty:type_() _ "]" { Type::Domain(Ident::set(), vec![ty]) }
            / "Multiset" _ "[" _ ty:type_() _ "]" { Type::Domain(Ident::multiset(), vec![ty]) }
            / "Map" _ "[" _ a:type_() _ "," _ b:type_() _ "]" { Type::Domain(Ident::map(), vec![a, b]) }
            / type_constr()

        rule type_constr() -> Type = nm:ident() _ tys:("[" _ tys:(type_() ** comma()) _ "]" { tys })?
            {   let tys = tys.unwrap_or_default();
                Type::Domain(nm, tys)
            }

        rule formal_arg() -> IdnDeclTyped = idn:idndecl() _ ":" _ ty:type_() { IdnDeclTyped { idn, ty } }

        rule decl_named_formal_arg() -> ArgOrType = a:formal_arg()  { ArgOrType::Arg(a) }

        rule decl_formal_arg() -> ArgOrType = decl_named_formal_arg() / t:type_() { ArgOrType::Type(t) }
        /// Accessors

        rule predicate_access() -> LocAccess = loc: func_app() { LocAccess { loc: Box::new(loc) } }

        rule predicate_perm() -> AccExp = acc_exp() / acc:predicate_access() { AccExp { acc, perm: ExpKind::write() } }

        // TODO: only accept expressions that end with a .field
        rule field_access() -> ExpKind = suffix_exp()

        rule loc_access() -> LocAccess = f:field_access() { LocAccess { loc: Box::new(f) } } / predicate_access()

        rule res_access() -> ResAccess = e:magic_wand_exp() { ResAccess::Exp(e) } / l:loc_access() { ResAccess::Loc(l) }

        rule acc_exp() -> AccExp
            = "acc" _ "(" _ acc:loc_access() _ perm:("," _ e:exp() { e })? _ ")" { AccExp { acc, perm: perm.unwrap_or_else(ExpKind::write) } }

        rule trigger() -> Trigger = "{" _ es:(exp() ** comma()) _ "}" { Trigger { exp: es } }


        /// Expressions

        rule set_constructor_exp() -> ExpKind
            = "Set" _ "[" _ ty:type_() _ "]" _ "(" _ ")" { ExpKind::Ascribe(Box::new(ExpKind::FuncApp(Ident::set(), Vec::new())), Type::Domain(Ident::set(), vec![ty])) }
            / "Set" _ "(" _ es:(exp() ** comma()) _ ")" { ExpKind::FuncApp(Ident::set(), es) }
            / "Multiset" _ "[" _ ty:type_() _ "]" _ "(" _ ")"{ ExpKind::Ascribe(Box::new(ExpKind::FuncApp(Ident::multiset(), Vec::new())), Type::Domain(Ident::multiset(), vec![ty])) }
            / "Multiset" _ "(" _ es:(exp() ** comma()) _ ")" { ExpKind::FuncApp(Ident::multiset(), es) }

        rule seq_constructor_exp() -> ExpKind
            = "Seq" _ "[" _ ty:type_() _ "]" _ "(" _ ")" { ExpKind::Ascribe(Box::new(ExpKind::FuncApp(Ident::seq(), Vec::new())), Type::Domain(Ident::seq(), vec![ty])) }
            / "Seq" _ "(" _ es:(exp() ** comma()) _ ")" { ExpKind::FuncApp(Ident::seq(), es) }
            / "[" _ s:exp() _ ".." _ e:exp() _ ")" { ExpKind::BinOp(BinOp::Range, s, e) }

        rule map_constructor_exp() -> ExpKind
            = "Map" _ "[" _ a:type_() _ "," _ b:type_() _ "]" _ "(" _ ")" { ExpKind::Ascribe(Box::new(ExpKind::FuncApp(Ident::map(), Vec::new())), Type::Domain(Ident::map(), vec![a, b])) }
            / "Map" _ "(" _ es:((l:exp() _ ":=" _ r:exp() { (l, r)}) ** comma()) _ ")" {
                es.into_iter().fold(ExpKind::FuncApp(Ident::map(), Vec::new()), |acc, (l, r)| {
                    ExpKind::Index(Box::new(acc), IndexOp::Assign(l, r))
                })
            }

        rule forperm_exp() -> ExpKind = "forperm" _ args:(formal_arg() ++ comma()) _ "[" _ res:res_access() _ "]" _ "::" _ exp:exp()
            { ExpKind::ForPerm(args, res, exp) }

        rule let_in_exp() -> ExpKind = "let" _ id:idndecl() _ "==" _ "(" _ e:exp() _ ")" _ "in" _ body:exp()
            { ExpKind::LetIn(id, e, body) }

        rule magic_wand_exp() -> AccExp = loc:exp() { match *loc {
            ExpKind::Acc(acc) => acc,
            _ => AccExp { acc: LocAccess { loc }, perm: ExpKind::write() }
        } }

        rule func_app() -> ExpKind = id:ident() (" ")* "(" _ es:(exp() ** comma()) _ ")" { ExpKind::FuncApp(id, es) }

        rule atom() -> ExpKind
            = kw(<"true">) { ExpKind::Const(ConstKind::Bool(true)) } / kw(<"false">) { ExpKind::Const(ConstKind::Bool(false)) }
            / i:integer() { ExpKind::Const(ConstKind::Int(i)) }
            / kw(<"null">) { ExpKind::Const(ConstKind::Null) }
            / kw(<"result">) { ExpKind::Result }
            / "(" _ e:exp_kind() _ ty:(":" _ ty:type_() { ty })? _ ")" { match ty {
                Some(ty) => ExpKind::Ascribe(Box::new(e), ty),
                None => e
                }
            }
            / kw(<"old">) _ i:("[" _ i:ident() _ "]" {i})? _ "(" _ e:exp() _ ")" { ExpKind::Old(i, e) }
            // / "[" _ i:ident() _ "]" _ "(" _ e:exp() _ ")" { ExpKind::At(i, Box::new(e)) }
            // / kw(<"lhs">) _ "(" _ e:exp() _ ")" { ExpKind::Lhs(Box::new(e)) }
            / kw(<"none">) { ExpKind::Const(ConstKind::Real(num::BigInt::from(0).into())) }
            / kw(<"write">) { ExpKind::Const(ConstKind::Real(num::BigInt::from(1).into())) }
            / kw(<"epsilon">) { ExpKind::Const(ConstKind::Epsilon) }
            / kw(<"wildcard">) { ExpKind::Const(ConstKind::Wildcard) }
            / kw(<"perm">) _ "(" _ l:exp() _ ")" { ExpKind::UnOp(UnOp::Perm, l) }
            / "[" _ e:exp() _ "," _ f:exp() _ "]" { ExpKind::BinOp(BinOp::InhaleExhale, e, f)}

            / kw(<"unfolding">) _ acc:predicate_perm() _ "in" _ e:exp() { ExpKind::HeapUpdate(HeapUpdateOp::Unfold, acc, e) }
            / kw(<"folding">) _ acc:predicate_perm() _ "in" _ e:exp() { ExpKind::HeapUpdate(HeapUpdateOp::Fold, acc, e) }

            / kw(<"applying">) _ "(" _ mwexp:magic_wand_exp() _ ")" _ "in" _ e:exp() { ExpKind::HeapUpdate(HeapUpdateOp::Apply, mwexp, e) }
            / kw(<"packaging">) _ "(" _ mwexp:magic_wand_exp() _ ")" _ "in" _ e:exp() { ExpKind::HeapUpdate(HeapUpdateOp::Package, mwexp, e) }
            / kw(<"forall">) _ args:(formal_arg() ++ comma()) _ "::" _ triggers:(trigger()**_) _ e:exp() { ExpKind::Quantifier(QuantifierKind::Forall, args, triggers, e) }
            / kw(<"exists">) _ args:(formal_arg() ++ comma()) _ "::" _ triggers:(trigger()**_) _ e:exp() { ExpKind::Quantifier(QuantifierKind::Exists, args, triggers, e) }

            / s:seq_constructor_exp()
            / s:set_constructor_exp()
            / m:map_constructor_exp()
            / "|" _ e:exp() _ "|" { ExpKind::UnOp(UnOp::Abs, e) }
            / let_in_exp()
            / forperm_exp()
            / a:acc_exp() { ExpKind::Acc(a) }
            / func_app()
            / i:ident() { ExpKind::Ident(i) }



        rule full_exp() -> ExpKind = precedence! {
            x:@ z:(_ "?" _ z:exp() _ ":" _ {z}) y:(@) { ExpKind::Ternary(Box::new(x), z, Box::new(y)) }
            --
            x:@ (_ "<==>" _) y:(@) { ExpKind::BinOp(BinOp::Iff, Box::new(x), Box::new(y)) }
            --
            x:@ (_ "==>" _) y:(@) { ExpKind::BinOp(BinOp::Implies, Box::new(x), Box::new(y)) }
            --
            x:@ (_ "--*" _) y:(@) { ExpKind::BinOp(BinOp::MagicWand, Box::new(x), Box::new(y)) }
            --
            x:@ (_ "||" _) y:(@) { ExpKind::BinOp(BinOp::Or, Box::new(x), Box::new(y)) }
            --
            x:@ (_ "&&" _) y:(@) { ExpKind::BinOp(BinOp::And, Box::new(x), Box::new(y)) }
            --
            x:@ (_ "!=" _) y:(@) { ExpKind::BinOp(BinOp::Neq, Box::new(x), Box::new(y)) }
            x:@ (_ "==" _) y:(@) { ExpKind::BinOp(BinOp::Eq, Box::new(x), Box::new(y)) }
            --
            x:@ (_ "<=" _) y:(@) { ExpKind::BinOp(BinOp::Le, Box::new(x), Box::new(y)) }
            x:@ (_ ">=" _) y:(@) { ExpKind::BinOp(BinOp::Ge, Box::new(x), Box::new(y)) }
            x:@ (_ ">" _) y:(@) {  ExpKind::BinOp(BinOp::Gt, Box::new(x), Box::new(y)) }
            x:@ (_ "<" _) y:(@) { ExpKind::BinOp(BinOp::Lt, Box::new(x), Box::new(y)) }
            x:@ (_ "in" &(white_space() / "(") _) y:(@) { ExpKind::BinOp(BinOp::In, Box::new(x), Box::new(y))}
            --
            x:(@) (_ "-" _) y:@ { ExpKind::BinOp(BinOp::Minus, Box::new(x), Box::new(y)) }
            x:(@) (_ "+" _) y:@ { ExpKind::BinOp(BinOp::Plus, Box::new(x), Box::new(y)) }
            x:(@) (_ "++" _) y:@ { ExpKind::BinOp(BinOp::Concat, Box::new(x), Box::new(y)) }
            x:(@) (_ "union" white_space() _) y:@ { ExpKind::BinOp(BinOp::Union, Box::new(x), Box::new(y)) }
            x:(@) (_ "setminus" white_space() _) y:@ { ExpKind::BinOp(BinOp::SetMinus, Box::new(x), Box::new(y))}
            x:(@) (_ "intersection" white_space() _) y:@ { ExpKind::BinOp(BinOp::Intersection, Box::new(x), Box::new(y))}
            x:(@) (_ "subset" white_space() _) y:@ { ExpKind::BinOp(BinOp::Subset, Box::new(x), Box::new(y))}
            --
            x:(@) (_ "*" _) y:@ { ExpKind::BinOp(BinOp::Mult, Box::new(x), Box::new(y)) }
            x:(@) (_ "/" _) y:@ { ExpKind::BinOp(BinOp::Div, Box::new(x), Box::new(y)) }
            x:(@) (_ "%" _) y:@ { ExpKind::BinOp(BinOp::Mod, Box::new(x), Box::new(y)) }
            x:(@) (_ "\\" _) y:@ { ExpKind::BinOp(BinOp::IntDiv, Box::new(x), Box::new(y)) }
            --
            x:@ i:(_ "." i:ident() {i}) { ExpKind::Field(Box::new(x), i) }
            x:@ _ "[" _ s:seq_op() _ "]" _  { ExpKind::Index(Box::new(x), s) }
            "-" _ x:@ { ExpKind::UnOp(UnOp::Neg, Box::new(x)) }
            "!" _ x:@ { ExpKind::UnOp(UnOp::Not, Box::new(x)) }
            --
            a:annotated(<atom()>) {a}
        }

        rule exp_kind() -> ExpKind = annotated(<full_exp()>)

        rule exp() -> Exp = e:exp_kind() { Box::new(e) }

        rule suffix_exp() -> ExpKind = a:atom() _ suff:(("." id:ident() { Ok(id) } / "[" _ e:exp() _ "]" { Err(e) }) ** _)
            {
                let mut res = a;
                for s in suff {
                    match s {
                        Ok(id) => res = ExpKind::Field(Box::new(res), id),
                        Err(e) => res = ExpKind::Index(Box::new(res), IndexOp::Index(e))
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

        rule block() -> StmtBlock = "{" _ s:(s:annotated(<statement()>) opt_semi() { s})* "}" { Block(s) }

        rule block_exp() -> ExpBlock = "{" _ e:exp() _ "}" { Block(e) }

        rule statement() -> Statement
            = kw(<"assert">) _ e:exp() { Statement::Assert(e)}
            / kw(<"refute">) _ e:exp() { Statement::Refute(e)}
            / kw(<"assume">) _ e:exp() { Statement::Assume(e)}
            / kw(<"inhale">) _ e:exp() { Statement::Inhale(HeapExp::new(e))}
            / kw(<"exhale">) _ e:exp() { Statement::Exhale(HeapExp::new(e))}
            / kw(<"fold">) _ e:predicate_perm() { Statement::Fold(e)}
            / kw(<"unfold">) _ e:predicate_perm() { Statement::Unfold(e)}
            / kw(<"goto">) _ id:label() { Statement::Goto(id)}
            / kw(<"label">) _ id:label() _ invs:(invariant() ** _) { Statement::Label(IdnDecl(id), Invariant(HeapExp::conjoin(invs)))}
            / kw(<"havoc">) _ l:loc_access() { Statement::Havoc(l)}
            / kw(<"quasihavoc">) _ a:(e:exp() _ "==>" {e})? _ b:exp() { Statement::QuasiHavoc(a, b)}
            / kw(<"quasihavocall">) _ args:(formal_arg() ++ _) _ "::" _ a:(e:exp() _ "==>" {e})? _ b:exp() { Statement::QuasiHavocAll(args, a, b)}
            / kw(<"var">) _ args:(formal_arg() ** comma()) _ e:(":=" _ e:assign_rhs() {e})? { Statement::Var(args, e)}
            / while_statement()
            / if_statement()
            / wand_statement()
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
                let c = Contract::from(spec);
                Statement::While(cond, Invariant(c.precondition), c.decreases, block)
            }

        rule while_spec_item() -> PrePostDec = i:invariant() { PrePostDec::Pre(i) } / d:decreases() { d }

        rule invariant() -> Exp = "invariant" _ e:exp() { e }

        rule if_statement() -> Statement = "if" _ "(" _ cond:exp() _ ")" _ then:block() _ elsifs:(elsif_block()** _) _ else_:("else" _ else_:block() { else_})? {
            let mut elsifs = [(cond, then)].into_iter().chain(elsifs).rev();
            let (cond, then) = elsifs.next().unwrap();
            elsifs.fold(Statement::If(cond, then, else_), |acc, (cond, then)| Statement::If(cond, then, Some(Block(vec![acc]))))
        }

        rule elsif_block() -> (Exp, StmtBlock) =
            "elseif" _ "(" _ exp:exp() _ ")" _ block:block() { (exp, block)}

        rule assign_stmt() -> Statement = tgts:(tgts:(assign_target() ++ comma()) _ ":=" { tgts })? _ rhs:assign_rhs()
            { Statement::Assign(tgts.unwrap_or_default(), rhs) }

        rule assign_target() -> Exp = e:suffix_exp() { Box::new(e) }

        rule assign_rhs() -> AssignRhs =
              "new" _ "(" _ "*" _ ")" { AssignRhs::New(StarOrNames::Star) }
            / "new" _ "(" _ args:(ident() ** comma()) _ ")" { AssignRhs::New(StarOrNames::Names(args))}
            / e:exp() { match *e {
                ExpKind::FuncApp(id, args) => AssignRhs::Call(id, args),
                _ => AssignRhs::Exp(e)
            }}

        rule wand_statement() -> Statement =// "wand" _ name:ident() _ ":" _ exp:exp() { Statement::Wand(name, exp) } /
            "package" _ exp:magic_wand_exp() _ block:block()? { Statement::Package(exp, block) } /
            "apply" _ exp:magic_wand_exp() { Statement::Apply(exp) }

        rule constraining_block() -> () = "constraining" _ "(" _ ident() ++ comma() _ ")" _ block()

        rule expression_or_block() -> ExpOrBlock = exp:exp()  { ExpOrBlock::Exp(exp) } / block:block() { ExpOrBlock::Block(block) }

        /// Declarations

        pub rule sil_program() -> Program = _ decls:annotated(<d:single_decl() { vec![d] } / multi_decl()>) ** opt_semi() _
            { Program(decls.into_iter().flatten().collect()) }

        rule single_decl() -> Declaration
            = i:import() { Declaration::Import(i) }
            / d:define() { Declaration::Define(d) }
            / f:function() { Declaration::Function(f) }
            / p:predicate() { Declaration::Predicate(p) }
            / m:method() { Declaration::Method(m) }
            / a:adt() { Declaration::Adt(a) }

        rule import() -> Import = "import" _ r:("<" _ r:relative_path() _ ">" { (r, false) } / "\"" _ r:relative_path() _ "\"" { (r, true) })
            { Import { path: r.0, local: r.1 } }

        rule define() -> Define = "define" _ nm:idndecl() _ ids:(tupled(<idndecl()>))? _ body:expression_or_block()
            { Define { name: nm, args: ids.unwrap_or_default(), body } }

        rule multi_decl() -> Vec<Declaration> =
            domain() / field()

        rule domain() -> Vec<Declaration> =
            "domain" _
            name:idndecl() _
            params:(bracketed(<idndecl()>))? _
            interp:domain_interpretation()? _
            "{" _ elements:(annotated(<domain_element()>) ** _) _ "}"
        {
            [Declaration::Domain(Domain { name: name.clone(), interpretation: interp.unwrap_or_default() })].into_iter().chain(
                elements.into_iter().map(|kind| Declaration::DomainElement(DomainElement { domain: name.0.clone(), kind }))
            ).collect()
        }

        rule domain_interpretation() -> Vec<(Ident, String)> = "interpretation" _ rs:tupled(<interpretation_elt()>) { rs }

        rule interpretation_elt() -> (Ident, String) = i:ident() _ ":" _ s:string_lit() { (i, s)}

        rule domain_element() -> DomainElementKind = f:domain_function() { DomainElementKind::Function(f)} / a:axiom() { DomainElementKind::Axiom(a) }

        rule domain_function() -> DomainFunction = u:"unique"? _ sig:domain_function_signature() _ interp:func_interpretation()?
            { DomainFunction { unique: u.is_some(), signature: sig, interpretation: interp } }

        rule domain_function_signature() -> Signature = "function" _ id:idndecl() _ "(" _ args:(decl_formal_arg() ** comma()) _ ")" _ ":" _ ret:type_()
            { Signature { name: id, args, ret: vec!(ArgOrType::Type(ret)) } }

        rule func_interpretation() -> String = "interpretation" _ s:string_lit() { s }

        rule field() -> Vec<Declaration> = "field" _ fields:((f:formal_arg() {
            Declaration::Field(Field(Signature::field(f)))
        }) ** comma()) { fields }

        rule function() -> Function = sig:function_signature() _ cont:contract()  _ body:block_exp()?
            { Function { signature: sig, contract: cont, body } }

        rule function_signature() -> Signature = "function" _ id:idndecl() _ "(" _ args:(decl_named_formal_arg() ** comma()) _ ")" _ ":" _ ret:type_()
            { Signature { name: id, args, ret: vec!(ArgOrType::Type(ret)) } }

        rule predicate() -> Predicate = "predicate" _ id:idndecl() _ args:tupled(<decl_named_formal_arg()>) _ exp:block_exp()?
            { Predicate { signature: Signature { name: id, args, ret: Vec::new() }, body: exp.map(|e| e.map(HeapExp::new)) } }

        rule formal_returns()  -> Vec<ArgOrType> = "returns" _ rets:tupled(<decl_named_formal_arg()>) { rets }

        rule method() -> Method = "method" _ id:idndecl() _ args:tupled(<decl_named_formal_arg()>) _ ret:formal_returns()? _ cont:contract() _ body:block()?
            { Method { signature: Signature { name: id, args, ret: ret.unwrap_or_default() }, contract: cont, body } }

        rule adt() -> Adt = "adt" _ ty:type_constr() _ vars:adt_variants() _ derives:derives()?
            {
                let Type::Domain(name, args) = ty else { unreachable!() };
                Adt { name: IdnDecl(name), args, variants: vars , derives: derives.map(|s| vec![s]).unwrap_or_default()  }
            }

        rule adt_variant() -> Variant = name:idndecl() _ fields:tupled(<formal_arg()>) _
                { Variant { name, fields } }

        rule adt_variants() -> Vec<Variant> = "{" _ vars:adt_variant()* _ "}" { vars }

        rule derives() -> String = "derives" _ "{" _ str:$((!"}" [_])*) _ "}" { str.to_string() }

        rule relative_path() -> String = s:$(['~' | '.']? (['/']? ['.' | 'A'..='Z' | 'a'..='z' | '0'..='9' | '_' | '\\' | '-' | ' '])+) { s.to_string() }

        rule string_lit() -> String = str:$("\"" _ (!"\"" [_])* _ "\"") { str.to_string() }

        rule axiom() -> Axiom = "axiom" _ name :idndecl()? _ exp:block_exp()
            { Axiom {name, exp } }

        rule precondition() -> PrePostDec = "requires" _ e:exp() { PrePostDec::Pre(e) }

        rule postcondition() -> PrePostDec = "ensures" _ e:exp() { PrePostDec::Post(e) }

        rule decreases() -> PrePostDec = "decreases" _ d:decreases_kind()? _ e:("if" _ e:exp() { e })? { PrePostDec::Decreases(Decreases { kind: d, guard: e }) }

        rule decreases_kind() -> DecreasesKind = "*" { DecreasesKind::Star } / "_" { DecreasesKind::Underscore } / e:(exp() ** comma()) { DecreasesKind::Exp(e) }

        rule contract() -> Contract =
            pres:(p:(precondition()  / d:decreases() { d }) opt_semi() {p})* _ posts:(p:(postcondition() / d:decreases() { d } ) opt_semi() {p})*
            { Contract::from(pres).add_posts(posts) }


    }
}
