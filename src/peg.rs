peg::parser! {
    pub grammar silver_parser() for str {
        rule _ = quiet! { ___ __ ** ___ ___ }

        rule white_space() = quiet! { " " / "\t" / "\n" / "\r\n" } / expected!("whitespace")
        rule ___ = white_space()*

        rule __ = "//" (! "\n" [_])* / "/*" (! "\n" [_]) "*/"

        rule start_char() -> &'input str
            = $(['A'..='Z' | 'a'..='z'| '$' | '_' ])

        rule char() -> &'input str
            = $(['A'..='Z' | 'a'..='z'| '$' | '_' | '\'' | '0'..='9' ])

        rule reserved()
            = "havoc" / "true" / "false"
            / "Set" / "Seq" / "Map" / "Multiset" / "Int" / "Bool" / "Perm" / "Ref" / "Rational"
            / "forperm" / "let" / "in" / "if" / "then" / "else" / "elsif" / "while" / "do"
            / "assert" / "assume" / "havoc"
            / "return" / "break" / "continue" / "skip"
            / "forall" / "exists"
            / "inhale" / "exhale" / "unfold" / "fold" / "acc"
            / "in"
            / "none"

        rule ident()
            = quiet! { !(reserved() white_space()) n:$(start_char() char()*) }
            / expected!("identifier")

        rule integer() = ['0'..='9']+

        rule comma() = _ "," _

        rule type_() -> ()
            = "Int"
            / "Bool"
            / "Perm"
            / "Ref"
            / "Rational"
            / ("Seq" _ "[" _ type_() _ "]")
            / ("Set" _ "[" _ type_() _ "]")
            / ("Map" _ "[" _ type_() _ "," _ type_() _ "]")
            / type_constr()

        rule type_constr() -> () = ident() _ ("[" _ type_() ** comma() _ "]")?

        rule formal_arg() -> () = ident() _ ":" _ type_() { }

        /// Accessors

        rule predicate_access() -> () =
            ident() _ "(" _ exp() ** comma() _ ")"

        rule field_access() = ident() ("." ident())+

        rule loc_access() = field_access() / predicate_access()

        rule res_access() = magic_wand_exp() / loc_access()

        // acc_exp = { "acc"~"("~loc_access~(","~exp)?~")" | predicate_access }
        rule acc_exp() = "acc" _ "(" _ loc_access() _ ("," _ exp())? _ ")" / predicate_access()

        rule trigger() = "{" _ exp() ** comma() _ "}"


        /// Expressions

        rule seq_op_exp()
            = exp() _ "[" _ exp() _ "]"
            / exp() _ "[" _ ".." _ exp() _ "]"
            / exp() _ "[" _ exp() _ ".." _ "]"
            / exp() _ "[" _ exp() _ ".." _ exp() _ "]"
            / exp() _ "[" _ exp() _ ":=" _ exp() _ "]"

        rule set_constructor_exp()
            = "Set" _ "[" _ type_() _ "]" _ "(" _ ")"
            / "Set" _ "(" _ exp() ** comma() _ ")"
            / "Multiset" _ "[" _ type_() _ "]" _ "(" _ ")"
            / "Multiset" _ "(" _ exp() ** comma() _ ")"

        rule seq_constructor_exp()
            = "Seq" _ "[" _ type_() _ "]" _ "(" _ ")"
            / "Seq" _ "(" _ exp() ** comma() _ ")"
            / "[" _ exp() _ ".." _ exp() _ ")"

        rule map_constructor_exp()
            = "Map" _ "[" _ type_() _ "," _ type_() _ "]" _ "(" _ ")"
            / "Map" _ "(" _ (exp() _ ":=" _ exp()) ** comma() _ ")"

        rule forperm_exp() = "forperm" _ formal_arg() ++ comma() _ "[" _ res_access() _ "]" _ "::" _ exp()

        rule let_in_exp() = "let" _ ident() _ "==" _ "(" _ exp() _ ")" _ "in" _ exp()

        rule magic_wand_exp() = exp()

        rule func_app() = ident() _ "(" _ exp() ** comma() _ ")"

        rule atom()
            = "true" / "false"
            / integer()
            / "null"
            / "result"
            / "(" _ exp() _ ")"
            / "old" _ ("[" _ ident() _ "]")? _ "(" _ exp() _ ")"
            / "[" _ ident() _ "]" _ "(" _ exp() _ ")"
            / "lhs" _ "(" _ exp() _ ")"
            / "none" / "write" / "epsilon" / "wildcard"
            / "perm" _ "(" _ loc_access() _ ")"
            / "[" _ exp() _ "," _ exp() _ "]"
            / "unfolding" _ acc_exp() _ "in" _ exp()
            / "folding" _ acc_exp() _ "in" _ exp()
            / "applying" _ "(" _ magic_wand_exp() _ ")" _ "in" _ exp()
            / "packaging" _ "(" _ magic_wand_exp() _ ")" _ "in" _ exp()
            / "forall" _ formal_arg() ++ comma() _ "::" _ trigger()* _ exp()
            / "exists" _ formal_arg() ++ comma() _ "::" _ trigger()* _ exp()
            / seq_constructor_exp()
            / set_constructor_exp()
            / map_constructor_exp()
            / "|" _ exp() _ "|"
            / let_in_exp()
            / forperm_exp()
            / predicate_access()
            / acc_exp()
            / func_app()
            / ident()

        rule exp() -> () = precedence! {
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
            x:@ (_ "in" &white_space() _) y:(@) {}
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
            x:@ ("." ident()) {}
            --
            atom() {}
        }

        rule paren_list<R>(r : rule<R>) -> () = "(" _ r() ** comma() _ ")" { () }

        /// Statements

        rule block() = "{" _ statement() ** (opt_semi()) _ "}"

        rule statement()
            = "assert" _ exp()
            / "refute" _ exp()
            / "assume" _ exp()
            / "inhale" _ exp()
            / "exhale" _ exp()
            / "fold" _ acc_exp()
            / "unfold" _ acc_exp()
            / "goto" _ ident()
            / "label" _ ident() _ invariant() ** _
            / "havoc" _ loc_access()
            / "quasihavoc" _ (exp() _ "==>")? _ exp()
            / "quasihavocall" _ formal_arg() ++ _ _ "::" _ (exp() _ "==>")? _ exp()
            / "var" _ formal_arg() ** comma() _ (":=" _ exp())?
            / while_statement()
            / if_statement()
            / wand_statement()
            / ident() _ ":=" _ "new" _ "(" _ "*" _ ")"
            / ident() _ ":=" _ "new" _ "(" _ ident() ++ comma() _ ")"
            / call_statement()
            / fresh_statement()
            / constraining_block()
            / block()

        rule semi() = _ ";" _
        rule opt_semi() = _ ";"? _

        rule while_statement() = "while" _ "(" _ exp() _ ")" _ invariant() ** opt_semi() _ block()

        rule invariant() = "invariant" _ exp()

        rule if_statement() = "if" _ "(" _ exp() _ ")" _ block() _ ("elsif" _ "(" _ exp() _ ")" _ block() _)** _ _ ("else" _ block())?

        // call_statement = { assign_target~((","~assign_target)*~":="~exp)? }
        rule call_statement() = assign_target() ++ comma() _ ":=" _ exp()

        rule assign_target() = field_access() / func_app() / ident()

        rule fresh_statement() = "fresh" _ ident() ++ comma()

        rule wand_statement() = "wand" _ ident() _ ":" _ exp()
            / "package" _ magic_wand_exp() _ block()?
            / "apply" _ magic_wand_exp()

        rule constraining_block() = "constraining" _ "(" _ ident() ++ comma() _ ")" _ block()

        /// Declarations

        pub rule sil_program() = _ declaration() ** _ _

        rule declaration() = import() / define() / domain() / field() / function() / predicate() / method() / adt()

        rule import() = "import" _ ("<" _ relative_path() _ ">" / "\"" _ relative_path() _ "\"")

        rule define() = "define" _ ident() _ "(" _ ident() ** comma() _ ")" _ expression_or_block()

        rule expression_or_block() = exp() / block()

        rule domain() = "domain" _ domain_name() _ domain_interpretation()? _ "{" _ domain_element() ** opt_semi() _ "}"

        rule domain_name() = ident() _ ("[" _ ident() ** comma() _ "]")?

        rule domain_interpretation() = "interpretation" _ "(" _ (ident() _ ":" _ string_lit())++comma() _ ")"

        rule domain_element() = domain_function() / axiom()

        // TODO(xavier): factor out semi colon stuff
        rule domain_function() = "unique"? _ function_signature() _ func_interpretation()?

        rule function_signature() = "function" _ ident() _ "(" _ formal_arg() ** comma() _ ")" _ ":" _ type_()

        rule func_interpretation() = "interpretation" _ string_lit()

        rule field() = "field" _ ident() _ ":" _ type_()

        rule function() = function_signature() _ (precondition() / decreases())** _ _ (postcondition() / decreases()) ** _  _ ("{" _ exp() _ "}")?

        rule predicate() = "predicate" _ ident() _ paren_list(<formal_arg()>) _ "{" _ exp() _ "}"

        rule method() = "method" _ ident() _ paren_list(<formal_arg()>) _ (precondition() / decreases())** _ _ (postcondition() / decreases()) ** _ _ block()

        rule adt() = "adt" _ type_constr() _ adt_variants() _ derives()?

        rule adt_variants() = "{" _ (ident() _ formal_arg() ** comma() _)* _ "}"

        rule derives() = "derives" _ "{" _ (!"}" [_])* _ "}"

        rule relative_path() = ['~' | '.']? ['/']? ['.' | 'A'..='Z' | 'a'..='z' | '0'..='9' | '_' | '\\' | '-' | ' ']*

        rule string_lit() = "\"" _ (!"\"" [_])* _ "\""

        rule axiom() = "axiom" _ ident()? _ "{" _ exp() _ "}"

        rule precondition() = "requires" _ exp()

        rule postcondition() = "ensures" _ exp()

        rule decreases() = "decreases" _ exp()



    }
}

pub struct Ident(pub String);
