use pest::{pratt_parser::PrattParser, Parser};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "silver.pest"]
pub struct Silver;

// Expression operator priority
// ============================
// (operators are in general, right-associative)

// ternary conditional operator
// <==>
// ==>
// --*
// ||
// &&
// == !=
// <= >= < > in
// ++ + - union intersection setminus subset (left-associative!)
// * / \ % (left-associative!)
// field-access seq-op-exp
// other expressions

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence is defined lowest to highest
        PrattParser::new()
            .op(Op::infix(iff, Right))
            .op(Op::infix(implies, Right))
            .op(Op::infix(magic_wand, Right))
            .op(Op::infix(or, Right))
            .op(Op::infix(and, Right))
            .op(Op::infix(eq, Right) | Op::infix(neq, Right))
            .op(
              Op::infix(lte, Right) | Op::infix(gte, Right) | Op::infix(lt, Right) |
              Op::infix(gt, Right) | Op::infix(membership, Right)
            )
            .op(
              Op::infix(concat, Left) | Op::infix(add, Left) | Op::infix(sub, Left) |
              Op::infix(union, Left) | Op::infix(intersection, Left) |
              Op::infix(setminus, Left) | Op::infix(subset, Left)
            )
            .op(Op::infix(mul, Left) | Op::infix(div, Left) | Op::infix(rem, Left) | Op::infix(perm_div, Left))

            // Addition and subtract have equal precedence
            .op(Op::infix(binop, Left))
            // .op(Op::infix(add, Left) | Op::infix(subtract, Left))
            // .op(Op::infix(multiply, Left) | Op::infix(divide, Left))
    };
}

#[test]
fn parse_binop() {
    Silver::parse(Rule::single_exp, "1 + 2 + 3 <==> true || false").unwrap_or_else(|e| panic!("{e}"));

    // assert!(parse.is_ok())
}

#[test]
fn parse_ternary() {
    Silver::parse(Rule::single_exp, "1 ? 2 : 3 ? 3 : 5").unwrap_or_else(|e| panic!("{e}"));
}
