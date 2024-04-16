use pest::pratt_parser::PrattParser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "silver.pest"]
pub struct Silver;

pub mod peg;

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
            // .op(Op::infix(binop, Left))
            // .op(Op::infix(add, Left) | Op::infix(subtract, Left))
            // .op(Op::infix(multiply, Left) | Op::infix(divide, Left))
    };
}

#[cfg(test)]
macro_rules! test_parse {
    ($par:ident, $e:tt) => {
        Silver::parse(Rule::$par, $e).unwrap_or_else(|e| panic!("{e}"))
    };
    ($x:tt) => {
        Silver::parse(Rule::sil_program, $x).unwrap_or_else(|e| panic!("{e}"))
    };
}

#[cfg(test)]
mod test {
    use super::*;
    use pest::Parser;
    #[test]
    fn parse_binop() {
        test_parse!(single_exp, "1 + 2 + 3 <==> true || false");
    }

    #[test]
    fn parse_field() {
        test_parse!(field, "field keys : Seq[Int]");
        test_parse!("field keys : Seq[Int]");
    }

    #[test]
    fn parse_exp() {
        test_parse!(single_exp, "forall a: Int, b: Int :: true ==> true");
        test_parse!(acc_exp, "acc(x.f)");

        test_parse!(single_exp, "unfolding acc(P(x)) in x");
        test_parse!(single_exp, "id(x).g == (unfolding acc(P(x)) in x).g");
        test_parse!(single_exp, "1 == unfolding pred() in ref");
    }

    #[test]
    fn parse_stmt() {
        // test_parse!(single_stmt, "assert id(x).g == (unfolding acc(P(x)) in x).g");
        test_parse!(single_stmt, "{inhale false inhale false }");
        test_parse!(single_stmt, "exhale acc(x.f)");
        test_parse!(single_stmt, "m(x)");
        test_parse!(one_decreases, "decreases if false");
        test_parse!(one_decreases, "decreases f(r.val) if m(r.val) == 1");
        test_parse!(single_stmt, "(b[..1][0]).f := a");
    }

    #[test]
    fn parse_ternary() {
        test_parse!(single_exp, "1 ? 2 : 3 ? 3 : 5");
    }
}
