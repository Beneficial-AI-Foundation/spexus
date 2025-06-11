//! Parser for the Spexus specification language.
//!
//! This module implements the parser for Spexus using the `pest` parsing library.
//! It converts text input into the AST structures defined in the `syntax` module.
//!
//! The parser handles:
//! - Specification entries with preconditions, invariants, and postconditions
//! - Quantifiers (forall, exists) with optional domains
//! - Terms in the propositional calculus (arithmetic, logical, and comparison operations)
//! - Array operations (length, indexing, slicing, membership)
//! - Literals (integers, strings, booleans, lists)
//!
//! The grammar is defined in `spexus.pest` and follows operator precedence rules
//! for arithmetic, logical, and comparison operations.
//!
//! # Example
//! ```
//! use lang::parser::SpexusParser;
//!
//! let input = r#"
//! spec divide {
//!     prec: divisor != 0
//!     post: result == dividend / divisor
//! }
//! "#;
//!
//! let spexus = SpexusParser::parse_spexus(input).unwrap();
//! ```

use crate::syntax::*;
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "spexus.pest"]
pub struct SpexusParser;

pub type ParseResult<T> = Result<T, Box<dyn std::error::Error>>;

impl SpexusParser {
    pub fn parse_spexus(input: &str) -> ParseResult<Spexus> {
        let pairs = Self::parse(Rule::spexus, input)?;
        let pair = pairs.into_iter().next().unwrap();
        Ok(parse_spexus_inner(pair))
    }
}

fn parse_spexus_inner(pair: pest::iterators::Pair<Rule>) -> Spexus {
    let mut entries = Vec::new();

    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::spec_entry => {
                entries.push(parse_spec_entry(inner_pair));
            }
            Rule::EOI => {} // End of input
            _ => unreachable!(),
        }
    }

    Spexus { entries }
}

fn parse_spec_entry(pair: pest::iterators::Pair<Rule>) -> SpecEntry {
    let mut quantifiers = Vec::new();
    let mut core_opt = None;

    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::quantifier => {
                quantifiers.push(parse_quantifier(inner_pair));
            }
            Rule::spec_core => {
                core_opt = Some(parse_spec_core(inner_pair));
            }
            _ => unreachable!(),
        }
    }

    SpecEntry {
        quantifiers,
        core: core_opt.expect("spec_entry must have spec_core"),
    }
}

fn parse_quantifier(pair: pest::iterators::Pair<Rule>) -> Quantifier {
    let inner_pair = pair.into_inner().next().unwrap();

    match inner_pair.as_rule() {
        Rule::forall_quant => {
            let mut inner = inner_pair.into_inner();
            let var_name = inner.next().unwrap().as_str().to_string();
            let domain = inner.next().map(parse_term);

            Quantifier::Forall {
                var: Variable { name: var_name },
                domain,
            }
        }
        Rule::exists_quant => {
            let mut inner = inner_pair.into_inner();
            let var_name = inner.next().unwrap().as_str().to_string();
            let domain = inner.next().map(parse_term);

            Quantifier::Exists {
                var: Variable { name: var_name },
                domain,
            }
        }
        _ => unreachable!(),
    }
}

fn parse_spec_core(pair: pest::iterators::Pair<Rule>) -> SpecCore {
    let mut symbol = String::new();
    let mut precondition = Term::tt();
    let mut invariant = Term::tt();
    let mut postcondition = Term::tt();

    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::identifier => {
                // This is the spec name right after "spec"
                if symbol.is_empty() {
                    symbol = inner_pair.as_str().to_string();
                }
            }
            Rule::prec_decl => {
                precondition = parse_term(inner_pair.into_inner().next().unwrap());
            }
            Rule::inv_decl => {
                invariant = parse_term(inner_pair.into_inner().next().unwrap());
            }
            Rule::post_decl => {
                postcondition = parse_term(inner_pair.into_inner().next().unwrap());
            }
            _ => unreachable!("Unexpected rule in spec_core: {:?}", inner_pair.as_rule()),
        }
    }

    SpecCore {
        symbol,
        precondition,
        invariant,
        postcondition,
    }
}

fn parse_term(pair: pest::iterators::Pair<Rule>) -> Term {
    match pair.as_rule() {
        Rule::term => parse_term(pair.into_inner().next().unwrap()),
        Rule::iff_expr => parse_iff_expr(pair),
        _ => unreachable!(),
    }
}

fn parse_iff_expr(pair: pest::iterators::Pair<Rule>) -> Term {
    let mut inner = pair.into_inner();
    let mut left = parse_implies_expr(inner.next().unwrap());

    while let Some(op_pair) = inner.next() {
        if op_pair.as_str() == "<==>" {
            let right = parse_implies_expr(inner.next().unwrap());
            left = Term::Iff(Box::new(left), Box::new(right));
        }
    }

    left
}

fn parse_implies_expr(pair: pest::iterators::Pair<Rule>) -> Term {
    let mut inner = pair.into_inner();
    let mut left = parse_or_expr(inner.next().unwrap());

    while let Some(op_pair) = inner.next() {
        if op_pair.as_str() == "==>" {
            let right = parse_or_expr(inner.next().unwrap());
            left = Term::Implies(Box::new(left), Box::new(right));
        }
    }

    left
}

fn parse_or_expr(pair: pest::iterators::Pair<Rule>) -> Term {
    let mut inner = pair.into_inner();
    let mut left = parse_and_expr(inner.next().unwrap());

    while let Some(op_pair) = inner.next() {
        if op_pair.as_str() == "||" {
            let right = parse_and_expr(inner.next().unwrap());
            left = Term::Or(Box::new(left), Box::new(right));
        }
    }

    left
}

fn parse_and_expr(pair: pest::iterators::Pair<Rule>) -> Term {
    let mut inner = pair.into_inner();
    let mut left = parse_not_expr(inner.next().unwrap());

    while let Some(op_pair) = inner.next() {
        if op_pair.as_str() == "&&" {
            let right = parse_not_expr(inner.next().unwrap());
            left = Term::And(Box::new(left), Box::new(right));
        }
    }

    left
}

fn parse_not_expr(pair: pest::iterators::Pair<Rule>) -> Term {
    let mut inner = pair.into_inner();
    let first = inner.next().unwrap();

    if first.as_str() == "!" {
        let expr = parse_comparison_expr(inner.next().unwrap());
        Term::Not(Box::new(expr))
    } else {
        parse_comparison_expr(first)
    }
}

fn parse_comparison_expr(pair: pest::iterators::Pair<Rule>) -> Term {
    let mut inner = pair.into_inner();
    let mut left = parse_additive_expr(inner.next().unwrap());

    while let Some(op_pair) = inner.next() {
        let op = op_pair.as_str();
        let right = parse_additive_expr(inner.next().unwrap());

        left = match op {
            "==" => Term::Eq(Box::new(left), Box::new(right)),
            "!=" => Term::Ne(Box::new(left), Box::new(right)),
            "<" => Term::Lt(Box::new(left), Box::new(right)),
            "<=" => Term::Le(Box::new(left), Box::new(right)),
            ">" => Term::Gt(Box::new(left), Box::new(right)),
            ">=" => Term::Ge(Box::new(left), Box::new(right)),
            _ => unreachable!(),
        };
    }

    left
}

fn parse_additive_expr(pair: pest::iterators::Pair<Rule>) -> Term {
    let mut inner = pair.into_inner();
    let mut left = parse_multiplicative_expr(inner.next().unwrap());

    while let Some(op_pair) = inner.next() {
        let op = op_pair.as_str();
        let right = parse_multiplicative_expr(inner.next().unwrap());

        left = match op {
            "+" => Term::Add(Box::new(left), Box::new(right)),
            "-" => Term::Sub(Box::new(left), Box::new(right)),
            _ => unreachable!(),
        };
    }

    left
}

fn parse_multiplicative_expr(pair: pest::iterators::Pair<Rule>) -> Term {
    let mut inner = pair.into_inner();
    let mut left = parse_postfix_expr(inner.next().unwrap());

    while let Some(op_pair) = inner.next() {
        let op = op_pair.as_str();
        let right = parse_postfix_expr(inner.next().unwrap());

        left = match op {
            "*" => Term::Mul(Box::new(left), Box::new(right)),
            "/" => Term::Div(Box::new(left), Box::new(right)),
            "%" => Term::Mod(Box::new(left), Box::new(right)),
            _ => unreachable!(),
        };
    }

    left
}

fn parse_postfix_expr(pair: pest::iterators::Pair<Rule>) -> Term {
    let mut inner = pair.into_inner();
    let mut base = parse_base_expr(inner.next().unwrap());

    for postfix_pair in inner {
        match postfix_pair.as_rule() {
            Rule::postfix_op => {
                let op_inner = postfix_pair.into_inner().next().unwrap();
                match op_inner.as_rule() {
                    Rule::index_op => {
                        let index = parse_term(op_inner.into_inner().next().unwrap());
                        base = Term::Index(Box::new(base), Box::new(index));
                    }
                    Rule::slice_op => {
                        let mut slice_inner = op_inner.into_inner();
                        let start = parse_term(slice_inner.next().unwrap());
                        let end = parse_term(slice_inner.next().unwrap());
                        base = Term::Slice(Box::new(base), Box::new(start), Box::new(end));
                    }
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }

    base
}

fn parse_base_expr(pair: pest::iterators::Pair<Rule>) -> Term {
    match pair.as_rule() {
        Rule::base_expr => parse_base_expr(pair.into_inner().next().unwrap()),
        Rule::integer => {
            let value: i64 = pair.as_str().parse().unwrap();
            Term::Value(Value::Atom(Atom::Integer(value)))
        }
        Rule::string => {
            let s = pair.as_str();
            // Remove quotes
            let content = &s[1..s.len() - 1];
            Term::Value(Value::Atom(Atom::String(content.to_string())))
        }
        Rule::boolean => {
            let value = pair.as_str() == "true";
            Term::Value(Value::Atom(Atom::Bool(value)))
        }
        Rule::identifier => Term::Variable(Variable {
            name: pair.as_str().to_string(),
        }),
        Rule::list => {
            let mut atoms = Vec::new();
            for inner_pair in pair.into_inner() {
                match inner_pair.as_rule() {
                    Rule::integer => {
                        let value: i64 = inner_pair.as_str().parse().unwrap();
                        atoms.push(Atom::Integer(value));
                    }
                    Rule::string => {
                        let s = inner_pair.as_str();
                        let content = &s[1..s.len() - 1];
                        atoms.push(Atom::String(content.to_string()));
                    }
                    Rule::boolean => {
                        let value = inner_pair.as_str() == "true";
                        atoms.push(Atom::Bool(value));
                    }
                    _ => {}
                }
            }
            Term::Value(Value::List(atoms))
        }
        Rule::function_call => parse_function_call(pair),
        Rule::quantified_expr => parse_quantified_expr(pair),
        Rule::paren_expr => parse_term(pair.into_inner().next().unwrap()),
        _ => unreachable!("Unexpected rule in base_expr: {:?}", pair.as_rule()),
    }
}

fn parse_function_call(pair: pest::iterators::Pair<Rule>) -> Term {
    let mut inner = pair.into_inner();
    let func_name = inner.next().unwrap().as_str();
    let args: Vec<Term> = inner.map(parse_term).collect();

    match func_name {
        "len" => {
            assert_eq!(args.len(), 1);
            Term::Len(Box::new(args.into_iter().next().unwrap()))
        }
        "in" => {
            assert_eq!(args.len(), 2);
            let mut args = args.into_iter();
            Term::In(
                Box::new(args.next().unwrap()),
                Box::new(args.next().unwrap()),
            )
        }
        _ => unreachable!("Unknown function: {}", func_name),
    }
}

fn parse_quantified_expr(pair: pest::iterators::Pair<Rule>) -> Term {
    let mut inner = pair.into_inner();
    let quant_type = inner.next().unwrap();

    match quant_type.as_rule() {
        Rule::forall_expr => {
            let mut quant_inner = quant_type.into_inner();
            let var_name = quant_inner.next().unwrap().as_str().to_string();
            let domain_pair = quant_inner.next().unwrap();

            let domain = match domain_pair.as_rule() {
                Rule::range_expr => {
                    let mut range_inner = domain_pair.into_inner();
                    let start = parse_term(range_inner.next().unwrap());
                    let end = parse_term(range_inner.next().unwrap());
                    // Convert range to a term representation
                    // For now, we'll create a synthetic domain term
                    Term::And(
                        Box::new(Term::Ge(Box::new(Term::var(&var_name)), Box::new(start))),
                        Box::new(Term::Lt(Box::new(Term::var(&var_name)), Box::new(end))),
                    )
                }
                _ => parse_term(domain_pair),
            };

            let body = parse_term(inner.next().unwrap());

            Term::Forall {
                var: Variable { name: var_name },
                domain: Box::new(domain),
                body: Box::new(body),
            }
        }
        Rule::exists_expr => {
            let mut quant_inner = quant_type.into_inner();
            let var_name = quant_inner.next().unwrap().as_str().to_string();
            let domain_pair = quant_inner.next().unwrap();

            let domain = match domain_pair.as_rule() {
                Rule::range_expr => {
                    let mut range_inner = domain_pair.into_inner();
                    let start = parse_term(range_inner.next().unwrap());
                    let end = parse_term(range_inner.next().unwrap());
                    Term::And(
                        Box::new(Term::Ge(Box::new(Term::var(&var_name)), Box::new(start))),
                        Box::new(Term::Lt(Box::new(Term::var(&var_name)), Box::new(end))),
                    )
                }
                _ => parse_term(domain_pair),
            };

            let body = parse_term(inner.next().unwrap());

            Term::Exists {
                var: Variable { name: var_name },
                domain: Box::new(domain),
                body: Box::new(body),
            }
        }
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_spec() {
        let input = r#"
        spec divide {
            prec: divisor != 0
            post: result == dividend / divisor
        }
        "#;

        let spexus = SpexusParser::parse_spexus(input).unwrap();
        assert_eq!(spexus.entries.len(), 1);
        assert_eq!(spexus.entries[0].symbol(), "divide");
        assert_eq!(spexus.entries[0].quantifiers.len(), 0);
    }

    #[test]
    fn test_parse_quantified_spec() {
        let input = r#"
        forall T
        exists n: n >= 0
        spec generic_func {
            prec: len(input) == n
            post: len(result) == n
        }
        "#;

        let spexus = SpexusParser::parse_spexus(input).unwrap();
        assert_eq!(spexus.entries.len(), 1);

        let entry = &spexus.entries[0];
        assert_eq!(entry.quantifiers.len(), 2);

        match &entry.quantifiers[0] {
            Quantifier::Forall { var, domain } => {
                assert_eq!(var.name, "T");
                assert!(domain.is_none());
            }
            _ => panic!("Expected forall"),
        }

        match &entry.quantifiers[1] {
            Quantifier::Exists { var, domain } => {
                assert_eq!(var.name, "n");
                assert!(domain.is_some());
            }
            _ => panic!("Expected exists"),
        }
    }

    #[test]
    fn test_parse_array_spec() {
        let input = r#"
        spec safe_get {
            prec: index >= 0 && index < len(arr)
            inv: len(arr) > 0
            post: result == arr[index]
        }
        "#;

        let spexus = SpexusParser::parse_spexus(input).unwrap();
        assert_eq!(spexus.entries.len(), 1);
        assert_eq!(spexus.entries[0].symbol(), "safe_get");
    }

    #[test]
    fn test_parse_indexing() {
        let input = r#"
        spec test_indexing {
            post: result == arr[0] && slice == arr[1..3]
        }
        "#;

        let spexus = SpexusParser::parse_spexus(input).unwrap();
        assert_eq!(spexus.entries.len(), 1);
    }

    #[test]
    fn test_parse_quantified_term() {
        let input = r#"
        spec all_positive {
            post: result <==> forall x in arr: x > 0
        }
        "#;

        let spexus = SpexusParser::parse_spexus(input).unwrap();
        assert_eq!(spexus.entries.len(), 1);
    }
}
