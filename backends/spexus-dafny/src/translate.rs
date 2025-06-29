//! Translator from Dafny specifications to Spexus specifications.
//!
//! This module implements the translation logic that converts parsed Dafny method specifications
//! into Spexus specifications. It handles the mapping between Dafny's requires/ensures clauses
//! and Spexus's prec/post clauses, as well as expression translation.

use crate::parser::{DafnyExpression, DafnyMethodSpec};
use spexus_lang::syntax::{Atom, SpecCore, Term, Value, Variable};

/// Translate a Dafny method specification to a Spexus specification
pub fn translate_dafny_to_spexus(dafny_spec: &DafnyMethodSpec) -> SpecCore {
    let mut spec = SpecCore::new(&dafny_spec.name);

    // Translate requires clauses to precondition
    if !dafny_spec.requires_clauses.is_empty() {
        let precondition = translate_requires_clauses(&dafny_spec.requires_clauses);
        spec = spec.with_precondition(precondition);
    }

    // Translate ensures clauses to postcondition
    if !dafny_spec.ensures_clauses.is_empty() {
        let postcondition = translate_ensures_clauses(&dafny_spec.ensures_clauses);
        spec = spec.with_postcondition(postcondition);
    }

    spec
}

/// Translate multiple requires clauses into a single precondition
fn translate_requires_clauses(requires: &[DafnyExpression]) -> Term {
    if requires.is_empty() {
        Term::tt()
    } else if requires.len() == 1 {
        println!("DEBUG: Single requires clause: {:?}", requires[0]);
        translate_expression(&requires[0])
    } else {
        // Conjoin multiple requires clauses
        let mut result = translate_expression(&requires[0]);
        for clause in &requires[1..] {
            result = Term::And(Box::new(result), Box::new(translate_expression(clause)));
        }
        result
    }
}

/// Translate multiple ensures clauses into a single postcondition
fn translate_ensures_clauses(ensures: &[DafnyExpression]) -> Term {
    if ensures.is_empty() {
        Term::tt()
    } else if ensures.len() == 1 {
        translate_expression(&ensures[0])
    } else {
        // Conjoin multiple ensures clauses
        let mut result = translate_expression(&ensures[0]);
        for clause in &ensures[1..] {
            result = Term::And(Box::new(result), Box::new(translate_expression(clause)));
        }
        result
    }
}

/// Translate a Dafny expression to a Spexus term
fn translate_expression(expr: &DafnyExpression) -> Term {
    match expr {
        // Literals
        DafnyExpression::Integer(n) => Term::Value(Value::Atom(Atom::Integer(*n))),
        DafnyExpression::String(s) => Term::Value(Value::Atom(Atom::String(s.clone()))),
        DafnyExpression::Boolean(b) => Term::Value(Value::Atom(Atom::Bool(*b))),
        DafnyExpression::Variable(name) => Term::Variable(Variable { name: name.clone() }),

        // Arithmetic
        DafnyExpression::Add(l, r) => Term::Add(
            Box::new(translate_expression(l)),
            Box::new(translate_expression(r)),
        ),
        DafnyExpression::Sub(l, r) => Term::Sub(
            Box::new(translate_expression(l)),
            Box::new(translate_expression(r)),
        ),
        DafnyExpression::Mul(l, r) => Term::Mul(
            Box::new(translate_expression(l)),
            Box::new(translate_expression(r)),
        ),
        DafnyExpression::Div(l, r) => Term::Div(
            Box::new(translate_expression(l)),
            Box::new(translate_expression(r)),
        ),
        DafnyExpression::Mod(l, r) => Term::Mod(
            Box::new(translate_expression(l)),
            Box::new(translate_expression(r)),
        ),

        // Comparisons
        DafnyExpression::Eq(l, r) => Term::Eq(
            Box::new(translate_expression(l)),
            Box::new(translate_expression(r)),
        ),
        DafnyExpression::Ne(l, r) => Term::Ne(
            Box::new(translate_expression(l)),
            Box::new(translate_expression(r)),
        ),
        DafnyExpression::Lt(l, r) => Term::Lt(
            Box::new(translate_expression(l)),
            Box::new(translate_expression(r)),
        ),
        DafnyExpression::Le(l, r) => Term::Le(
            Box::new(translate_expression(l)),
            Box::new(translate_expression(r)),
        ),
        DafnyExpression::Gt(l, r) => Term::Gt(
            Box::new(translate_expression(l)),
            Box::new(translate_expression(r)),
        ),
        DafnyExpression::Ge(l, r) => Term::Ge(
            Box::new(translate_expression(l)),
            Box::new(translate_expression(r)),
        ),

        // Logical operators
        DafnyExpression::And(l, r) => Term::And(
            Box::new(translate_expression(l)),
            Box::new(translate_expression(r)),
        ),
        DafnyExpression::Or(l, r) => Term::Or(
            Box::new(translate_expression(l)),
            Box::new(translate_expression(r)),
        ),
        DafnyExpression::Not(t) => Term::Not(Box::new(translate_expression(t))),
        DafnyExpression::Implies(l, r) => Term::Implies(
            Box::new(translate_expression(l)),
            Box::new(translate_expression(r)),
        ),

        // Array operations
        DafnyExpression::Length(arr) => Term::Len(Box::new(translate_expression(arr))),
        DafnyExpression::Index(arr, idx) => Term::Index(
            Box::new(translate_expression(arr)),
            Box::new(translate_expression(idx)),
        ),
        DafnyExpression::Slice(arr, start, end) => Term::Slice(
            Box::new(translate_expression(arr)),
            Box::new(translate_expression(start)),
            Box::new(translate_expression(end)),
        ),

        // Quantifiers
        DafnyExpression::Forall { variables, body } => {
            if variables.len() == 1 {
                Term::Forall {
                    var: Variable {
                        name: variables[0].clone(),
                    },
                    domain: Box::new(Term::tt()), // Default domain
                    body: Box::new(translate_expression(body)),
                }
            } else {
                // Handle multiple variables by nesting foralls
                let mut result = translate_expression(body);
                for var in variables.iter().rev() {
                    result = Term::Forall {
                        var: Variable { name: var.clone() },
                        domain: Box::new(Term::tt()),
                        body: Box::new(result),
                    };
                }
                result
            }
        }
        DafnyExpression::Exists { variables, body } => {
            if variables.len() == 1 {
                Term::Exists {
                    var: Variable {
                        name: variables[0].clone(),
                    },
                    domain: Box::new(Term::tt()), // Default domain
                    body: Box::new(translate_expression(body)),
                }
            } else {
                // Handle multiple variables by nesting exists
                let mut result = translate_expression(body);
                for var in variables.iter().rev() {
                    result = Term::Exists {
                        var: Variable { name: var.clone() },
                        domain: Box::new(Term::tt()),
                        body: Box::new(result),
                    };
                }
                result
            }
        }

        // Function calls
        DafnyExpression::FunctionCall { name, arguments } => {
            // Handle special cases for common Dafny functions
            match name.as_str() {
                "Length" | "length" => {
                    if arguments.len() == 1 {
                        Term::Len(Box::new(translate_expression(&arguments[0])))
                    } else {
                        // Fallback to function call
                        Term::Variable(Variable {
                            name: format!("{}({})", name, arguments.len()),
                        })
                    }
                }
                _ => {
                    // For now, treat function calls as variables
                    // In a more complete implementation, we'd need to handle function definitions
                    Term::Variable(Variable {
                        name: format!("{}({})", name, arguments.len()),
                    })
                }
            }
        }
    }
}

/// Translate a complete Dafny specification string to Spexus format
pub fn translate_dafny_string_to_spexus(
    input: &str,
) -> Result<Vec<SpecCore>, Box<dyn std::error::Error>> {
    use crate::parser::DafnyParser;

    let dafny_specs = DafnyParser::parse_dafny(input)?;
    let mut spexus_specs = Vec::new();

    for dafny_spec in dafny_specs {
        spexus_specs.push(translate_dafny_to_spexus(&dafny_spec));
    }

    Ok(spexus_specs)
}

#[cfg(test)]
mod dfy_tests {
    use super::*;
    use crate::parser::DafnyParser;

    #[test]
    fn test_translate_simple_divide() {
        let input = r#"
        method divide(dividend: int, divisor: int)
            requires divisor != 0
            ensures result == dividend / divisor
        {
            ...
        }
        "#;

        let dafny_specs = DafnyParser::parse_dafny(input).unwrap();
        assert_eq!(dafny_specs.len(), 1);

        let spexus_spec = translate_dafny_to_spexus(&dafny_specs[0]);
        assert_eq!(spexus_spec.symbol(), "divide");

        // Check precondition
        let expected_prec = Term::Ne(Box::new(Term::var("divisor")), Box::new(Term::int(0)));
        assert_eq!(spexus_spec.precondition(), &expected_prec);

        // Check postcondition
        let expected_post = Term::Eq(
            Box::new(Term::var("result")),
            Box::new(Term::Div(
                Box::new(Term::var("dividend")),
                Box::new(Term::var("divisor")),
            )),
        );
        assert_eq!(spexus_spec.postcondition(), &expected_post);
    }

    // TODO: Fix this test
    #[ignore]
    #[test]
    fn test_translate_array_method() {
        let input = r#"
        method safe_get(arr: array<int>, index: int)
            requires index >= 0 && index < arr.Length
            ensures result == arr[index]
        {
            ...
        }
        "#;

        let dafny_specs = DafnyParser::parse_dafny(input).unwrap();
        assert_eq!(dafny_specs.len(), 1);

        let spexus_spec = translate_dafny_to_spexus(&dafny_specs[0]);
        assert_eq!(spexus_spec.symbol(), "safe_get");

        // Debug: print what the precondition looks like
        println!("DEBUG: Precondition: {:?}", spexus_spec.precondition());

        // Check precondition (should be conjunction of two conditions)
        let prec = spexus_spec.precondition();
        match prec {
            Term::And(l, r) => {
                // Check first part: index >= 0
                match l.as_ref() {
                    Term::Ge(lhs, rhs) => {
                        assert_eq!(lhs.as_variable().unwrap().name(), "index");
                        assert_eq!(rhs.as_ref(), &Term::int(0));
                    }
                    _ => panic!("Expected Ge in precondition"),
                }
                // Check second part: index < arr.length
                match r.as_ref() {
                    Term::Lt(lhs, rhs) => {
                        assert_eq!(lhs.as_variable().unwrap().name(), "index");
                        assert_eq!(rhs.as_ref(), &Term::Len(Box::new(Term::var("arr"))));
                    }
                    _ => panic!("Expected Lt in precondition"),
                }
            }
            _ => panic!("Expected And in precondition"),
        }

        // Check postcondition
        let expected_post = Term::Eq(
            Box::new(Term::var("result")),
            Box::new(Term::Index(
                Box::new(Term::var("arr")),
                Box::new(Term::var("index")),
            )),
        );
        assert_eq!(spexus_spec.postcondition(), &expected_post);
    }

    #[test]
    fn test_translate_multiple_requires() {
        let input = r#"
        method complex_method(x: int, y: int)
            requires x > 0
            requires y > 0
            ensures result > 0
        {
            ...
        }
        "#;

        let dafny_specs = DafnyParser::parse_dafny(input).unwrap();
        let spexus_spec = translate_dafny_to_spexus(&dafny_specs[0]);

        // Check that multiple requires are conjoined
        let prec = spexus_spec.precondition();
        match prec {
            Term::And(l, r) => {
                // First condition: x > 0
                match l.as_ref() {
                    Term::Gt(lhs, rhs) => {
                        assert_eq!(lhs.as_variable().unwrap().name(), "x");
                        assert_eq!(rhs.as_ref(), &Term::int(0));
                    }
                    _ => panic!("Expected Gt in first precondition"),
                }
                // Second condition: y > 0
                match r.as_ref() {
                    Term::Gt(lhs, rhs) => {
                        assert_eq!(lhs.as_variable().unwrap().name(), "y");
                        assert_eq!(rhs.as_ref(), &Term::int(0));
                    }
                    _ => panic!("Expected Gt in second precondition"),
                }
            }
            _ => panic!("Expected And in precondition"),
        }
    }
}
