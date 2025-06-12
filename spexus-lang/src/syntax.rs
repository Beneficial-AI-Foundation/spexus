//! Abstract Syntax Tree (AST) definitions for the Spexus specification language.
//!
//! This module defines the core data structures that represent Spexus specifications
//! in memory. The AST is designed to capture:
//!
//! - Atomic values (integers, strings, booleans)
//! - Terms in the propositional calculus (arithmetic, logical, and comparison operations)
//! - Quantifiers (forall, exists) with their domains
//! - Array operations (length, indexing, slicing, membership)
//! - Specification entries with preconditions, invariants, and postconditions
//!
//! The AST is designed to be:
//! - Immutable (all fields are private, accessed via getters)
//! - Builder-pattern friendly (for easy construction)
//! - Well-documented (with comprehensive doc comments)
//! - Type-safe (using enums for variants)
//!
//! # Example
//! ```
//! use spexus_lang::syntax::{SpecCore, Term};
//!
//! let spec = SpecCore::new("divide")
//!     .with_precondition(Term::Ne(
//!         Box::new(Term::var("divisor")),
//!         Box::new(Term::int(0))
//!     ))
//!     .with_postcondition(Term::Eq(
//!         Box::new(Term::var("result")),
//!         Box::new(Term::Div(
//!             Box::new(Term::var("dividend")),
//!             Box::new(Term::var("divisor"))
//!         ))
//!     ));
//! ```

use std::collections::HashSet;

/// Atomic values in Spexus
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Atom {
    Integer(i64),
    String(String),
    Bool(bool),
}

/// Values in Spexus: either atoms or lists of atoms
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Value {
    Atom(Atom),
    List(Vec<Atom>),
}

/// Variables in formulas
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable {
    pub name: String,
}

/// Terms in the propositional calculus T
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    // Literals
    Value(Value),
    Variable(Variable),

    // Built-in functions
    Len(Box<Term>),                         // len(arr)
    Index(Box<Term>, Box<Term>),            // arr[i]
    Slice(Box<Term>, Box<Term>, Box<Term>), // arr[i..j]
    In(Box<Term>, Box<Term>),               // in(x, arr)

    // Arithmetic
    Add(Box<Term>, Box<Term>), // x + y
    Sub(Box<Term>, Box<Term>), // x - y
    Mul(Box<Term>, Box<Term>), // x * y
    Div(Box<Term>, Box<Term>), // x / y
    Mod(Box<Term>, Box<Term>), // x % y

    // Comparisons
    Eq(Box<Term>, Box<Term>), // x == y
    Ne(Box<Term>, Box<Term>), // x != y
    Lt(Box<Term>, Box<Term>), // x < y
    Le(Box<Term>, Box<Term>), // x <= y
    Gt(Box<Term>, Box<Term>), // x > y
    Ge(Box<Term>, Box<Term>), // x >= y

    // Logical operators
    And(Box<Term>, Box<Term>),     // P && Q
    Or(Box<Term>, Box<Term>),      // P || Q
    Not(Box<Term>),                // !P
    Implies(Box<Term>, Box<Term>), // P ==> Q
    Iff(Box<Term>, Box<Term>),     // P <==> Q

    // Quantifiers
    Forall {
        var: Variable,
        domain: Box<Term>, // collection to quantify over
        body: Box<Term>,   // predicate
    },
    Exists {
        var: Variable,
        domain: Box<Term>,
        body: Box<Term>,
    },

    // Bounded quantifiers
    ForallRange {
        var: Variable,
        start: Box<Term>,
        end: Box<Term>,
        body: Box<Term>,
    },
    ExistsRange {
        var: Variable,
        start: Box<Term>,
        end: Box<Term>,
        body: Box<Term>,
    },
}

/// Quantification binding for metavariables
#[derive(Debug, Clone, PartialEq)]
pub enum Quantifier {
    Forall {
        var: Variable,
        domain: Option<Term>, // None for unconstrained quantification
    },
    Exists {
        var: Variable,
        domain: Option<Term>,
    },
}

/// The basic 4-struct specification
#[derive(Debug, Clone, PartialEq)]
pub struct SpecCore {
    pub symbol: String,
    pub precondition: Term,
    pub invariant: Term,
    pub postcondition: Term,
}

/// A complete specification entry: quantifiers + core spec
#[derive(Debug, Clone, PartialEq)]
pub struct SpecEntry {
    pub quantifiers: Vec<Quantifier>, // Prefix quantification for metavariables
    pub core: SpecCore,
}

/// A complete Spexus specification (list of spec entries)
#[derive(Debug, Clone, PartialEq)]
pub struct Spexus {
    pub entries: Vec<SpecEntry>,
}

impl Atom {
    /// Get a reference to the integer value if this is an Integer atom
    pub fn as_integer(&self) -> Option<&i64> {
        match self {
            Atom::Integer(n) => Some(n),
            _ => None,
        }
    }

    /// Get a reference to the string value if this is a String atom
    pub fn as_string(&self) -> Option<&str> {
        match self {
            Atom::String(s) => Some(s),
            _ => None,
        }
    }

    /// Get the boolean value if this is a Bool atom
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Atom::Bool(b) => Some(*b),
            _ => None,
        }
    }
}

impl Value {
    /// Get a reference to the atom if this is an Atom value
    pub fn as_atom(&self) -> Option<&Atom> {
        match self {
            Value::Atom(a) => Some(a),
            _ => None,
        }
    }

    /// Get a reference to the list if this is a List value
    pub fn as_list(&self) -> Option<&Vec<Atom>> {
        match self {
            Value::List(l) => Some(l),
            _ => None,
        }
    }
}

impl Variable {
    /// Get a reference to the variable name
    pub fn name(&self) -> &str {
        &self.name
    }
}

impl Term {
    /// Convenience constructor for True
    pub fn tt() -> Self {
        Term::Value(Value::Atom(Atom::Bool(true)))
    }

    /// Convenience constructor for False
    pub fn ff() -> Self {
        Term::Value(Value::Atom(Atom::Bool(false)))
    }

    /// Convenience constructor for integer literals
    pub fn int(n: i64) -> Self {
        Term::Value(Value::Atom(Atom::Integer(n)))
    }

    /// Convenience constructor for string literals
    pub fn string(s: impl Into<String>) -> Self {
        Term::Value(Value::Atom(Atom::String(s.into())))
    }

    /// Convenience constructor for variables
    pub fn var(name: impl Into<String>) -> Self {
        Term::Variable(Variable { name: name.into() })
    }

    /// Convenience constructor for list literals
    pub fn list(atoms: Vec<Atom>) -> Self {
        Term::Value(Value::List(atoms))
    }

    /// Get a reference to the value if this is a Value term
    pub fn as_value(&self) -> Option<&Value> {
        match self {
            Term::Value(v) => Some(v),
            _ => None,
        }
    }

    /// Get a reference to the variable if this is a Variable term
    pub fn as_variable(&self) -> Option<&Variable> {
        match self {
            Term::Variable(v) => Some(v),
            _ => None,
        }
    }

    /// Get a reference to the inner term if this is a Len term
    pub fn as_len(&self) -> Option<&Term> {
        match self {
            Term::Len(t) => Some(t),
            _ => None,
        }
    }

    /// Get references to the array and index terms if this is an Index term
    pub fn as_index(&self) -> Option<(&Term, &Term)> {
        match self {
            Term::Index(arr, idx) => Some((arr, idx)),
            _ => None,
        }
    }

    /// Get references to the array, start, and end terms if this is a Slice term
    pub fn as_slice(&self) -> Option<(&Term, &Term, &Term)> {
        match self {
            Term::Slice(arr, start, end) => Some((arr, start, end)),
            _ => None,
        }
    }

    /// Get references to the element and array terms if this is an In term
    pub fn as_in(&self) -> Option<(&Term, &Term)> {
        match self {
            Term::In(x, arr) => Some((x, arr)),
            _ => None,
        }
    }

    /// Get references to the left and right terms if this is an arithmetic term
    pub fn as_arithmetic(&self) -> Option<(&Term, &Term)> {
        match self {
            Term::Add(l, r)
            | Term::Sub(l, r)
            | Term::Mul(l, r)
            | Term::Div(l, r)
            | Term::Mod(l, r) => Some((l, r)),
            _ => None,
        }
    }

    /// Get references to the left and right terms if this is a comparison term
    pub fn as_comparison(&self) -> Option<(&Term, &Term)> {
        match self {
            Term::Eq(l, r)
            | Term::Ne(l, r)
            | Term::Lt(l, r)
            | Term::Le(l, r)
            | Term::Gt(l, r)
            | Term::Ge(l, r) => Some((l, r)),
            _ => None,
        }
    }

    /// Get references to the left and right terms if this is a logical term
    pub fn as_logical(&self) -> Option<(&Term, &Term)> {
        match self {
            Term::And(l, r) | Term::Or(l, r) | Term::Implies(l, r) | Term::Iff(l, r) => {
                Some((l, r))
            }
            _ => None,
        }
    }

    /// Get a reference to the inner term if this is a Not term
    pub fn as_not(&self) -> Option<&Term> {
        match self {
            Term::Not(t) => Some(t),
            _ => None,
        }
    }

    /// Get the variable, domain, and body if this is a Forall term
    pub fn as_forall(&self) -> Option<(&Variable, &Term, &Term)> {
        match self {
            Term::Forall { var, domain, body } => Some((var, domain, body)),
            _ => None,
        }
    }

    /// Get the variable, domain, and body if this is an Exists term
    pub fn as_exists(&self) -> Option<(&Variable, &Term, &Term)> {
        match self {
            Term::Exists { var, domain, body } => Some((var, domain, body)),
            _ => None,
        }
    }

    /// Get the variable, start, end, and body if this is a ForallRange term
    pub fn as_forall_range(&self) -> Option<(&Variable, &Term, &Term, &Term)> {
        match self {
            Term::ForallRange {
                var,
                start,
                end,
                body,
            } => Some((var, start, end, body)),
            _ => None,
        }
    }

    /// Get the variable, start, end, and body if this is an ExistsRange term
    pub fn as_exists_range(&self) -> Option<(&Variable, &Term, &Term, &Term)> {
        match self {
            Term::ExistsRange {
                var,
                start,
                end,
                body,
            } => Some((var, start, end, body)),
            _ => None,
        }
    }
}

impl Quantifier {
    /// Get the variable and domain if this is a Forall quantifier
    pub fn as_forall(&self) -> Option<(&Variable, Option<&Term>)> {
        match self {
            Quantifier::Forall { var, domain } => Some((var, domain.as_ref())),
            _ => None,
        }
    }

    /// Get the variable and domain if this is an Exists quantifier
    pub fn as_exists(&self) -> Option<(&Variable, Option<&Term>)> {
        match self {
            Quantifier::Exists { var, domain } => Some((var, domain.as_ref())),
            _ => None,
        }
    }
}

impl SpecCore {
    /// Create a new spec core with default True for prec/inv/post
    pub fn new(symbol: impl Into<String>) -> Self {
        Self {
            symbol: symbol.into(),
            precondition: Term::tt(),
            invariant: Term::tt(),
            postcondition: Term::tt(),
        }
    }

    /// Builder pattern for setting precondition
    pub fn with_precondition(mut self, prec: Term) -> Self {
        self.precondition = prec;
        self
    }

    /// Builder pattern for setting invariant
    pub fn with_invariant(mut self, inv: Term) -> Self {
        self.invariant = inv;
        self
    }

    /// Builder pattern for setting postcondition
    pub fn with_postcondition(mut self, post: Term) -> Self {
        self.postcondition = post;
        self
    }

    /// Get a reference to the precondition
    pub fn precondition(&self) -> &Term {
        &self.precondition
    }

    /// Get a reference to the invariant
    pub fn invariant(&self) -> &Term {
        &self.invariant
    }

    /// Get a reference to the postcondition
    pub fn postcondition(&self) -> &Term {
        &self.postcondition
    }

    /// Get a reference to the symbol
    pub fn symbol(&self) -> &str {
        &self.symbol
    }

    /// Extract all free variables from a term
    fn extract_variables_from_term(term: &Term) -> HashSet<String> {
        let mut vars = HashSet::new();
        match term {
            Term::Variable(var) => {
                vars.insert(var.name.clone());
            }
            Term::Len(term) => vars.extend(Self::extract_variables_from_term(term)),
            Term::Index(arr, idx) => {
                vars.extend(Self::extract_variables_from_term(arr));
                vars.extend(Self::extract_variables_from_term(idx));
            }
            Term::Slice(arr, start, end) => {
                vars.extend(Self::extract_variables_from_term(arr));
                vars.extend(Self::extract_variables_from_term(start));
                vars.extend(Self::extract_variables_from_term(end));
            }
            Term::In(x, arr) => {
                vars.extend(Self::extract_variables_from_term(x));
                vars.extend(Self::extract_variables_from_term(arr));
            }
            Term::Add(l, r)
            | Term::Sub(l, r)
            | Term::Mul(l, r)
            | Term::Div(l, r)
            | Term::Mod(l, r) => {
                vars.extend(Self::extract_variables_from_term(l));
                vars.extend(Self::extract_variables_from_term(r));
            }
            Term::Eq(l, r)
            | Term::Ne(l, r)
            | Term::Lt(l, r)
            | Term::Le(l, r)
            | Term::Gt(l, r)
            | Term::Ge(l, r) => {
                vars.extend(Self::extract_variables_from_term(l));
                vars.extend(Self::extract_variables_from_term(r));
            }
            Term::And(l, r) | Term::Or(l, r) | Term::Implies(l, r) | Term::Iff(l, r) => {
                vars.extend(Self::extract_variables_from_term(l));
                vars.extend(Self::extract_variables_from_term(r));
            }
            Term::Not(term) => vars.extend(Self::extract_variables_from_term(term)),
            Term::Forall { var, domain, body } | Term::Exists { var, domain, body } => {
                vars.extend(Self::extract_variables_from_term(domain));
                let mut body_vars = Self::extract_variables_from_term(body);
                body_vars.remove(&var.name);
                vars.extend(body_vars);
            }
            Term::ForallRange {
                var,
                start,
                end,
                body,
            }
            | Term::ExistsRange {
                var,
                start,
                end,
                body,
            } => {
                vars.extend(Self::extract_variables_from_term(start));
                vars.extend(Self::extract_variables_from_term(end));
                let mut body_vars = Self::extract_variables_from_term(body);
                body_vars.remove(&var.name);
                vars.extend(body_vars);
            }
            Term::Value(_) => {}
        }
        vars
    }

    /// Get a list of all free variables used in the specification
    pub fn get_free_variables(&self) -> Vec<String> {
        let mut vars = HashSet::new();

        // Collect variables from all three conditions
        vars.extend(Self::extract_variables_from_term(&self.precondition));
        vars.extend(Self::extract_variables_from_term(&self.invariant));
        vars.extend(Self::extract_variables_from_term(&self.postcondition));

        // Convert to sorted vector
        let mut result: Vec<String> = vars.into_iter().collect();
        result.sort();
        result
    }
}

impl SpecEntry {
    /// Create a new spec entry with no quantifiers
    pub fn new(symbol: impl Into<String>) -> Self {
        Self {
            quantifiers: Vec::new(),
            core: SpecCore::new(symbol),
        }
    }

    /// Create from an existing SpecCore
    pub fn from_core(core: SpecCore) -> Self {
        Self {
            quantifiers: Vec::new(),
            core,
        }
    }

    /// Add a forall quantifier
    pub fn forall(mut self, var: impl Into<String>, domain: Option<Term>) -> Self {
        self.quantifiers.push(Quantifier::Forall {
            var: Variable { name: var.into() },
            domain,
        });
        self
    }

    /// Add an exists quantifier
    pub fn exists(mut self, var: impl Into<String>, domain: Option<Term>) -> Self {
        self.quantifiers.push(Quantifier::Exists {
            var: Variable { name: var.into() },
            domain,
        });
        self
    }

    /// Builder pattern for setting precondition
    pub fn with_precondition(mut self, prec: Term) -> Self {
        self.core.precondition = prec;
        self
    }

    /// Builder pattern for setting invariant
    pub fn with_invariant(mut self, inv: Term) -> Self {
        self.core.invariant = inv;
        self
    }

    /// Builder pattern for setting postcondition
    pub fn with_postcondition(mut self, post: Term) -> Self {
        self.core.postcondition = post;
        self
    }

    /// Get the symbol name
    pub fn symbol(&self) -> &str {
        &self.core.symbol
    }

    /// Get a reference to the quantifiers
    pub fn quantifiers(&self) -> &Vec<Quantifier> {
        &self.quantifiers
    }

    /// Get a reference to the core specification
    pub fn core(&self) -> &SpecCore {
        &self.core
    }
}

impl Spexus {
    /// Create empty specification
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    /// Add a spec entry
    pub fn add_entry(mut self, entry: SpecEntry) -> Self {
        self.entries.push(entry);
        self
    }

    /// Find a spec entry by symbol name
    pub fn find_entry(&self, symbol: &str) -> Option<&SpecEntry> {
        self.entries.iter().find(|entry| entry.symbol() == symbol)
    }

    /// Get a vector of all entry symbol names
    pub fn entry_names(&self) -> Vec<&str> {
        self.entries.iter().map(|entry| entry.symbol()).collect()
    }

    /// Get a reference to the raw entries vector
    pub fn entries(&self) -> &Vec<SpecEntry> {
        &self.entries
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_spec() {
        let spec = SpecEntry::new("divide")
            .with_precondition(Term::Ne(
                Box::new(Term::var("divisor")),
                Box::new(Term::int(0)),
            ))
            .with_postcondition(Term::Eq(
                Box::new(Term::var("result")),
                Box::new(Term::Div(
                    Box::new(Term::var("dividend")),
                    Box::new(Term::var("divisor")),
                )),
            ));

        assert_eq!(spec.symbol(), "divide");
        assert_eq!(spec.quantifiers.len(), 0);
    }

    #[test]
    fn test_quantified_metavariables() {
        // Example: forall T, exists n, {symbol: "generic_func", prec: len(input) == n, ...}
        let spec = SpecEntry::new("generic_func")
            .forall("T", None) // Type parameter
            .exists(
                "n",
                Some(Term::Ge(Box::new(Term::var("n")), Box::new(Term::int(0)))),
            ) // Size parameter
            .with_precondition(Term::Eq(
                Box::new(Term::Len(Box::new(Term::var("input")))),
                Box::new(Term::var("n")),
            ))
            .with_postcondition(Term::Eq(
                Box::new(Term::Len(Box::new(Term::var("result")))),
                Box::new(Term::var("n")),
            ));

        assert_eq!(spec.quantifiers.len(), 2);
        match &spec.quantifiers[0] {
            Quantifier::Forall { var, domain } => {
                assert_eq!(var.name, "T");
                assert!(domain.is_none());
            }
            _ => panic!("Expected forall"),
        }
        match &spec.quantifiers[1] {
            Quantifier::Exists { var, domain } => {
                assert_eq!(var.name, "n");
                assert!(domain.is_some());
            }
            _ => panic!("Expected exists"),
        }
    }

    #[test]
    fn test_array_length_spec() {
        let spec = SpecEntry::new("get_length").with_postcondition(Term::Eq(
            Box::new(Term::var("result")),
            Box::new(Term::Len(Box::new(Term::var("input_array")))),
        ));

        assert_eq!(spec.symbol(), "get_length");
    }

    #[test]
    fn test_quantified_spec() {
        // forall(i: 0..len(arr), arr[i] > 0)
        let all_positive = Term::ForallRange {
            var: Variable {
                name: "i".to_string(),
            },
            start: Box::new(Term::int(0)),
            end: Box::new(Term::Len(Box::new(Term::var("arr")))),
            body: Box::new(Term::Gt(
                Box::new(Term::Index(
                    Box::new(Term::var("arr")),
                    Box::new(Term::var("i")),
                )),
                Box::new(Term::int(0)),
            )),
        };

        let spec = SpecEntry::new("all_positive").with_postcondition(Term::Iff(
            Box::new(Term::var("result")),
            Box::new(all_positive),
        ));

        assert_eq!(spec.symbol(), "all_positive");
    }

    #[test]
    fn test_complete_spexus() {
        let spexus = Spexus::new()
            .add_entry(SpecEntry::new("divide").with_precondition(Term::Ne(
                Box::new(Term::var("divisor")),
                Box::new(Term::int(0)),
            )))
            .add_entry(
                SpecEntry::new("safe_get")
                    .with_precondition(Term::And(
                        Box::new(Term::Ge(
                            Box::new(Term::var("index")),
                            Box::new(Term::int(0)),
                        )),
                        Box::new(Term::Lt(
                            Box::new(Term::var("index")),
                            Box::new(Term::Len(Box::new(Term::var("arr")))),
                        )),
                    ))
                    .with_postcondition(Term::Eq(
                        Box::new(Term::var("result")),
                        Box::new(Term::Index(
                            Box::new(Term::var("arr")),
                            Box::new(Term::var("index")),
                        )),
                    )),
            );

        assert_eq!(spexus.entries.len(), 2);
        assert!(spexus.find_entry("divide").is_some());
        assert!(spexus.find_entry("safe_get").is_some());
        assert!(spexus.find_entry("nonexistent").is_none());
    }

    #[test]
    fn test_free_variables() {
        let spec = SpecCore::new("test")
            .with_precondition(Term::And(
                Box::new(Term::Gt(Box::new(Term::var("x")), Box::new(Term::int(0)))),
                Box::new(Term::Lt(Box::new(Term::var("y")), Box::new(Term::int(10)))),
            ))
            .with_invariant(Term::Forall {
                var: Variable {
                    name: "i".to_string(),
                },
                domain: Box::new(Term::var("arr")),
                body: Box::new(Term::Gt(
                    Box::new(Term::Index(
                        Box::new(Term::var("arr")),
                        Box::new(Term::var("i")),
                    )),
                    Box::new(Term::int(0)),
                )),
            })
            .with_postcondition(Term::Eq(
                Box::new(Term::var("result")),
                Box::new(Term::Add(
                    Box::new(Term::var("x")),
                    Box::new(Term::var("y")),
                )),
            ));

        let vars = spec.get_free_variables();
        assert_eq!(vars, vec!["arr", "result", "x", "y"]);
    }

    #[test]
    fn test_free_variables_empty_spec() {
        let spec = SpecCore::new("empty");
        let vars = spec.get_free_variables();
        assert!(vars.is_empty());
    }

    #[test]
    fn test_free_variables_nested_quantifiers() {
        let spec = SpecCore::new("nested").with_precondition(Term::Forall {
            var: Variable {
                name: "i".to_string(),
            },
            domain: Box::new(Term::var("arr1")),
            body: Box::new(Term::Forall {
                var: Variable {
                    name: "j".to_string(),
                },
                domain: Box::new(Term::var("arr2")),
                body: Box::new(Term::And(
                    Box::new(Term::var("global")),
                    Box::new(Term::Gt(
                        Box::new(Term::Index(
                            Box::new(Term::var("arr1")),
                            Box::new(Term::var("i")),
                        )),
                        Box::new(Term::Index(
                            Box::new(Term::var("arr2")),
                            Box::new(Term::var("j")),
                        )),
                    )),
                )),
            }),
        });

        let vars = spec.get_free_variables();
        assert_eq!(vars, vec!["arr1", "arr2", "global"]);
    }

    #[test]
    fn test_free_variables_array_operations() {
        let spec = SpecCore::new("arrays")
            .with_precondition(Term::And(
                Box::new(Term::In(
                    Box::new(Term::var("x")),
                    Box::new(Term::var("arr")),
                )),
                Box::new(Term::Eq(
                    Box::new(Term::Len(Box::new(Term::var("arr")))),
                    Box::new(Term::var("n")),
                )),
            ))
            .with_postcondition(Term::And(
                Box::new(Term::Gt(
                    Box::new(Term::Index(
                        Box::new(Term::var("arr")),
                        Box::new(Term::var("i")),
                    )),
                    Box::new(Term::int(0)),
                )),
                Box::new(Term::Eq(
                    Box::new(Term::Slice(
                        Box::new(Term::var("arr")),
                        Box::new(Term::var("start")),
                        Box::new(Term::var("end")),
                    )),
                    Box::new(Term::var("result")),
                )),
            ));

        let vars = spec.get_free_variables();
        assert_eq!(vars, vec!["arr", "end", "i", "n", "result", "start", "x"]);
    }

    #[test]
    fn test_free_variables_logical_operations() {
        let spec = SpecCore::new("logical")
            .with_precondition(Term::Not(Box::new(Term::var("flag"))))
            .with_invariant(Term::Implies(
                Box::new(Term::var("a")),
                Box::new(Term::Or(Box::new(Term::var("b")), Box::new(Term::var("c")))),
            ))
            .with_postcondition(Term::Iff(
                Box::new(Term::var("result")),
                Box::new(Term::And(
                    Box::new(Term::var("x")),
                    Box::new(Term::var("y")),
                )),
            ));

        let vars = spec.get_free_variables();
        assert_eq!(vars, vec!["a", "b", "c", "flag", "result", "x", "y"]);
    }

    #[test]
    fn test_free_variables_arithmetic() {
        let spec = SpecCore::new("arithmetic")
            .with_precondition(Term::And(
                Box::new(Term::Gt(
                    Box::new(Term::Add(
                        Box::new(Term::var("x")),
                        Box::new(Term::var("y")),
                    )),
                    Box::new(Term::int(0)),
                )),
                Box::new(Term::Lt(
                    Box::new(Term::Mul(
                        Box::new(Term::var("a")),
                        Box::new(Term::var("b")),
                    )),
                    Box::new(Term::var("max")),
                )),
            ))
            .with_postcondition(Term::Eq(
                Box::new(Term::var("result")),
                Box::new(Term::Div(
                    Box::new(Term::var("sum")),
                    Box::new(Term::var("count")),
                )),
            ));

        let vars = spec.get_free_variables();
        assert_eq!(
            vars,
            vec!["a", "b", "count", "max", "result", "sum", "x", "y"]
        );
    }
}
