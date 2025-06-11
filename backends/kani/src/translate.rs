use lang::syntax::{Atom, SpecCore, Term, Value, Variable};

fn add_module_header(code: &mut String) {
    code.push_str("#[cfg(kani)]\n");
    code.push_str("mod verification {\n");
    code.push_str("    use super::*;\n\n");
}

fn add_proof_function_header(code: &mut String, symbol: &str) {
    code.push_str("    #[kani::proof]\n");
    code.push_str(&format!("    fn verify_{}() {{\n", symbol));
}

/// Helper function to determine variable type
fn get_var_type(var: &str, spec: &SpecCore) -> &'static str {
    // Check if variable is used in array operations
    let is_array = |term: &Term| -> bool {
        match term {
            Term::Len(t) => t.as_variable().map(|v| v.name() == var).unwrap_or(false),
            Term::Index(arr, _) => arr.as_variable().map(|v| v.name() == var).unwrap_or(false),
            Term::Slice(arr, _, _) => arr.as_variable().map(|v| v.name() == var).unwrap_or(false),
            Term::In(_, arr) => arr.as_variable().map(|v| v.name() == var).unwrap_or(false),
            _ => false,
        }
    };

    // Check if variable is used in string operations
    let is_string = |term: &Term| -> bool {
        if let Term::Value(Value::Atom(Atom::String(_))) = term {
            true
        } else {
            false
        }
    };

    // Check if variable is used in boolean operations
    let is_bool = |term: &Term| -> bool {
        if let Term::Value(Value::Atom(Atom::Bool(_))) = term {
            true
        } else {
            false
        }
    };

    // Check all terms in the specification
    if is_array(spec.precondition()) || is_array(spec.postcondition()) {
        "Vec<usize>"
    } else if is_string(spec.precondition()) || is_string(spec.postcondition()) {
        "String"
    } else if is_bool(spec.precondition()) || is_bool(spec.postcondition()) {
        "bool"
    } else {
        "usize"
    }
}
fn add_variable_declarations(code: &mut String, spec: &SpecCore) {
    let free_vars = spec.get_free_variables();
    let args: Vec<_> = free_vars
        .iter()
        .filter(|var| *var != "result")
        .cloned()
        .collect();

    for var in &args {
        let var_type = get_var_type(var, spec);
        code.push_str(&format!(
            "        let {}: {} = kani::any();\n",
            var, var_type
        ));
    }
}

fn add_precondition(code: &mut String, spec: &SpecCore) {
    if spec.precondition() != &Term::tt() {
        code.push_str("        kani::assume");
        code.push_str(&translate_term(spec.precondition()));
        code.push_str(";\n");
    }
}

fn add_function_call(code: &mut String, spec: &SpecCore) {
    let args: Vec<_> = spec
        .get_free_variables()
        .iter()
        .filter(|var| *var != "result")
        .cloned()
        .collect();

    code.push_str(&format!(
        "        let result = {}({});\n",
        spec.symbol(),
        args.join(", ")
    ));
}

fn add_postcondition(code: &mut String, spec: &SpecCore) {
    if spec.postcondition() != &Term::tt() {
        code.push_str("        assert!");
        code.push_str(&translate_term(spec.postcondition()));
        code.push_str(";\n");
    }
}

fn add_module_footer(code: &mut String) {
    code.push_str("    }\n");
    code.push_str("}\n");
}

/// Translates a Spexus value into Rust code
fn translate_value(value: &lang::syntax::Value) -> String {
    match value {
        lang::syntax::Value::Atom(a) => match a {
            lang::syntax::Atom::Integer(n) => n.to_string(),
            lang::syntax::Atom::String(s) => format!("\"{}\"", s),
            lang::syntax::Atom::Bool(b) => b.to_string(),
        },
        lang::syntax::Value::List(l) => {
            let elements: Vec<String> = l
                .iter()
                .map(|a| match a {
                    lang::syntax::Atom::Integer(n) => n.to_string(),
                    lang::syntax::Atom::String(s) => format!("\"{}\"", s),
                    lang::syntax::Atom::Bool(b) => b.to_string(),
                })
                .collect();
            format!("vec![{}]", elements.join(", "))
        }
    }
}
/// Translates a Spexus term into Rust code
fn translate_term(term: &Term) -> String {
    match term {
        Term::Value(v) => translate_value(v),
        Term::Variable(Variable { name }) => name.clone(),
        Term::Len(t) => format!("{}.len()", translate_term(t)),
        Term::Index(arr, idx) => format!("{}[{}]", translate_term(arr), translate_term(idx)),
        Term::Slice(arr, start, end) => format!(
            "{}[{}..{}]",
            translate_term(arr),
            translate_term(start),
            translate_term(end)
        ),
        Term::In(x, arr) => format!("{}.contains(&{})", translate_term(arr), translate_term(x)),
        Term::Add(l, r) => format!("({} + {})", translate_term(l), translate_term(r)),
        Term::Sub(l, r) => format!("({} - {})", translate_term(l), translate_term(r)),
        Term::Mul(l, r) => format!("({} * {})", translate_term(l), translate_term(r)),
        Term::Div(l, r) => format!("({} / {})", translate_term(l), translate_term(r)),
        Term::Mod(l, r) => format!("({} % {})", translate_term(l), translate_term(r)),
        Term::Eq(l, r) => format!("({} == {})", translate_term(l), translate_term(r)),
        Term::Ne(l, r) => format!("({} != {})", translate_term(l), translate_term(r)),
        Term::Lt(l, r) => format!("({} < {})", translate_term(l), translate_term(r)),
        Term::Le(l, r) => format!("({} <= {})", translate_term(l), translate_term(r)),
        Term::Gt(l, r) => format!("({} > {})", translate_term(l), translate_term(r)),
        Term::Ge(l, r) => format!("({} >= {})", translate_term(l), translate_term(r)),
        Term::And(l, r) => format!("({} && {})", translate_term(l), translate_term(r)),
        Term::Or(l, r) => format!("({} || {})", translate_term(l), translate_term(r)),
        Term::Not(t) => format!("(!{})", translate_term(t)),
        Term::Implies(l, r) => format!("(!{} || {})", translate_term(l), translate_term(r)),
        Term::Iff(l, r) => format!("({} == {})", translate_term(l), translate_term(r)),
        Term::Forall { var, domain, body } => {
            format!(
                "{}.iter().all(|{}| {})",
                translate_term(domain),
                var.name,
                translate_term(body)
            )
        }
        Term::Exists { var, domain, body } => {
            format!(
                "{}.iter().any(|{}| {})",
                translate_term(domain),
                var.name,
                translate_term(body)
            )
        }
        Term::ForallRange {
            var,
            start,
            end,
            body,
        } => {
            format!(
                "({}..{}).all(|{}| {})",
                translate_term(start),
                translate_term(end),
                var.name,
                translate_term(body)
            )
        }
        Term::ExistsRange {
            var,
            start,
            end,
            body,
        } => {
            format!(
                "({}..{}).any(|{}| {})",
                translate_term(start),
                translate_term(end),
                var.name,
                translate_term(body)
            )
        }
    }
}

/// Translates a Spexus specification into a Kani proof harness
pub fn translate_to_kani(spec: &SpecCore) -> String {
    let mut code = String::new();
    add_module_header(&mut code);
    add_proof_function_header(&mut code, spec.symbol());
    add_variable_declarations(&mut code, spec);
    add_precondition(&mut code, spec);
    add_function_call(&mut code, spec);
    add_postcondition(&mut code, spec);
    add_module_footer(&mut code);
    code
}

/// Tests for the translate_to_kani function
// I'm uncertain how brittle these tests are, but they're a good start.
#[cfg(test)]
mod tests {
    use super::*;

    fn normalize_whitespace(s: &str) -> String {
        s.lines()
            .map(|line| line.trim_end())
            .collect::<Vec<_>>()
            .join("\n")
    }

    #[test]
    fn test_translate_divide_spec() {
        let spec = SpecCore::new("divide")
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

        let expected = r#"#[cfg(kani)]
mod verification {
    use super::*;

    #[kani::proof]
    fn verify_divide() {
        let dividend: usize = kani::any();
        let divisor: usize = kani::any();
        kani::assume(divisor != 0);
        let result = divide(dividend, divisor);
        assert!(result == (dividend / divisor));
    }
}
"#;

        assert_eq!(
            normalize_whitespace(&translate_to_kani(&spec)),
            normalize_whitespace(expected)
        );
    }

    #[test]
    fn test_translate_array_spec() {
        let spec = SpecCore::new("safe_get")
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
            ));

        let expected = r#"#[cfg(kani)]
mod verification {
    use super::*;

    #[kani::proof]
    fn verify_safe_get() {
        let arr: usize = kani::any();
        let index: usize = kani::any();
        kani::assume((index >= 0) && (index < arr.len()));
        let result = safe_get(arr, index);
        assert!(result == arr[index]);
    }
}
"#;

        assert_eq!(
            normalize_whitespace(&translate_to_kani(&spec)),
            normalize_whitespace(expected)
        );
    }
}
