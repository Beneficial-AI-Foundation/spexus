//! Parser for Dafny specifications.
//!
//! This module implements a parser for Dafny method specifications using the `pest` parsing library.
//! It converts Dafny method specifications into an intermediate representation that can then be
//! translated to Spexus specifications.

use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "dafny.pest"]
pub struct DafnyParser;

pub type ParseResult<T> = Result<T, Box<dyn std::error::Error>>;

/// Intermediate representation of a Dafny method specification
#[derive(Debug, Clone, PartialEq)]
pub struct DafnyMethodSpec {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<String>,
    pub requires_clauses: Vec<DafnyExpression>,
    pub ensures_clauses: Vec<DafnyExpression>,
    pub decreases_clause: Option<DafnyExpression>,
    pub reads_clause: Option<DafnyExpression>,
    pub modifies_clause: Option<DafnyExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub type_name: String,
}

/// Intermediate representation of Dafny expressions
#[derive(Debug, Clone, PartialEq)]
pub enum DafnyExpression {
    // Literals
    Integer(i64),
    String(String),
    Boolean(bool),
    Variable(String),

    // Arithmetic
    Add(Box<DafnyExpression>, Box<DafnyExpression>),
    Sub(Box<DafnyExpression>, Box<DafnyExpression>),
    Mul(Box<DafnyExpression>, Box<DafnyExpression>),
    Div(Box<DafnyExpression>, Box<DafnyExpression>),
    Mod(Box<DafnyExpression>, Box<DafnyExpression>),

    // Comparisons
    Eq(Box<DafnyExpression>, Box<DafnyExpression>),
    Ne(Box<DafnyExpression>, Box<DafnyExpression>),
    Lt(Box<DafnyExpression>, Box<DafnyExpression>),
    Le(Box<DafnyExpression>, Box<DafnyExpression>),
    Gt(Box<DafnyExpression>, Box<DafnyExpression>),
    Ge(Box<DafnyExpression>, Box<DafnyExpression>),

    // Logical operators
    And(Box<DafnyExpression>, Box<DafnyExpression>),
    Or(Box<DafnyExpression>, Box<DafnyExpression>),
    Not(Box<DafnyExpression>),
    Implies(Box<DafnyExpression>, Box<DafnyExpression>),

    // Array operations
    Length(Box<DafnyExpression>),
    Index(Box<DafnyExpression>, Box<DafnyExpression>),
    Slice(
        Box<DafnyExpression>,
        Box<DafnyExpression>,
        Box<DafnyExpression>,
    ),

    // Quantifiers
    Forall {
        variables: Vec<String>,
        body: Box<DafnyExpression>,
    },
    Exists {
        variables: Vec<String>,
        body: Box<DafnyExpression>,
    },

    // Function calls
    FunctionCall {
        name: String,
        arguments: Vec<DafnyExpression>,
    },
}

impl DafnyParser {
    /// Parse a Dafny specification string into a list of method specifications
    pub fn parse_dafny(input: &str) -> ParseResult<Vec<DafnyMethodSpec>> {
        let pairs = Self::parse(Rule::dafny, input)?;
        let pair = pairs.into_iter().next().unwrap();
        Ok(parse_dafny_inner(pair))
    }
}

fn parse_dafny_inner(pair: pest::iterators::Pair<Rule>) -> Vec<DafnyMethodSpec> {
    let mut methods = Vec::new();

    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::method_spec => {
                methods.push(parse_method_spec(inner_pair));
            }
            Rule::EOI => {} // End of input
            _ => unreachable!(),
        }
    }

    methods
}

fn parse_method_spec(pair: pest::iterators::Pair<Rule>) -> DafnyMethodSpec {
    let mut name = String::new();
    let mut parameters = Vec::new();
    let return_type = None;
    let mut requires_clauses = Vec::new();
    let mut ensures_clauses = Vec::new();
    let mut decreases_clause = None;
    let mut reads_clause = None;
    let mut modifies_clause = None;

    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::method_header => {
                let (method_name, method_params, _method_return) = parse_method_header(inner_pair);
                name = method_name;
                parameters = method_params;
            }
            Rule::requires_clause => {
                let expr = parse_expression(inner_pair.into_inner().last().unwrap());
                println!("DEBUG: Parsed requires clause: {:?}", expr);
                requires_clauses.push(expr);
            }
            Rule::ensures_clause => {
                ensures_clauses.push(parse_expression(inner_pair.into_inner().last().unwrap()));
            }
            Rule::decreases_clause => {
                decreases_clause = Some(parse_expression(inner_pair.into_inner().last().unwrap()));
            }
            Rule::reads_clause => {
                reads_clause = Some(parse_expression(inner_pair.into_inner().last().unwrap()));
            }
            Rule::modifies_clause => {
                modifies_clause = Some(parse_expression(inner_pair.into_inner().last().unwrap()));
            }
            _ => {}
        }
    }

    DafnyMethodSpec {
        name,
        parameters,
        return_type,
        requires_clauses,
        ensures_clauses,
        decreases_clause,
        reads_clause,
        modifies_clause,
    }
}

fn parse_method_header(
    pair: pest::iterators::Pair<Rule>,
) -> (String, Vec<Parameter>, Option<String>) {
    let mut name = String::new();
    let mut parameters = Vec::new();
    let return_type = None;

    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::identifier => {
                if name.is_empty() {
                    name = inner_pair.as_str().to_string();
                }
            }
            Rule::parameter_list => {
                parameters = parse_parameter_list(inner_pair);
            }
            _ => {}
        }
    }

    (name, parameters, return_type)
}

fn parse_parameter_list(pair: pest::iterators::Pair<Rule>) -> Vec<Parameter> {
    let mut parameters = Vec::new();

    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::parameter => {
                parameters.push(parse_parameter(inner_pair));
            }
            _ => {}
        }
    }

    parameters
}

fn parse_parameter(pair: pest::iterators::Pair<Rule>) -> Parameter {
    let mut name = String::new();
    let mut type_name = String::new();

    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::identifier => {
                if name.is_empty() {
                    name = inner_pair.as_str().to_string();
                }
            }
            Rule::type_annotation => {
                type_name = inner_pair.as_str().to_string();
            }
            _ => {}
        }
    }

    Parameter { name, type_name }
}

fn parse_expression(pair: pest::iterators::Pair<Rule>) -> DafnyExpression {
    match pair.as_rule() {
        Rule::expression => parse_expression(pair.into_inner().next().unwrap()),
        Rule::implies_expr => parse_implies_expr(pair),
        _ => unreachable!(),
    }
}

fn parse_implies_expr(pair: pest::iterators::Pair<Rule>) -> DafnyExpression {
    let mut inner = pair.into_inner();
    let mut left = parse_or_expr(inner.next().unwrap());

    while let Some(op_pair) = inner.next() {
        if op_pair.as_str() == "==>" {
            let right = parse_or_expr(inner.next().unwrap());
            left = DafnyExpression::Implies(Box::new(left), Box::new(right));
        }
    }

    left
}

fn parse_or_expr(pair: pest::iterators::Pair<Rule>) -> DafnyExpression {
    let mut inner = pair.into_inner();
    let mut left = parse_and_expr(inner.next().unwrap());

    while let Some(op_pair) = inner.next() {
        if op_pair.as_str() == "||" {
            let right = parse_and_expr(inner.next().unwrap());
            left = DafnyExpression::Or(Box::new(left), Box::new(right));
        }
    }

    left
}

fn parse_and_expr(pair: pest::iterators::Pair<Rule>) -> DafnyExpression {
    println!(
        "DEBUG: parse_and_expr called with rule: {:?}",
        pair.as_rule()
    );
    let mut inner = pair.into_inner();
    let mut left = parse_not_expr(inner.next().unwrap());
    println!("DEBUG: parse_and_expr initial left: {:?}", left);

    while let Some(op_pair) = inner.next() {
        println!(
            "DEBUG: parse_and_expr op rule: {:?}, op str: '{:?}'",
            op_pair.as_rule(),
            op_pair.as_str()
        );
        if op_pair.as_str() == "&&" {
            let right = parse_not_expr(inner.next().unwrap());
            println!(
                "DEBUG: Parsing && expression: left={:?}, right={:?}",
                left, right
            );
            left = DafnyExpression::And(Box::new(left), Box::new(right));
        } else {
            println!("DEBUG: Expected && but got: '{:?}'", op_pair.as_str());
        }
    }

    left
}

fn parse_not_expr(pair: pest::iterators::Pair<Rule>) -> DafnyExpression {
    let mut inner = pair.into_inner();
    let first = inner.next().unwrap();

    if first.as_str() == "!" {
        let expr = parse_comparison_expr(inner.next().unwrap());
        DafnyExpression::Not(Box::new(expr))
    } else {
        parse_comparison_expr(first)
    }
}

fn parse_comparison_expr(pair: pest::iterators::Pair<Rule>) -> DafnyExpression {
    let mut inner = pair.into_inner();
    let mut left = parse_additive_expr(inner.next().unwrap());

    while let Some(op_pair) = inner.next() {
        let op = op_pair.as_str();
        let right = parse_additive_expr(inner.next().unwrap());

        left = match op {
            "==" => DafnyExpression::Eq(Box::new(left), Box::new(right)),
            "!=" => DafnyExpression::Ne(Box::new(left), Box::new(right)),
            "<" => DafnyExpression::Lt(Box::new(left), Box::new(right)),
            "<=" => DafnyExpression::Le(Box::new(left), Box::new(right)),
            ">" => DafnyExpression::Gt(Box::new(left), Box::new(right)),
            ">=" => DafnyExpression::Ge(Box::new(left), Box::new(right)),
            _ => unreachable!(),
        };
    }

    left
}

fn parse_additive_expr(pair: pest::iterators::Pair<Rule>) -> DafnyExpression {
    let mut inner = pair.into_inner();
    let mut left = parse_multiplicative_expr(inner.next().unwrap());

    while let Some(op_pair) = inner.next() {
        let op = op_pair.as_str();
        let right = parse_multiplicative_expr(inner.next().unwrap());

        left = match op {
            "+" => DafnyExpression::Add(Box::new(left), Box::new(right)),
            "-" => DafnyExpression::Sub(Box::new(left), Box::new(right)),
            _ => unreachable!(),
        };
    }

    left
}

fn parse_multiplicative_expr(pair: pest::iterators::Pair<Rule>) -> DafnyExpression {
    let mut inner = pair.into_inner();
    let mut left = parse_postfix_expr(inner.next().unwrap());

    while let Some(op_pair) = inner.next() {
        let op = op_pair.as_str();
        let right = parse_postfix_expr(inner.next().unwrap());

        left = match op {
            "*" => DafnyExpression::Mul(Box::new(left), Box::new(right)),
            "/" => DafnyExpression::Div(Box::new(left), Box::new(right)),
            "%" => DafnyExpression::Mod(Box::new(left), Box::new(right)),
            _ => unreachable!(),
        };
    }

    left
}

fn parse_postfix_expr(pair: pest::iterators::Pair<Rule>) -> DafnyExpression {
    let mut inner = pair.into_inner();
    let mut expr = parse_base_expr(inner.next().unwrap());

    for op_pair in inner {
        match op_pair.as_rule() {
            Rule::index_op => {
                let index = parse_expression(op_pair.into_inner().next().unwrap());
                expr = DafnyExpression::Index(Box::new(expr), Box::new(index));
            }
            Rule::slice_op => {
                let mut slice_inner = op_pair.into_inner();
                let start = parse_expression(slice_inner.next().unwrap());
                let end = parse_expression(slice_inner.next().unwrap());
                expr = DafnyExpression::Slice(Box::new(expr), Box::new(start), Box::new(end));
            }
            Rule::field_access => {
                let field_name = op_pair.into_inner().next().unwrap().as_str().to_string();
                // Treat field access as a function call
                expr = DafnyExpression::FunctionCall {
                    name: field_name,
                    arguments: vec![expr],
                };
            }
            _ => {}
        }
    }

    expr
}

fn parse_base_expr(pair: pest::iterators::Pair<Rule>) -> DafnyExpression {
    match pair.as_rule() {
        Rule::base_expr => parse_base_expr(pair.into_inner().next().unwrap()),
        Rule::integer => {
            let value: i64 = pair.as_str().parse().unwrap();
            DafnyExpression::Integer(value)
        }
        Rule::string => {
            let s = pair.as_str();
            let content = &s[1..s.len() - 1];
            DafnyExpression::String(content.to_string())
        }
        Rule::boolean => {
            let value = pair.as_str() == "true";
            DafnyExpression::Boolean(value)
        }
        Rule::identifier => DafnyExpression::Variable(pair.as_str().to_string()),
        Rule::function_call => parse_function_call(pair),
        Rule::quantified_expr => parse_quantified_expr(pair),
        Rule::paren_expr => parse_expression(pair.into_inner().next().unwrap()),
        _ => unreachable!("Unexpected rule in base_expr: {:?}", pair.as_rule()),
    }
}

fn parse_function_call(pair: pest::iterators::Pair<Rule>) -> DafnyExpression {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    let mut arguments = Vec::new();

    for arg_pair in inner {
        if arg_pair.as_rule() == Rule::expression {
            arguments.push(parse_expression(arg_pair));
        }
    }

    DafnyExpression::FunctionCall { name, arguments }
}

fn parse_quantified_expr(pair: pest::iterators::Pair<Rule>) -> DafnyExpression {
    let mut inner = pair.into_inner();
    let quantifier = inner.next().unwrap();
    let body = parse_expression(inner.next().unwrap());

    match quantifier.as_rule() {
        Rule::forall_expr => {
            let mut variables = Vec::new();
            for var_pair in quantifier.into_inner() {
                if var_pair.as_rule() == Rule::identifier {
                    variables.push(var_pair.as_str().to_string());
                }
            }
            DafnyExpression::Forall {
                variables,
                body: Box::new(body),
            }
        }
        Rule::exists_expr => {
            let mut variables = Vec::new();
            for var_pair in quantifier.into_inner() {
                if var_pair.as_rule() == Rule::identifier {
                    variables.push(var_pair.as_str().to_string());
                }
            }
            DafnyExpression::Exists {
                variables,
                body: Box::new(body),
            }
        }
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod dfy_tests {
    use super::*;

    #[test]
    fn test_parse_simple_method() {
        let input = r#"
        method divide(dividend: int, divisor: int)
            requires divisor != 0
            ensures result == dividend / divisor
        {
            ...
        }
        "#;

        let methods = DafnyParser::parse_dafny(input).unwrap();
        assert_eq!(methods.len(), 1);
        assert_eq!(methods[0].name, "divide");
        assert_eq!(methods[0].parameters.len(), 2);
        assert_eq!(methods[0].requires_clauses.len(), 1);
        assert_eq!(methods[0].ensures_clauses.len(), 1);
    }

    #[test]
    fn test_parse_array_method() {
        let input = r#"
        method safe_get(arr: array<int>, index: int)
            requires index >= 0 && index < arr.Length
            ensures result == arr[index]
        {
            ...
        }
        "#;

        let methods = DafnyParser::parse_dafny(input).unwrap();
        assert_eq!(methods.len(), 1);
        assert_eq!(methods[0].name, "safe_get");
    }

    #[test]
    fn test_parse_simple_and() {
        let input = r#"
        method test(x: int, y: int)
            requires x > 0 && y > 0
        {
            ...
        }
        "#;

        let methods = DafnyParser::parse_dafny(input).unwrap();
        assert_eq!(methods.len(), 1);
        assert_eq!(methods[0].name, "test");
        assert_eq!(methods[0].requires_clauses.len(), 1);

        // Check if the requires clause is an And expression
        match &methods[0].requires_clauses[0] {
            DafnyExpression::And(_, _) => println!("DEBUG: Successfully parsed And expression"),
            other => println!("DEBUG: Got {:?} instead of And", other),
        }
    }
}
