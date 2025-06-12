use spexus_lang::parser::SpexusParser;
use spexus_lang::syntax;
use std::fs;
use std::path::Path;

#[test]
fn test_parse_sample_spexus_file() {
    // Load the test file
    let test_file_path = Path::new("tests/sample.spexus");
    let content = fs::read_to_string(test_file_path)
        .expect("Failed to read test file - make sure tests/sample.spexus exists");

    // Parse the content
    let spexus = SpexusParser::parse_spexus(&content).expect("Failed to parse sample.spexus file");

    // Verify we got the expected number of specs
    assert_eq!(
        spexus.entries.len(),
        5,
        "Expected 5 spec entries in sample.spexus"
    );

    // Verify specific specs exist
    let spec_names: Vec<&str> = spexus.entries.iter().map(|entry| entry.symbol()).collect();

    assert!(spec_names.contains(&"divide"), "Should contain divide spec");
    assert!(
        spec_names.contains(&"safe_get"),
        "Should contain safe_get spec"
    );
    assert!(
        spec_names.contains(&"process_array"),
        "Should contain process_array spec"
    );
    assert!(
        spec_names.contains(&"substring"),
        "Should contain substring spec"
    );
    assert!(
        spec_names.contains(&"filter_positive"),
        "Should contain filter_positive spec"
    );

    // Test specific spec details
    let divide_spec = spexus
        .find_entry("divide")
        .expect("divide spec should exist");
    assert_eq!(
        divide_spec.quantifiers.len(),
        0,
        "divide should have no quantifiers"
    );

    let process_array_spec = spexus
        .find_entry("process_array")
        .expect("process_array spec should exist");
    assert_eq!(
        process_array_spec.quantifiers.len(),
        2,
        "process_array should have 2 quantifiers"
    );

    // Verify quantifiers
    match &process_array_spec.quantifiers[0] {
        syntax::Quantifier::Forall { var, domain } => {
            assert_eq!(var.name, "T");
            assert!(domain.is_none(), "T should be unconstrained");
        }
        _ => panic!("First quantifier should be forall T"),
    }

    match &process_array_spec.quantifiers[1] {
        syntax::Quantifier::Exists { var, domain } => {
            assert_eq!(var.name, "n");
            assert!(domain.is_some(), "n should have domain constraint");
        }
        _ => panic!("Second quantifier should be exists n"),
    }

    println!(
        "✅ Successfully parsed {} spec entries from sample.spexus",
        spexus.entries.len()
    );

    // Print all parsed specs for verification
    for entry in &spexus.entries {
        println!("📋 Spec: {}", entry.symbol());
        if !entry.quantifiers.is_empty() {
            println!("   Quantifiers: {}", entry.quantifiers.len());
        }
    }
}

#[test]
fn test_parse_individual_specs() {
    let test_file_path = Path::new("tests/sample.spexus");
    let content = fs::read_to_string(test_file_path).expect("Failed to read test file");

    let spexus = SpexusParser::parse_spexus(&content).expect("Failed to parse sample.spexus file");

    // Test divide spec in detail
    let divide_spec = spexus.find_entry("divide").unwrap();
    assert_eq!(divide_spec.symbol(), "divide");

    // Test that we can access the precondition and postcondition
    // (We won't test the exact AST structure here, just that parsing succeeded)
    match &divide_spec.core.precondition {
        syntax::Term::Ne(_, _) => {
            // Good, precondition is a not-equal comparison
        }
        _ => panic!("divide precondition should be a != comparison"),
    }

    match &divide_spec.core.postcondition {
        syntax::Term::Eq(_, _) => {
            // Good, postcondition is an equality
        }
        _ => panic!("divide postcondition should be an == comparison"),
    }
}

#[test]
fn test_file_structure_and_comments() {
    let test_file_path = Path::new("tests/sample.spexus");
    let content = fs::read_to_string(test_file_path).expect("Failed to read test file");

    // Verify the file contains comments (they should be ignored by parser)
    assert!(content.contains("// Basic arithmetic function"));
    assert!(content.contains("// Array operations"));

    // Parser should still work despite comments
    let spexus =
        SpexusParser::parse_spexus(&content).expect("Parser should handle comments correctly");

    assert!(
        spexus.entries.len() > 0,
        "Should parse specs even with comments"
    );
}
