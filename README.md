# Spexus - Specification Nexus

A universal translator for formal verification specifications that converts Lean terms to and from various proof stack specification languages.

## Vision

Spexus aims to be a common lingua franca for shipping specifications to formal verification proof stacks like Verus, Dafny, Kani, and RefinedC. The goal is to:

1. **Translate natural language to Spexus** with an LLM
2. **Translate Spexus to each proof stack's specification syntax** with principled deductive methods
3. **Enable bidirectional translation** between different specification languages

## Current Implementation

The current implementation provides:

- **Basic transpilation infrastructure** for Dafny, Verus, Kani, and RefinedC
- **Runtime transpilation functions**: `pp_dfy`, `pp_verus`, `pp_kani`, `pp_refinedc`
- **Example specifications** demonstrating the concept
- **Modular architecture** ready for expansion

## Usage (aspirational)

If we take the following spec with the `spexus` attribute: 
```lean
@[spexus]
def factorial_spec (factorial: Nat → Nat) : Prop :=
  forall n, factorial n = factorial_rec n
  where factorial_rec (n : Nat) : Nat :=
    match n with
    | 0 => 1
    | n + 1 => (n + 1) * factorial_rec n
```
We would like to be able to **transpile** to dafny, verus, etc. 

``` lean
#eval pp_dfy factorial_spec
```
Should result in 
```dafny
module FactorialSpec {

  // The “reference” factorial, defined by recursion
  function factorial_rec(n: nat): nat
    decreases n
  {
    if n == 0 then
      1
    else
      n * factorial_rec(n - 1)
  }

  // A predicate saying that any implementation `f` equals the reference
  predicate factorial_spec(f: nat -> nat)
  {
    forall n: nat :: f(n) == factorial_rec(n)
  }

}
```

``` lean
#eval pp_verus factorial_spec
#eval pp_kani factorial_spec
#eval pp_refinedc factorial_spec
```

## Architecture

### Core Components

1. **TargetLang**: Enumeration of supported proof stacks
2. **extractSpecFromTerm**: Analyzes Lean terms to extract specification structure
3. **Pretty-printers**: Convert specifications to target language syntax
4. **Runtime functions**: Provide the public API for transpilation

### File Structure

```
spexus/
├── Spexus.lean          # Main library entry point
├── Spexus/Basic.lean    # Core transpilation logic
├── Main.lean            # Example usage and demonstration
└── lakefile.toml        # Build configuration
```

## Next Steps

### Immediate Priorities

1. **Implement proper term analysis** in `extractSpecFromTerm`
   - Parse Lean expressions to extract pre/post conditions
   - Handle different specification patterns
   - Support complex type structures

2. **Add #[spexus] attribute registration**
   - Create proper attribute handler
   - Register marked terms for transpilation
   - Provide lookup mechanism

3. **Enhance pretty-printers**
   - Support more complex specifications
   - Handle language-specific features
   - Improve output formatting

### Future Enhancements

4. **Bidirectional translation**
   - Parse target language specifications back to Lean
   - Maintain semantic equivalence
   - Handle language-specific constructs

5. **LLM integration**
   - Natural language to Spexus translation
   - Specification generation from descriptions
   - Interactive refinement

6. **Extended language support**
   - Add more proof stacks
   - Support for domain-specific languages
   - Custom transpilation rules

## Building and Running

```bash
# Build the project
lake build

# Run the example
lake exe spexus

# Build specific target
lake build Spexus
```

## Contributing

This is a research project exploring the intersection of formal verification and language translation. Contributions are welcome, especially:

- Implementation of term analysis algorithms
- Support for additional proof stacks
- Improvements to the transpilation logic
- Documentation and examples

## License

[Add appropriate license information]
