# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Spexus is a universal translator for formal verification specifications that converts Lean 4 terms to and from various proof stack specification languages. It serves as a "specification nexus" providing a common lingua franca for formal verification tools including Dafny, Verus, Kani (Rust model checker), and RefinedC.

The core idea is to implement _transpilers_ (to dafny, verus, etc.) as _pretty printers_.

## Technology Stack

- **Primary Language**: Lean 4 (functional programming language and theorem prover)
- **Lean Version**: `leanprover/lean4-nightly:nightly-2025-06-30`
- **Build System**: Lake (Lean's package manager)
- **Development Environment**: Nix flakes with development shells
- **Target Languages**: Dafny, Verus, Kani, RefinedC

## Build Commands

```bash
# Build the entire project
lake build

# Build the library only
lake build Spexus

# Run the main demonstration
lake exe spexus

# Run the test harness
lake exe test

# Clean build artifacts
lake clean
```

## Development Environment

This project uses Nix flakes for reproducible development environments:

```bash
# Enter development shell
nix develop

# Format code
nix fmt
```

The development environment includes:
- `elan` (Lean installer)
- `dafny` (for Dafny integration)
- `cargo` (for Rust/Kani integration)
- `claude-code` (this tool)

## Core Architecture

### Main Components

1. **`Spexus/Basic.lean`** (334 lines): Core transpilation logic
   - `TargetLang` enumeration: Dafny, Verus, Kani, RefinedC
   - `extractSpecFromTerm`: Analyzes Lean expressions
   - Pretty-printers for each target language
   - Runtime API functions: `pp_dfy`, `pp_verus`, `pp_kani`, `pp_refinedc`

2. **`Spexus/Attributes.lean`** (16 lines): Spexus attribute system
   - `@[spexus]` attribute registration
   - `hasSpexusAttr` lookup function

3. **`Main.lean`** (57 lines): Example usage and demonstrations

4. **`Test.lean`** (19 lines): Test cases for Kani transpilation

### Specification Patterns

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
-- intended use: 
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

### Target Language Mappings

- **Dafny**: Uses `requires`/`ensures` clauses
- **Verus**: Uses `requires` clauses in function specifications
- **Kani**: Generates `#[kani::proof]` harnesses with `kani::assume`/`assert!`
- **RefinedC**: Uses `ensures` clauses

## Cursor Rules Integration

The project includes specific rules for target languages:

### Kani Translation Rules
- Map `prec` (preconditions) to `kani::assume`
- Map `post` (postconditions) to `assert!`
- Generate proof harnesses in `#[cfg(kani)] mod verification` blocks
- Use `#[kani::proof]` attribute for verification functions

### Dafny Integration
- Support for `requires`, `ensures`, `decreases` clauses
- Proper handling of specification clauses and frame conditions
- Integration with Dafny's verification model

## Project Structure

```
spexus/
├── Spexus.lean              # Main library entry point
├── Spexus/
│   ├── Basic.lean           # Core transpilation logic
│   └── Attributes.lean      # Attribute system
├── Main.lean                # Example demonstrations
├── Test.lean                # Test harness
├── lakefile.toml            # Lake build configuration
├── lean-toolchain           # Lean version specification
├── flake.nix               # Nix development environment
└── .cursor/rules/          # Language-specific translation rules
    ├── spexus.mdc          # Project overview
    ├── kani.mdc            # Kani translation patterns
    ├── kani-docs.mdc       # Kani documentation
    └── dafny-docs.mdc      # Dafny specification syntax
```

## Development Workflow

1. **Adding New Specifications**: Mark definitions with `@[spexus]` attribute
2. **Testing Transpilation**: Use runtime functions `pp_dfy`, `pp_verus`, etc.
3. **Extending Target Languages**: Add new cases to `TargetLang` enum and corresponding pretty-printers
4. **Term Analysis**: Enhance `extractSpecFromTerm` for complex expressions

## Current Implementation Status

**Implemented**:
- Basic transpilation infrastructure for all target languages
- Runtime transpilation functions
- Example specifications and demonstrations
- Modular architecture ready for expansion

**Development Priorities**:
1. Enhanced term analysis in `extractSpecFromTerm`
2. Complete `@[spexus]` attribute workflow integration
3. Improved pretty-printer support for complex specifications
4. Bidirectional translation capabilities
5. LLM integration for natural language to Spexus translation

## Key Design Principles

- **Language Agnostic**: Lean 4 as the universal specification language
- **Modular Architecture**: Easy to extend for new target languages
- **Principled Translation**: Deductive methods for semantic preservation
- **Research Focus**: Exploring formal verification language translation
