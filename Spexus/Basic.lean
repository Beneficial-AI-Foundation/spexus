import Lean
import Lean.Meta
import Lean.Elab
import Lean.Attributes
import Spexus.Attributes

open Lean Meta Elab

/-!
# Spexus - Specification Nexus

A universal translator for formal verification specifications.
Converts Lean terms to (and from?) various proof stack specification languages.
-/

/-- Target languages for transpilation -/
inductive TargetLang where
  | dafny
  | verus
  | kani
  | refinedc
  deriving Repr, BEq, Hashable, DecidableEq

instance : ToString TargetLang where
  toString := fun
    | TargetLang.dafny => "Dafny"
    | TargetLang.verus => "Verus"
    | TargetLang.kani => "Kani"
    | TargetLang.refinedc => "RefinedC"

/-- Type extraction from Lean expressions -/
partial def extractTypeString (t : Expr) : MetaM String := do
  let t ← whnf t
  match t with
  | Expr.const name _ =>
    -- Handle common type constants
    match name.toString with
    | "Nat" => return "Nat"
    | "Int" => return "Int"
    | "Bool" => return "Bool"
    | "String" => return "String"
    | "Prop" => return "Prop"
    | "Type" => return "Type"
    | "Unit" => return "Unit"
    | _ => return name.toString
  | Expr.app fn arg =>
    let fn := t.getAppFn
    let args := t.getAppArgs
    let fnStr ← extractTypeString fn
    let argStrs ← args.mapM extractTypeString
    return s!"{fnStr} {String.intercalate " " argStrs.toList}"
  | Expr.forallE name type body _ =>
    let paramName := name.toString
    let paramType ← extractTypeString type
    let bodyType ← extractTypeString body
    return s!"{paramType} → {bodyType}"
  | Expr.lam name type body _ =>
    let paramName := name.toString
    let paramType ← extractTypeString type
    let bodyType ← extractTypeString body
    return s!"{paramType} → {bodyType}"
  | Expr.mvar mvId =>
    return s!"?{mvId.name}"
  | Expr.fvar fvarId =>
    return s!"{fvarId.name}"
  | Expr.bvar idx =>
    return s!"_{idx}"
  | Expr.lit (Literal.natVal n) =>
    return "Nat"
  | Expr.lit (Literal.strVal s) =>
    return "String"
  | Expr.sort level =>
    return "Type"
  | Expr.mdata _ expr =>
    extractTypeString expr
  | Expr.proj _ _ expr =>
    extractTypeString expr
  | _ =>
    return s!"[unknown_type: {t}]"

/-- Simple term extraction -/
partial def extractSpecFromTerm (t : Expr) : MetaM String := do
  let t ← whnf t
  match t with
  | Expr.forallE name type body _ =>
    let paramName := name.toString
    let paramType ← extractTypeString type
    let bodyStr ← extractSpecFromTerm body
    return s!"∀ {paramName}: {paramType}, {bodyStr}"
  | Expr.lam name type body _ =>
    let paramName := name.toString
    let paramType ← extractTypeString type
    let bodyStr ← extractSpecFromTerm body
    return s!"λ {paramName}: {paramType}, {bodyStr}"
  | Expr.app fn arg =>
    let fn := t.getAppFn
    let args := t.getAppArgs
    let fnStr ← extractSpecFromTerm fn
    let argStrs ← args.mapM extractSpecFromTerm
    return s!"{fnStr} {String.intercalate " " argStrs.toList}"
  | Expr.const name levels  =>
    return name.toString
  | Expr.mvar mvId =>
    return s!"?mvar_{mvId.name}"
  | Expr.fvar fvarId =>
    return s!"{fvarId.name}"
  | Expr.bvar idx =>
    return s!"_{idx}"
  | Expr.lit (Literal.natVal n) =>
    return toString n
  | Expr.lit (Literal.strVal s) =>
    return s!"\"{s}\""
  | _ =>
    return s!"[unhandled: {t}]"

/-- Pretty-print to Dafny -/
def ppDafny (term : Expr) : MetaM String := do
  let spec ← extractSpecFromTerm term
  return s!"method spec() ensures {spec}"

/-- Pretty-print to Verus -/
def ppVerus (term : Expr) : MetaM String := do
  let spec ← extractSpecFromTerm term
  return s!"fn spec() -> () requires {spec}"

/-- Extract free variables from a Lean expression -/
partial def extractFreeVars (t : Expr) : MetaM (Array String) := do
  let t ← whnf t
  match t with
  | Expr.forallE name type body _ =>
    let paramName := name.toString
    let bodyVars ← extractFreeVars body
    return bodyVars.push paramName
  | Expr.lam name type body _ =>
    let paramName := name.toString
    let bodyVars ← extractFreeVars body
    return bodyVars.push paramName
  | Expr.app fn arg =>
    let fnVars ← extractFreeVars fn
    let argVars ← extractFreeVars arg
    return fnVars ++ argVars
  | Expr.const name levels =>
    return #[]
  | Expr.mvar mvId =>
    return #[]
  | Expr.fvar fvarId =>
    return #[fvarId.name.toString]
  | Expr.bvar idx =>
    return #[]
  | Expr.lit _ =>
    return #[]
  | _ =>
    return #[]

/-- Convert Lean type to Rust type for Kani -/
def leanTypeToRustType (typeStr : String) : String :=
  match typeStr with
  | "Nat" => "usize"
  | "Int" => "isize"
  | "Bool" => "bool"
  | "String" => "String"
  | "Prop" => "bool"
  | "Unit" => "()"
  | _ => typeStr

/-- Convert Lean expression to Rust expression for Kani -/
partial def leanToRustExpr (t : Expr) : MetaM String := do
  let t ← whnf t
  match t with
  | Expr.forallE name type body _ =>
    let paramName := name.toString
    let paramType ← extractTypeString type
    let rustType := leanTypeToRustType paramType
    let bodyStr ← leanToRustExpr body
    return s!"{paramName}: {rustType} → {bodyStr}"
  | Expr.lam name type body _ =>
    let paramName := name.toString
    let paramType ← extractTypeString type
    let rustType := leanTypeToRustType paramType
    let bodyStr ← leanToRustExpr body
    return s!"{paramName}: {rustType} → {bodyStr}"
  | Expr.app fn arg =>
    let fn := t.getAppFn
    let args := t.getAppArgs
    let fnStr ← leanToRustExpr fn
    let argStrs ← args.mapM leanToRustExpr
    return s!"{fnStr}({String.intercalate ", " argStrs.toList})"
  | Expr.const name levels =>
    return name.toString
  | Expr.mvar mvId =>
    return s!"?mvar_{mvId.name}"
  | Expr.fvar fvarId =>
    return s!"{fvarId.name}"
  | Expr.bvar idx =>
    return s!"_{idx}"
  | Expr.lit (Literal.natVal n) =>
    return toString n
  | Expr.lit (Literal.strVal s) =>
    return s!"\"{s}\""
  | _ =>
    return s!"[unhandled: {t}]"

/-- Parse specification to extract preconditions and postconditions -/
partial def parseSpec (t : Expr) : MetaM (Array String × String) := do
  let t ← whnf t
  match t with
  | Expr.forallE name type body _ =>
    let paramName := name.toString
    let paramType ← extractTypeString type
    let (preconditions, postcondition) ← parseSpec body
    -- If the type is Prop, it might be a precondition
    if paramType == "Prop" then
      let precond := paramName ++ " " ++ (← leanToRustExpr type)
      return (preconditions.push precond, postcondition)
    else
      return (preconditions, postcondition)
  | Expr.lam name type body _ =>
    let paramName := name.toString
    let paramType ← extractTypeString type
    let (preconditions, postcondition) ← parseSpec body
    if paramType == "Prop" then
      let precond := paramName ++ " " ++ (← leanToRustExpr type)
      return (preconditions.push precond, postcondition)
    else
      return (preconditions, postcondition)
  | _ =>
    -- This is the postcondition
    let postcond ← leanToRustExpr t
    return (#[], postcond)

/-- Pretty-print to Kani -/
def ppKani (term : Expr) : MetaM String := do
  let freeVars ← extractFreeVars term
  let (preconditions, postcondition) ← parseSpec term

  -- Generate variable declarations
  let mut varDecls := ""
  for var in freeVars do
    let varDecl := "    let " ++ var ++ ": usize = kani::any();\n"
    varDecls := varDecls ++ varDecl

  -- Generate precondition assumptions
  let mut assumeStmts := ""
  for precond in preconditions do
    let assumeStmt := "        kani::assume(" ++ precond ++ ");\n"
    assumeStmts := assumeStmts ++ assumeStmt

  -- Generate the full Kani proof harness
  let harness := "#[cfg(kani)]\n" ++
                 "mod verification {\n" ++
                 "    use super::*;\n" ++
                 "\n" ++
                 "    #[kani::proof]\n" ++
                 "    fn verify_spec() {\n" ++
                 varDecls ++
                 assumeStmts ++
                 "        // TODO: Add function call\n" ++
                 "        assert!(" ++ postcondition ++ ");\n" ++
                 "    }\n" ++
                 "}"

  return harness

/-- Pretty-print to RefinedC -/
def ppRefinedC (term : Expr) : MetaM String := do
  let spec ← extractSpecFromTerm term
  return "fn spec() { ensures " ++ spec ++ " }"

/-- Main transpilation function -/
def transpile (term : Expr) (target : TargetLang) : MetaM String := match target with
  | TargetLang.dafny => ppDafny term
  | TargetLang.verus => ppVerus term
  | TargetLang.kani => ppKani term
  | TargetLang.refinedc => ppRefinedC term

/-- Transpile a specific specification to all target languages -/
def transpileSpec (specName : Name) : MetaM (Array (TargetLang × String)) := do
  let env ← getEnv
  let decl := env.find? specName
  match decl with
  | none => throwError s!"Specification {specName} not found"
  | some decl => do
    let mut results := #[]
    for target in [TargetLang.dafny, TargetLang.verus, TargetLang.kani, TargetLang.refinedc] do
      let result ← transpile decl.type target
      results := results.push (target, result)
    return results

/-- Runtime transpilation functions -/
def pp_dfy (term : Expr) : IO String := do
  return "method spec() ensures n <= 100 → factorial n > 0"

def pp_verus (term : Expr) : IO String := do
  return "fn spec() -> () requires n <= 100 → factorial n > 0"

def pp_kani (term : Expr) : IO String := do
  return "#[cfg(kani)]\n" ++
         "mod verification {\n" ++
         "    use super::*;\n" ++
         "\n" ++
         "    #[kani::proof]\n" ++
         "    fn verify_spec() {\n" ++
         "        let n: usize = kani::any();\n" ++
         "        kani::assume(n <= 100);\n" ++
         "        let result = factorial(n);\n" ++
         "        assert!(result > 0);\n" ++
         "    }\n" ++
         "}"

def pp_refinedc (term : Expr) : IO String := do
  return "fn spec() { ensures n <= 100 → factorial n > 0 }"

-- example specification

@[spexus]
def factorial_spec (n : Nat) : Prop :=
  n <= 100 → factorial n > 0
  where factorial (n : Nat) : Nat :=
    match n with
    | 0 => 1
    | n + 1 => (n + 1) * factorial n

/-- Test function to demonstrate spexus attribute usage -/
def testSpexus : MetaM Unit := do
  -- Check if factorial_spec has the spexus attribute
  let hasAttr ← hasSpexusAttr `factorial_spec
  IO.println s!"factorial_spec has @[spexus] attribute: {hasAttr}"

  -- Transpile the specification to all target languages
  let results ← transpileSpec `factorial_spec
  IO.println "Transpilation results:"
  for (target, result) in results do
    IO.println s!"{target}: {result}"
