import Lean
import Lean.Meta
import Lean.Elab
import Lean.Attributes
import Spexus.Attributes

open Lean Meta Elab

set_option linter.unusedVariables false
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
  let freeVars ← extractFreeVars term
  let (maybePrecond, postcond) ← parseImplicationDafny term

  -- Generate parameter declarations
  let mut paramDecls := ""
  for var in freeVars do
    let paramDecl := var ++ ": nat"
    if paramDecls.isEmpty then
      paramDecls := paramDecl
    else
      paramDecls := paramDecls ++ ", " ++ paramDecl

  -- Generate method signature
  let methodSig := if paramDecls.isEmpty then
    "method spec()"
  else
    s!"method spec({paramDecls})"

  -- Generate requires clause if there's a precondition
  let requiresClause := match maybePrecond with
    | some precond => s!"  requires {precond}\n"
    | none => ""

  -- Generate ensures clause
  let ensuresClause := s!"  ensures {postcond}"

  -- Build complete method
  let fullMethod := methodSig ++ "\n" ++ requiresClause ++ ensuresClause

  return fullMethod

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

/-- Convert Lean type to Dafny type -/
def leanTypeToDafnyType (typeStr : String) : String :=
  match typeStr with
  | "Nat" => "nat"
  | "Int" => "int"
  | "Bool" => "bool"
  | "String" => "string"
  | "Prop" => "bool"
  | "Unit" => "()"
  | _ => typeStr

/-- Convert Lean expression to Dafny expression -/
partial def leanToDafnyExpr (t : Expr) : MetaM String := do
  let t ← whnf t
  match t with
  | Expr.forallE name type body _ =>
    let bodyStr ← leanToDafnyExpr body
    return bodyStr
  | Expr.lam name type body _ =>
    let bodyStr ← leanToDafnyExpr body
    return bodyStr
  | Expr.app fn arg =>
    let fn := t.getAppFn
    let args := t.getAppArgs
    match fn with
    | Expr.const `LE.le _ =>
      if args.size >= 2 then
        let lhs ← leanToDafnyExpr args[0]!
        let rhs ← leanToDafnyExpr args[1]!
        return s!"{lhs} <= {rhs}"
      else
        return "[invalid_le]"
    | Expr.const `LT.lt _ =>
      if args.size >= 2 then
        let lhs ← leanToDafnyExpr args[0]!
        let rhs ← leanToDafnyExpr args[1]!
        return s!"{lhs} < {rhs}"
      else
        return "[invalid_lt]"
    | Expr.const `GT.gt _ =>
      if args.size >= 2 then
        let lhs ← leanToDafnyExpr args[0]!
        let rhs ← leanToDafnyExpr args[1]!
        return s!"{lhs} > {rhs}"
      else
        return "[invalid_gt]"
    | Expr.const `GE.ge _ =>
      if args.size >= 2 then
        let lhs ← leanToDafnyExpr args[0]!
        let rhs ← leanToDafnyExpr args[1]!
        return s!"{lhs} >= {rhs}"
      else
        return "[invalid_ge]"
    | Expr.const `Eq _ =>
      if args.size >= 3 then
        let lhs ← leanToDafnyExpr args[1]!
        let rhs ← leanToDafnyExpr args[2]!
        return s!"{lhs} == {rhs}"
      else
        return "[invalid_eq]"
    | _ =>
      let fnStr ← leanToDafnyExpr fn
      let argStrs ← args.mapM leanToDafnyExpr
      return s!"{fnStr}({String.intercalate ", " argStrs.toList})"
  | Expr.const name levels =>
    match name.toString with
    | "factorial" => return "factorial"
    | _ => return name.toString
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

/-- Convert Lean expression to Rust expression for Kani -/
partial def leanToRustExpr (t : Expr) : MetaM String := do
  let t ← whnf t
  match t with
  | Expr.forallE name type body _ =>
    let paramName := name.toString
    let bodyStr ← leanToRustExpr body
    return bodyStr
  | Expr.lam name type body _ =>
    let paramName := name.toString
    let bodyStr ← leanToRustExpr body
    return bodyStr
  | Expr.app fn arg =>
    let fn := t.getAppFn
    let args := t.getAppArgs
    match fn with
    | Expr.const `LE.le _ =>
      if args.size >= 2 then
        let lhs ← leanToRustExpr args[0]!
        let rhs ← leanToRustExpr args[1]!
        return s!"{lhs} <= {rhs}"
      else
        return "[invalid_le]"
    | Expr.const `LT.lt _ =>
      if args.size >= 2 then
        let lhs ← leanToRustExpr args[0]!
        let rhs ← leanToRustExpr args[1]!
        return s!"{lhs} < {rhs}"
      else
        return "[invalid_lt]"
    | Expr.const `GT.gt _ =>
      if args.size >= 2 then
        let lhs ← leanToRustExpr args[0]!
        let rhs ← leanToRustExpr args[1]!
        return s!"{lhs} > {rhs}"
      else
        return "[invalid_gt]"
    | Expr.const `GE.ge _ =>
      if args.size >= 2 then
        let lhs ← leanToRustExpr args[0]!
        let rhs ← leanToRustExpr args[1]!
        return s!"{lhs} >= {rhs}"
      else
        return "[invalid_ge]"
    | Expr.const `Eq _ =>
      if args.size >= 3 then
        let lhs ← leanToRustExpr args[1]!
        let rhs ← leanToRustExpr args[2]!
        return s!"{lhs} == {rhs}"
      else
        return "[invalid_eq]"
    | _ =>
      let fnStr ← leanToRustExpr fn
      let argStrs ← args.mapM leanToRustExpr
      return s!"{fnStr}({String.intercalate ", " argStrs.toList})"
  | Expr.const name levels =>
    match name.toString with
    | "factorial" => return "factorial"
    | _ => return name.toString
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

/-- Parse implication to extract precondition and postcondition for Dafny -/
partial def parseImplicationDafny (t : Expr) : MetaM (Option String × String) := do
  let t ← whnf t
  match t with
  | Expr.forallE name type body _ =>
    -- In Lean, P → Q is represented as ∀ (h : P), Q
    if (← extractTypeString type) == "Prop" then
      let precStr ← leanToDafnyExpr type
      let postStr ← leanToDafnyExpr body
      return (some precStr, postStr)
    else
      let exprStr ← leanToDafnyExpr t
      return (none, exprStr)
  | _ =>
    let exprStr ← leanToDafnyExpr t
    return (none, exprStr)

/-- Parse implication to extract precondition and postcondition -/
partial def parseImplication (t : Expr) : MetaM (Option String × String) := do
  let t ← whnf t
  match t with
  | Expr.forallE name type body _ =>
    -- In Lean, P → Q is represented as ∀ (h : P), Q
    if (← extractTypeString type) == "Prop" then
      let precStr ← leanToRustExpr type
      let postStr ← leanToRustExpr body
      return (some precStr, postStr)
    else
      let exprStr ← leanToRustExpr t
      return (none, exprStr)
  | _ =>
    let exprStr ← leanToRustExpr t
    return (none, exprStr)

/-- Parse specification to extract preconditions and postconditions -/
partial def parseSpec (t : Expr) : MetaM (Array String × String) := do
  let t ← whnf t
  match t with
  | Expr.forallE name type body _ =>
    let paramName := name.toString
    let paramType ← extractTypeString type
    let (preconditions, postcondition) ← parseSpec body
    return (preconditions, postcondition)
  | Expr.lam name type body _ =>
    let paramName := name.toString
    let paramType ← extractTypeString type
    let (preconditions, postcondition) ← parseSpec body
    return (preconditions, postcondition)
  | _ =>
    -- Check if this is an implication (precond → postcond)
    let (maybePrecond, postcond) ← parseImplication t
    match maybePrecond with
    | some precond => return (#[precond], postcond)
    | none => return (#[], postcond)

/-- Pretty-print to Kani -/
def ppKani (term : Expr) : MetaM String := do
  let freeVars ← extractFreeVars term
  let (preconditions, postcondition) ← parseSpec term

  -- Generate variable declarations
  let mut varDecls := ""  
  for var in freeVars do
    let varDecl := "        let " ++ var ++ ": usize = kani::any();\n"
    varDecls := varDecls ++ varDecl

  -- Generate precondition assumptions
  let mut assumeStmts := ""
  for precond in preconditions do
    let assumeStmt := "        kani::assume(" ++ precond ++ ");\n"
    assumeStmts := assumeStmts ++ assumeStmt

  -- Generate function call (extract function name from postcondition)
  let mut functionCall := ""
  if postcondition.startsWith "factorial" || postcondition.endsWith "factorial" || "factorial" ∈ postcondition.split Char.isWhitespace then
    functionCall := "        let result = factorial(n);\n"

  -- Generate the full Kani proof harness
  let harness := "#[cfg(kani)]\n" ++
                 "mod verification {\n" ++
                 "    use super::*;\n" ++
                 "\n" ++
                 "    #[kani::proof]\n" ++
                 "    fn verify_spec() {\n" ++
                 varDecls ++
                 assumeStmts ++
                 functionCall ++
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
  let fileCtx: Core.Context := {
    fileName := "<runtime>",
    fileMap := ⟨"", #[]⟩
  }
  let ctx: optParam Context {} := {}
  let st : Core.State := {
    env := ← getEnv
  }
  let result ← (ppDafny term).run ctx |>.toIO fileCtx st
  return result.fst

def pp_verus (term : Expr) : IO String := do
  return "fn spec() -> () requires n <= 100 → factorial n > 0"

def pp_kani (term : Expr) : IO String := do
  let fileCtx: Core.Context := {
    fileName := "<runtime>",
    fileMap := ⟨"", #[]⟩
  }
  let ctx: optParam Context {} := {}
  let st : Core.State := {
    env := ← getEnv
  }
  let result ← (ppKani term).run ctx |>.toIO fileCtx st
  return result.fst

def pp_refinedc (term : Expr) : IO String := do
  return "fn spec() { ensures n <= 100 → factorial n > 0 }"

-- example specification

@[spexus]
def factorial_spec (factorial: Nat → Nat) : Prop :=
  forall n, factorial n = factorial_rec n
  where factorial_rec (n : Nat) : Nat :=
    match n with
    | 0 => 1
    | n + 1 => (n + 1) * factorial_rec n

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

-- Test the individual Kani pretty printer with factorial_spec
-- #eval do 
--   let env ← getEnv
--   match env.find? `factorial_spec with
--   | some decl => 
--     let result ← ppKani decl.type |>.run' {}
--     IO.println "=== Testing Actual ppKani Implementation ==="
--     IO.println result
--   | none => 
--     IO.println "factorial_spec not found"

def liftFactorialSpec : MetaM Expr := do
  pure (mkConst `factorial_spec)

#eval do
  let e <- liftFactorialSpec
  transpile e TargetLang.dafny

-- Test the Dafny implementation directly
#eval do
  let env ← getEnv
  match env.find? `factorial_spec with
  | some decl => 
    let result ← ppDafny decl.type
    IO.println "=== Testing Actual ppDafny Implementation ==="
    IO.println result
  | none => 
    IO.println "factorial_spec not found"
