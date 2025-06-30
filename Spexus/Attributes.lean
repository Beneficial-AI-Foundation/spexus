import Lean
import Lean.Meta
import Lean.Elab
import Lean.Attributes

open Lean Meta Elab

/-- Register the spexus attribute -/
initialize spexusAttr : TagAttribute ←
  registerTagAttribute `spexus "Mark a definition as a specification for transpilation"

/-- Check if a declaration has the spexus attribute -/
def hasSpexusAttr (declName : Name) : MetaM Bool := do
  let env ← getEnv
  return spexusAttr.hasTag env declName
