import Spexus
import Lean
import Lean.Meta
open Lean Meta
-- Test the Kani pretty printer with a simple specification
def testKaniPrinter : MetaM Unit := do
  -- Test with the factorial specification
  let results ← transpileSpec `factorial_spec

  IO.println "=== Kani Transpilation Test ==="
  for (target, result) in results do
    if target == TargetLang.kani then
      IO.println "Kani output:"
      IO.println result
      IO.println ""

-- Run the test
#eval testKaniPrinter
