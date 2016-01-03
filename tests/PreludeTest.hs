import Test.HUnit

import qualified Euterpea as E
import BreveEval (run, Val(..), Trace(TrLoc))
import BrevePrelude

import Text.RawString.QQ

-- Helpers for creating result values
dummyloc = TrLoc (0,0)

note p o d = Vnote (Vp p dummyloc) (Vn o dummyloc) (Vd d dummyloc)
rest d = Vrest (Vd d dummyloc)

-- Helpers for building tests from tables
makeTest (res, prog) = TestLabel prog $ TestCase $
    either (assertFailure . show) (assertEqual ("Evaluating: " ++ show prog) res) (run prog)

-- Test Programs
-- id
idTestProgs =
    [ (Vn 1 dummyloc, "main = id(1)")
    , (Vd 1.0 dummyloc, "main = id(1.0)")
    , (Vp E.D dummyloc, "main = id(D)")
    , (Vb True, "main = id(true)")
    , (Vn (-1) dummyloc, "main = id(-1)")
    , (note E.D 4 (1/4), "main = id((D 4 1/4))")
    ]
idTests = TestList $ map makeTest idTestProgs

tests = TestList [ TestLabel "Prelude: id" idTests ]
