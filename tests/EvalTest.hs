import Test.HUnit

import BreveEval
import qualified Euterpea as E

-- BreveEval Tests
-- Mostly for eval/evalExpr

dummyLoc = TrLoc (0,0)

makeTestList :: String -> [(String, BreveErrorM Val)] -> Test
makeTestList name tests = TestLabel name $ TestList $ map (\(p,e) -> run p ~=? e) tests

-- Basic Sanity Tests

basicTestsData =
    [ ("main = 5 + 5", Right $ Vn 10 dummyLoc)
    , ("main = 1.0 + 2.0", Right $ Vd 3.0 dummyLoc)
    , ("main = 5 < 10", Right $ Vb True)
    , ("main = (D 4 1/4)", Right $ Vnote (Vp E.D dummyLoc) (Vn 4 dummyLoc) (Vd 0.25 dummyLoc))
    , ("main = (rest 4)", Right $ Vrest (Vd 4.0 dummyLoc))
    ]

basicTests = makeTestList "Basic Sanity Tests" basicTestsData

main = runTestTT basicTests >>= print
