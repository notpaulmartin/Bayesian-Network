-- File:          tests.hs
-- Project:       BayesNet implementation
-- File Created:  Wednesday, 17th March 2021 5:30:48 pm
-- Author(s):     Paul Martin
--
-- Last Modified: Wednesday, 17th March 2021 5:30:49 pm
-- Modified By:   Paul Martin

module Tests where

import           BayesNet
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Test.HUnit
import           Test.HUnit.Approx

-- b = PrimBoolRV "Burglary" 0.001
-- e = PrimBoolRV "Earthquake" 0.002

-- a = BoolRV "Alarm" [b, e] aInhibitors
-- aInhibitors :: Inhibitors
-- aInhibitors = Map.fromList [
--         (b, 0.06),  -- P(¬a | b)
--         (e, 0.71)   -- P(¬a | e)
--     ]

-- j = BoolRV "JohnCalls" [a] jInhibitors
-- jInhibitors :: Inhibitors
-- jInhibitors = Map.fromList [
--         (a, 0.1)  -- P(¬j | a)
--     ]

-- m = BoolRV "MaryCalls" [a] mInhibitors
-- mInhibitors :: Inhibitors
-- mInhibitors = Map.fromList [
--         (a, 0.3)  -- P(¬j | a)
--     ]


-- Accepts a margin of error in floats
floatErrorMargin = 0.0001
assertEqual' s = assertApproxEqual s floatErrorMargin

a, b, c :: BoolRV
a = PrimBoolRV "A" 0.9
b = PrimBoolRV "B" 0.5

c = BoolRV "C" [a, b] (Map.fromList [
        (a, 0.3),  -- P(¬c|a) = 0.3
        (b, 0.2)   -- P(¬c|b) = 0.2
    ])


-- C's probability table:
--
--  A B | c    ¬c
--  ––––––––––––––
--  1 1 | .94  .06
--  1 0 | .7   .3
--  0 1 | .8   .2
--  0 0 | 0    1

testPTable :: Test
testPTable = TestList [
        TestCase (assertEqual' "P(c)"     0    (pTable (IS c) [])),
        TestCase (assertEqual' "P(c|a)"   0.7  (pTable (IS c) [a])),
        TestCase (assertEqual' "P(c|a,b)" 0.94 (pTable (IS c) [a, b]))
    ]

test_p_Primitive :: Test
test_p_Primitive = TestList [
        TestCase (assertEqual' "P(a)"  0.9 (p [IS a] [])),
        TestCase (assertEqual' "P(¬a)" 0.1 (p [NOT a] []))
    ]

test_p_And :: Test
test_p_And = TestList [
        TestCase (assertEqual' "P(a,b)"  0.45 (p [IS a, IS b] [])),
        TestCase (assertEqual' "P(¬a,b)" 0.05 (p [NOT a, IS b] [])),
        TestCase (assertEqual' "P(c,a,¬b)"  0.315 (p [IS c, IS a, NOT b] []))
    ]

-- Test automatic insertion of arbitrary RVs: P(c) = P(c, A, B)
test_p_Arb :: Test
test_p_Arb = TestList [
        TestCase (assertEqual' "P(c)"  0.778 (p [IS c] [])),
        TestCase (assertEqual' "P(c,a)"  0.778 (p [IS c] []))
    ]

test_p_Conds :: Test
test_p_Conds = TestList [
        TestCase (assertEqual' "P(c|a,b)"  0.94 (p [IS c] [IS a, IS b]))
    ]

allTests :: Test
allTests = TestList [
        testPTable,
        test_p_Primitive,
        test_p_And,
        test_p_Arb,
        test_p_Conds
    ]
