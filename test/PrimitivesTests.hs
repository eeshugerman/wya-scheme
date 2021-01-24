module PrimitivesTests where

import Test.HUnit

import Eval
import Types
import qualified Primitives as Prim

testFactory
  :: [(String, [SchemeVal], SchemeVal)]    -- procName, args, expected
  -> Test
testFactory casePairs = TestList
  [ case lookup procName Prim.primitives of
      Nothing -> error $ "unknown primative: " ++ procName
      Just primProc ->
        case primProc args of
          Left err -> error "unexpected exception: "err
          Right actual -> TestCase $ assertEqual "" expected actual
  | (procName, args, expected) <- casePairs ]

-- shorthand
lOne = SNumber $ SInteger 1
lTwo = SNumber $ SInteger 2
lThree = SNumber $ SInteger 3


numericFoldableOpTests = testFactory
  [ ("+", [lOne, lOne], lTwo)
  , ("+", [lOne, lOne, lOne], lThree)

  , ( "/"
    , [ lOne , SNumber $ SComplex (SInteger 1) (SRational 3 4)]
    , SNumber $ SComplex (SReal 0.64) (SReal (-0.48))
    )
  ]

listOpsTests = testFactory
  [ ("car",  [SList [lOne, lTwo]],  lOne)
  , ("cdr",  [SList [lOne, lTwo]],  SList [lTwo])
  , ("cons", [lOne, lTwo],                     SDottedList [lOne] lTwo)
  , ("cons", [lOne, SList [lTwo]],  SList [lOne, lTwo])
  ]

tests = TestLabel "PRIMATIVE" $ TestList
  [ TestLabel "NUMERIC-FOLDABLE-OPS" numericFoldableOpTests
  , TestLabel "LIST-OPS" listOpsTests

  ]
