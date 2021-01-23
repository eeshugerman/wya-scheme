module PrimitivesTests where

import Test.HUnit

import Eval
import Types
import qualified Primitives as Prim

testFactory
  :: [(String, [LispVal], LispVal)]    -- procName, args, expected
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
lOne = LispNumber $ LispInteger 1
lTwo = LispNumber $ LispInteger 2
lThree = LispNumber $ LispInteger 3


numericFoldableOpTests = testFactory
  [ ("+", [lOne, lOne], lTwo)
  , ("+", [lOne, lOne, lOne], lThree)

  , ( "/"
    , [ lOne , LispNumber $ LispComplex (LispInteger 1) (LispRational 3 4)]
    , LispNumber $ LispComplex (LispReal 0.64) (LispReal (-0.48))
    )
  ]

listOpsTests = testFactory
  [ ("car",  [LispList [lOne, lTwo]],  lOne)
  , ("cdr",  [LispList [lOne, lTwo]],  LispList [lTwo])
  , ("cons", [lOne, lTwo],                     LispDottedList [lOne] lTwo)
  , ("cons", [lOne, LispList [lTwo]],  LispList [lOne, lTwo])
  ]

tests = TestLabel "PRIMATIVE" $ TestList
  [ TestLabel "NUMERIC-FOLDABLE-OPS" numericFoldableOpTests
  , TestLabel "LIST-OPS" listOpsTests

  ]
