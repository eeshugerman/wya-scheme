module PrimativesTests where

import Test.HUnit

import Eval
import Types
import qualified Primatives as Prim

unpackVal :: LispValOrError -> LispVal
unpackVal (Left err) = error $ "unexpected error: " ++ show err
unpackVal (Right val) = val

testFactory :: (Eq a, Show a) => (LispValOrError -> a) -> [(LispVal, a)] -> Test
testFactory unpacker casePairs = TestList $
  [ TestCase $ assertEqual "" expected (unpacker $ eval input)
  | (input, expected) <- casePairs]

-- TODO: maybe just parse scheme for these
-- shorthand
quote val = LispList [LispSymbol "quote", val]
qquote val = LispList [LispSymbol "quasiquote", val]
unquote val = LispList [LispSymbol "unquote", val]
unquoteSplicing val = LispList [LispSymbol "unquote-splicing", val]
lTrue = LispBool True
lFalse = LispBool False
lOne = LispNumber $ LispInteger 1
lTwo = LispNumber $ LispInteger 2
lThree = LispNumber $ LispInteger 3

numericFoldableOpTests = testFactory unpackVal
  [ (LispList [LispSymbol "+", lOne, lOne], lTwo)
  , (LispList [LispSymbol "+", lOne, lOne, lOne], lThree)

  , ( LispList [ LispSymbol "/"
               , lOne
               , LispNumber $ LispComplex (LispInteger 1) (LispRational 3 4)
               ]
    , LispNumber $ LispComplex (LispReal 0.64) (LispReal (-0.48))
    )
  ]

listOpsTests = testFactory unpackVal
  [ (LispList [LispSymbol "car", quote $ LispList [lOne, lTwo]],  lOne)
  , (LispList [LispSymbol "cdr", quote $ LispList [lOne, lTwo]],  LispList [lTwo])
  , (LispList [LispSymbol "cons", lOne, lTwo],   LispDottedList [lOne] lTwo)
  , (LispList [LispSymbol "cons", lOne, quote $ LispList [lTwo]], LispList [lOne, lTwo])
  ]

tests = TestLabel "PRIMATIVE" $ TestList
  [ TestLabel "NUMERIC-FOLDABLE-OPS" numericFoldableOpTests
  , TestLabel "LIST-OPS" listOpsTests

  ]
