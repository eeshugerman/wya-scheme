module EvalTests where

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

atomicTests = testFactory unpackVal
  [ (lTrue,                                 lTrue)
  , (LispCharacter 'a',                     LispCharacter 'a')
  , (LispString "foo",                      LispString "foo")
  , (LispNumber $ LispInteger 1,            LispNumber $ LispInteger 1)
  , (LispNumber $ LispRational 3 4,         LispNumber $ LispRational 3 4)
  , (LispNumber $ LispRational 6 8,         LispNumber $ LispRational 6 8)
  ]

quoteTests = testFactory unpackVal
  [ (quote lTrue,                           lTrue)
  , (quote $ quote lTrue,                   quote lTrue)
  , (quote $ LispList [LispSymbol "foo"],   LispList [LispSymbol "foo"])

  , (qquote lTrue,                          lTrue)
  , (qquote $ qquote lTrue,                 qquote lTrue)
  , (qquote $ LispList [LispSymbol "foo"],  LispList [LispSymbol "foo"])
  ]

qquoteTests = testFactory unpackVal
  [
    ( qquote $ unquote $ LispList [LispSymbol "+", lOne, lOne]
    , LispNumber $ LispInteger 2
    )

  , ( qquote $ LispList [ LispSymbol "foo"
                        , unquote $ LispList [LispSymbol "+" , lOne, lOne]
                        ]
    , LispList [LispSymbol "foo", lTwo]
    )

  , ( qquote $ qquote $ unquote $ LispList [LispSymbol "+" , lOne, lOne]
    , qquote $ unquote $ LispList [LispSymbol "+" , lOne, lOne]
    )
  ]

ifTests = testFactory unpackVal
  [ (LispList [LispSymbol "if", lTrue, lOne, lTwo],  lOne)
  , (LispList [LispSymbol "if", lFalse, lOne, lTwo], lTwo)

  , ( LispList [ LispSymbol "if"
              , LispList [LispSymbol "=", lOne, lOne]
              , lOne, lTwo],
      lOne
    )
  , ( LispList [ LispSymbol "if"
              , LispList [LispSymbol "=", lOne, lTwo]
              , lOne, lTwo],
      lTwo
    )
  ]

tests = TestLabel "EVAL" $ TestList
  [ TestLabel "ATOMIC"     atomicTests
  , TestLabel "QUOTE"      quoteTests
  , TestLabel "QUASIQUOTE" qquoteTests
  , TestLabel "IF"         ifTests
  ]
