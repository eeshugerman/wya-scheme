module EvalTests where

import Test.HUnit

import Eval
import Types
import qualified Primitives as Prim

unpackVal :: SchemeValOrError -> SchemeVal
unpackVal (Left err) = error $ "unexpected error: " ++ show err
unpackVal (Right val) = val

testFactory :: (Eq a, Show a) => (SchemeValOrError -> a) -> [(SchemeVal, a)] -> Test
testFactory unpacker casePairs = TestList $
  [ TestCase $ assertEqual "" expected (unpacker $ eval input)
  | (input, expected) <- casePairs]

-- TODO: maybe just parse scheme for these
-- shorthand
quote val = SList [SSymbol "quote", val]
qquote val = SList [SSymbol "quasiquote", val]
unquote val = SList [SSymbol "unquote", val]
unquoteSplicing val = SList [SSymbol "unquote-splicing", val]
lTrue = SBool True
lFalse = SBool False
lOne = SNumber $ SInteger 1
lTwo = SNumber $ SInteger 2
lThree = SNumber $ SInteger 3

atomicTests = testFactory unpackVal
  [ (lTrue,                                 lTrue)
  , (SChar 'a',                     SChar 'a')
  , (SString "foo",                      SString "foo")
  , (SNumber $ SInteger 1,            SNumber $ SInteger 1)
  , (SNumber $ SRational 3 4,         SNumber $ SRational 3 4)
  , (SNumber $ SRational 6 8,         SNumber $ SRational 6 8)
  ]

quoteTests = testFactory unpackVal
  [ (quote lTrue,                           lTrue)
  , (quote $ quote lTrue,                   quote lTrue)
  , (quote $ SList [SSymbol "foo"],   SList [SSymbol "foo"])

  , (qquote lTrue,                          lTrue)
  , (qquote $ qquote lTrue,                 qquote lTrue)
  , (qquote $ SList [SSymbol "foo"],  SList [SSymbol "foo"])
  ]

qquoteTests = testFactory unpackVal
  [
    ( qquote $ unquote $ SList [SSymbol "+", lOne, lOne]
    , SNumber $ SInteger 2
    )

  , ( qquote $ SList [ SSymbol "foo"
                        , unquote $ SList [SSymbol "+" , lOne, lOne]
                        ]
    , SList [SSymbol "foo", lTwo]
    )

  , ( qquote $ qquote $ unquote $ SList [SSymbol "+" , lOne, lOne]
    , qquote $ unquote $ SList [SSymbol "+" , lOne, lOne]
    )
  ]

ifTests = testFactory unpackVal
  [ (SList [SSymbol "if", lTrue, lOne, lTwo],  lOne)
  , (SList [SSymbol "if", lFalse, lOne, lTwo], lTwo)

  , ( SList [ SSymbol "if"
              , SList [SSymbol "=", lOne, lOne]
              , lOne, lTwo],
      lOne
    )
  , ( SList [ SSymbol "if"
              , SList [SSymbol "=", lOne, lTwo]
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
