module Main where

import Test.HUnit
import qualified Text.ParserCombinators.Parsec as Parsec

import Types (LispVal(..))
import qualified Parser as LP

instance Eq LispVal -- TODO: explicit implementation

apply :: Parsec.Parser LispVal -> String -> LispVal
apply parser input = case Parsec.parse parser"[test]" input of
  Left err -> error ("No match: " ++ show err)
  Right value -> value




boolTests = TestList
  [ TestCase (assertEqual "" (LispBool True) (apply LP.parseBool "#t"))
  , TestCase (assertEqual "" (LispBool True) (apply LP.parseBool "#f"))
  ]

tests = TestList
  [ TestLabel "bool tests" boolTests
  ]


main = do runTestTT tests
