module Main where

import Test.HUnit
import qualified Text.ParserCombinators.Parsec as Parsec

import Types (LispVal(..))
import qualified Parser as LP

lispValEquals :: LispVal -> LispVal -> Bool
lispValEquals x y = case (x, y) of
  (LispSymbol       x',     LispSymbol     y'    )     -> x' == y'
  (LispBool         x',     LispBool       y'    )     -> x' == y'
  (LispCharacter    x',     LispCharacter  y'    )     -> x' == y'
  (LispString       x',     LispString     y'    )     -> x' == y'
  (LispInteger      x',     LispInteger    y'    )     -> x' == y'
  (LispRational     x' x'', LispRational   y' y'')     -> x' == y' && x'' == y''
  (LispReal         x',     LispReal       y'    )     -> x' == y'
  (LispComplex      x' x'', LispComplex    y' y'')     -> x' == y' && x'' == y''
  (LispList         x',     LispList       y'    )     -> x' == y'
  (LispVector       x',     LispVector     y'    )     -> x' == y'
  (LispDottedList   x' x'', LispDottedList y' y'')     -> x' == y' && x'' == y''
  _ -> False

instance Eq LispVal where (==) = lispValEquals

apply :: Parsec.Parser LispVal -> String -> LispVal
apply parser input = case Parsec.parse parser"[test]" input of
  Left err -> error ("No match: " ++ show err)
  Right value -> value

symbolTests = TestList
  [ TestCase $ assertEqual "" (apply LP.parseSymbol "foo") (LispSymbol "foo")
  , TestCase $ assertEqual "" (apply LP.parseSymbol "...") (LispSymbol "...")
  , TestCase $ assertEqual "" (apply LP.parseSymbol "+")   (LispSymbol "+")
  , TestCase $ assertEqual "" (apply LP.parseSymbol "-")   (LispSymbol "-")
  ]

boolTests = TestList
  [ TestCase $ assertEqual "" (apply LP.parseBool "#t") (LispBool True)
  , TestCase $ assertEqual "" (apply LP.parseBool "#f") (LispBool False)
  ]

tests = TestList
  [ TestLabel "bool tests"   boolTests
  , TestLabel "symbol tests" symbolTests
  ]

main = do runTestTTAndExit tests
