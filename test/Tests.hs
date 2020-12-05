{-# LANGUAGE QuasiQuotes #-}

module Main where

import Test.HUnit
import qualified Text.ParserCombinators.Parsec as Parsec
import Text.RawString.QQ

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

testListFactory parser constructor cases = TestList $
  [ TestCase $ assertEqual "" (apply parser a) (constructor b)
  | (a, b) <- cases ]


symbolTests = testListFactory LP.parseSymbol LispSymbol
  [ ("foo",       "foo")
  , ("...",       "...")
  , ("+",         "+")
  , ("-",         "-")
  , ("asdf1",     "asdf1")
  , ("foo->bar",  "foo->bar")
  ]

boolTests = testListFactory LP.parseBool LispBool
  [ ("#t", True)
  , ("#f", False)]

charTests = testListFactory LP.parseCharacter LispCharacter
  [ ([r|#\a|],            'a')
  , ([r|#\A|],            'A')
  , ([r|#\(|],            '(')
  , ([r|#\f|],            'f')
  , ([r|#\t|],            't')
  , ([r|#\1|],            '1')
  , ([r|#\\|],            '\\')
  , ([r|#\#|],            '#')
  , ([r|#\space|],        ' ')
  , ([r|#\newline|],      '\n')
  ]

tests = TestList
  [ TestLabel "bool tests"   boolTests
  , TestLabel "symbol tests" symbolTests
  , TestLabel "char tests"   charTests
  ]

main = do runTestTTAndExit tests
