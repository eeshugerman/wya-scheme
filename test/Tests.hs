{-# LANGUAGE QuasiQuotes #-}

module Main where

import Test.HUnit
import qualified Text.ParserCombinators.Parsec as Parsec
import Text.RawString.QQ

import Types (LispVal(..))
import qualified Parser as LP


apply :: Parsec.Parser LispVal -> String -> LispVal
apply parser input = case Parsec.parse parser"[test]" input of
  Left err -> error ("No match: " ++ show err)
  Right value -> value

testListFactory parser constructor casePairs = TestList $
  [ TestCase $ assertEqual "" (apply parser a) (constructor b)
  | (a, b) <- casePairs ]


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
  [ ([r|#\a|],            'a' )
  , ([r|#\A|],            'A' )
  , ([r|#\(|],            '(' )
  , ([r|#\f|],            'f' )
  , ([r|#\t|],            't' )
  , ([r|#\1|],            '1' )
  , ([r|#\\|],            '\\')
  , ([r|#\#|],            '#' )
  , ([r|#\space|],        ' ' )
  , ([r|#\newline|],      '\n')
  ]

stringTests = testListFactory LP.parseString LispString
  [ ([r|"foo"|],       "foo")
  , ([r|"123"|],       "123")
  , ([r|"foo123"|],    "foo123")
  , ([r|"123foo"|],    "123foo")
  , ([r|"#50jf"|],     "#50jf")
  , ([r|"50jf%"|],     "50jf%")
  , ([r|" sdf "|],     " sdf ")
  , ([r|"foo bar"|],   "foo bar")
  , ([r|"\""|],        "\"")
  , ([r|"\t"|],        "\t")
  , ([r|"\n"|],        "\n")
  , ([r|"\r"|],        "\r")
  , ([r|"\\"|],        "\\")
  , ([r|"a\"b"|],      "a\"b")
  , ([r|"a\tb"|],      "a\tb")
  , ([r|"a\nb"|],      "a\nb")
  , ([r|"a\rb"|],      "a\rb")
  , ([r|"a\\b"|],      "a\\b")
  ]

integerTests = testListFactory LP.parseInteger LispInteger
  [ ("0",      0)
  , ("1",      1)
  , ("123",    123)
  , ("#d123",  123)
  , ("#b111",  7)
  , ("#o11",   9)
  , ("#x11",   17)
  , ("#D123",  123)
  , ("#B111",  7)
  , ("#O11",   9)
  , ("#X11",   17)
  ]

tests = TestList
  [ TestLabel "bool tests"      boolTests
  , TestLabel "symbol tests"    symbolTests
  , TestLabel "char tests"      charTests
  , TestLabel "string tests"    stringTests
  , TestLabel "integer tests"   integerTests
  ]

main = do runTestTTAndExit tests
