{-# LANGUAGE QuasiQuotes #-}

module Main where

import Test.HUnit
import qualified Text.ParserCombinators.Parsec as Parsec
import Text.RawString.QQ

import Types (LispVal(..))
import qualified Parser as LP
import Data.Array (listArray)


apply :: Parsec.Parser LispVal -> String -> LispVal
apply parser input = case Parsec.parse parser"[test]" input of
  Left err -> error $ show err
  Right value -> value

testFactory :: Parsec.Parser LispVal -> [(String, LispVal)] -> Test
testFactory parser casePairs = TestList $
  [ TestCase $ assertEqual "" (apply parser a) b
  | (a, b) <- casePairs ]


symbolTests = testFactory LP.parseSymbol
  [ ("foo",       LispSymbol "foo")
  , ("...",       LispSymbol "...")
  , ("+",         LispSymbol "+")
  , ("-",         LispSymbol "-")
  , ("asdf1",     LispSymbol "asdf1")
  , ("foo->bar",  LispSymbol "foo->bar")
  ]

boolTests = testFactory LP.parseBool
  [ ("#t", LispBool True)
  , ("#f", LispBool False)]

charTests = testFactory LP.parseCharacter
  [ ([r|#\a|],            LispCharacter 'a')
  , ([r|#\A|],            LispCharacter 'A')
  , ([r|#\(|],            LispCharacter '(')
  , ([r|#\f|],            LispCharacter 'f')
  , ([r|#\t|],            LispCharacter 't')
  , ([r|#\1|],            LispCharacter '1')
  , ([r|#\\|],            LispCharacter '\\')
  , ([r|#\#|],            LispCharacter '#')
  , ([r|#\space|],        LispCharacter ' ')
  , ([r|#\newline|],      LispCharacter '\n')
  ]

stringTests = testFactory LP.parseString
  [ ([r|"foo"|],       LispString "foo")
  , ([r|"123"|],       LispString "123")
  , ([r|"foo123"|],    LispString "foo123")
  , ([r|"123foo"|],    LispString "123foo")
  , ([r|"#50jf"|],     LispString "#50jf")
  , ([r|"50jf%"|],     LispString "50jf%")
  , ([r|" sdf "|],     LispString " sdf ")
  , ([r|"foo bar"|],   LispString "foo bar")
  , ([r|"\""|],        LispString "\"")
  , ([r|"\t"|],        LispString "\t")
  , ([r|"\n"|],        LispString "\n")
  , ([r|"\r"|],        LispString "\r")
  , ([r|"\\"|],        LispString "\\")
  , ([r|"a\"b"|],      LispString "a\"b")
  , ([r|"a\tb"|],      LispString "a\tb")
  , ([r|"a\nb"|],      LispString "a\nb")
  , ([r|"a\rb"|],      LispString "a\rb")
  , ([r|"a\\b"|],      LispString "a\\b")
  ]

integerTests = testFactory LP.parseInteger
  [ ("0",      LispInteger 0)
  , ("1",      LispInteger 1)
  , ("01",     LispInteger 1)
  , ("123",    LispInteger 123)

  , ("-0",     LispInteger 0)
  , ("-1",     LispInteger (-1))
  , ("-01",    LispInteger (-1))
  , ("-123",   LispInteger (-123))

  , ("#d123",  LispInteger 123)
  , ("#b111",  LispInteger 7)
  , ("#o11",   LispInteger 9)
  , ("#x11",   LispInteger 17)

  , ("#D123",  LispInteger 123)
  , ("#B111",  LispInteger 7)
  , ("#O11",   LispInteger 9)
  , ("#X11",   LispInteger 17)
  ]

rationalTests = testFactory LP.parseRational
  [ ("123/123",  LispRational 123 123)
  , ("1/123",    LispRational 1 123)
  , ("0/123",    LispRational 0 123)

  , ("-123/123", LispRational (-123) 123)
  , ("-1/123",   LispRational (-1) 123)
  , ("-0/123",   LispRational 0 123)
  ]

realTests = testFactory LP.parseReal
  [ ("123.123",  LispReal 123.123)
  , ("123.",     LispReal 123)
  , ("123.0",    LispReal 123)
  , (".123",     LispReal 0.123)
  , ("0.123",    LispReal 0.123)

  , ("-123.123", LispReal (-123.123))
  , ("-123.",    LispReal (-123))
  , ("-123.0",   LispReal (-123))
  , ("-.123",    LispReal (-0.123))
  , ("-0.123",   LispReal (-0.123))
  ]

($<) :: (a -> b) -> a -> b
($<) = ($)

complexTests = testFactory LP.parseComplex
  [ ("1+1i",      LispComplex $< LispInteger 1    $< LispInteger 1)
  , ("1.1+1.1i",  LispComplex $< LispReal 1.1     $< LispReal 1.1)
  , (".1+.1i",    LispComplex $< LispReal 0.1     $< LispReal 0.1)
  , ("1/2+.1i",   LispComplex $< LispRational 1 2 $< LispReal 0.1)
  , (".1+1/2i",   LispComplex $< LispReal 0.1     $< LispRational 1 2)

  , ("-1+1i",     LispComplex $< LispInteger (-1)    $< LispInteger 1)
  , ("-1.1+1.1i", LispComplex $< LispReal (-1.1)     $< LispReal 1.1)
  , ("-.1+.1i",   LispComplex $< LispReal (-0.1)     $< LispReal 0.1)
  , ("-1/2+.1i",  LispComplex $< LispRational (-1) 2 $< LispReal 0.1)
  , ("-.1+1/2i",  LispComplex $< LispReal (-0.1)     $< LispRational 1 2)

  , ("1-1i",      LispComplex $< LispInteger 1    $< LispInteger (-1))
  , ("1.1-1.1i",  LispComplex $< LispReal 1.1     $< LispReal (-1.1))
  , (".1-.1i",    LispComplex $< LispReal 0.1     $< LispReal (-0.1))
  , ("1/2-.1i",   LispComplex $< LispRational 1 2 $< LispReal (-0.1))
  , (".1-1/2i",   LispComplex $< LispReal 0.1     $< LispRational (-1) 2)
  ]

listTests = testFactory LP.parseListOrDottedList
  [ ("(1 2 3)",           LispList [LispInteger 1, LispInteger 2, LispInteger 3])
  , ("()",                LispList [])
  , ("(1)",               LispList [LispInteger 1])
  , ("(.1 2/3)",          LispList [LispReal 0.1, LispRational 2 3])

  , ([r|("foo")|],        LispList [LispString "foo"])
  , ([r|("bar" "baz")|],  LispList [LispString "bar", LispString "baz"])

  , ("(foo)",             LispList [LispSymbol "foo"])
  , ("(bar baz)",         LispList [LispSymbol "bar", LispSymbol "baz"])
  , ("(() ())",           LispList [LispList [], LispList []])

  , ("(-1 (2 3) 4)",      LispList [ LispInteger (-1)
                                   , LispList [LispInteger 2, LispInteger 3]
                                   , LispInteger 4
                                   ])

  , ( [r|("asdf" (-2/3 #t) #\space)|]
    , LispList [ LispString "asdf"
               , LispList [LispRational (-2) 3, LispBool True]
               , LispCharacter ' '
               ]
    )
  ]

testDottedList = testFactory LP.parseListOrDottedList
  [ ("(1 . 2)",           LispDottedList [LispInteger 1]  (LispInteger 2))

  , ("(1 . .2)",          LispDottedList [LispInteger 1]  (LispReal 0.2))

  , ("(1 . (2 3))",       LispDottedList [LispInteger 1]
                                         (LispList [LispInteger 2, LispInteger 3]))

  , ("(#f . (2 3))",      LispDottedList [LispBool False]
                                         (LispList [LispInteger 2, LispInteger 3]))

  , ("(1 2 . 3)",         LispDottedList [LispInteger 1, LispInteger 2]
                                         (LispInteger 3))

  , ("(1 .2 . -3)",       LispDottedList [LispInteger 1, LispReal 0.2]
                                         (LispInteger (-3)))

  , ("(1 . (2 . 3))" ,    LispDottedList [LispInteger 1]
                                         (LispDottedList [LispInteger 2]
                                                         (LispInteger 3)))
  , ("(1 . (2 . 3/4))",   LispDottedList [LispInteger 1]
                                         (LispDottedList [LispInteger 2]
                                                         (LispRational 3 4)))
  ]

makeVector :: [LispVal] -> LispVal
makeVector elems = LispVector $ listArray (0, length elems - 1) elems

testVector = testFactory LP.parseVector
  [ ("#(1 2)",  makeVector [LispInteger 1, LispInteger 2])
  , ("#(1)",    makeVector [LispInteger 1])
  , ("#()",     makeVector [])
  , ("#(#t 0)", makeVector [LispBool True, LispInteger 0])

  , ("#(#(1 2) (3 xyz))", makeVector [ makeVector [LispInteger 1, LispInteger 2]
                                     , LispList [LispInteger 3, LispSymbol "xyz"]])
  ]

testQuoted = testFactory LP.parseQuoted
  [ ("'foo",         LispList [LispSymbol "quote", LispSymbol "foo"])

  , ("'(1 2)",       LispList [ LispSymbol "quote"
                              , LispList [LispInteger 1, LispInteger 2]
                              ])
  , ([r|'(#\' '5)|], LispList [ LispSymbol "quote"
                              , LispList [ LispCharacter '\''
                                         , LispList [ LispSymbol "quote",
                                                      LispInteger 5
                                                    ]
                                         ]
                              ])
  ]

testQuasiquoted = testFactory LP.parseQuasiquoted
  [ ("`foo",           LispList [LispSymbol "quasiquote", LispSymbol "foo"])

  , ("`(1 2)",         LispList [ LispSymbol "quasiquote"
                                , LispList [LispInteger 1, LispInteger 2]
                                ])
  , ("``foo",          LispList [ LispSymbol "quasiquote"
                                , LispList [ LispSymbol "quasiquote"
                                           , LispSymbol "foo"
                                           ]
                                ])
  , ([r|`("foo" `5)|], LispList [ LispSymbol "quasiquote"
                              , LispList [ LispString "foo"
                                         , LispList [ LispSymbol "quasiquote",
                                                      LispInteger 5
                                                    ]
                                         ]
                              ])
  ]

testUnquoted = testFactory LP.parseUnquoted
  [ (",foo",        LispList [LispSymbol "unquote", LispSymbol "foo"])
  , (",1",          LispList [LispSymbol "unquote", LispInteger 1])
  , (",()",         LispList [LispSymbol "unquote", LispList []])

  , (",,()",        LispList [ LispSymbol "unquote"
                             , LispList [ LispSymbol "unquote"
                                        , LispList []
                                        ]
                             ])
  , (",(1 ,foo)",   LispList [ LispSymbol "unquote"
                             , LispList [ LispInteger 1
                                        , LispList [ LispSymbol "unquote"
                                                   , LispSymbol "foo"
                                                   ]
                                        ]
                             ])
  ]

tests = TestLabel "PARSE" $ TestList
  [ TestLabel  "BOOL"        boolTests
  , TestLabel  "SYMBOL"      symbolTests
  , TestLabel  "CHAR"        charTests
  , TestLabel  "STRING"      stringTests
  , TestLabel  "INTEGER"     integerTests
  , TestLabel  "RATIONAL"    rationalTests
  , TestLabel  "REAL"        realTests
  , TestLabel  "COMPLEX"     complexTests
  , TestLabel  "LIST"        listTests
  , TestLabel  "DOTTED LIST" testDottedList
  , TestLabel  "VECTOR"      testVector
  , TestLabel  "QUOTED"      testQuoted
  , TestLabel  "QUASIQUOTED" testQuasiquoted
  , TestLabel  "UNQUOTED"    testUnquoted
  ]

main = do runTestTTAndExit tests
