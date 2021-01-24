{-# LANGUAGE QuasiQuotes #-}

module ParserTests where

import Test.HUnit
import qualified Text.ParserCombinators.Parsec as Parsec
import Text.RawString.QQ

import Types (SchemeNumber(..), SchemeVal(..))
import qualified Parser as LP
import Data.Array (listArray)


apply :: Parsec.Parser a -> String -> a
apply parser input = case Parsec.parse parser "[test]" input of
  Left err -> error $ show err
  Right value -> value

testFactory :: (Eq a, Show a) => Parsec.Parser a -> [(String, a)] -> Test
testFactory parser casePairs = TestList $
  [ TestCase $ assertEqual "" b (apply parser a)
  | (a, b) <- casePairs ]


symbolTests :: Test
symbolTests = testFactory LP.parseSymbol
  [ ("foo",       SSymbol "foo")
  , ("...",       SSymbol "...")
  , ("+",         SSymbol "+")
  , ("-",         SSymbol "-")
  , ("asdf1",     SSymbol "asdf1")
  , ("foo-bar",   SSymbol "foo-bar")
  , ("foo->bar",  SSymbol "foo->bar")
  ]

boolTests :: Test
boolTests = testFactory LP.parseBool
  [ ("#t", SBool True)
  , ("#f", SBool False)
  ]

charTests :: Test
charTests =
  let printingChars =
        map (\c -> ("#\\" ++ [c], SChar c)) $
        concat [ ['a'..'z']
               , ['A'..'Z']
               , map (head . show) [0..9]
               , "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
               ]
      otherChars =
        [ ("#\\space",   SChar ' ')
        , ("#\\newline", SChar '\n')
        , ("#\\tab",     SChar '\t')
        ]
  in testFactory LP.parseCharacter (printingChars ++ otherChars)


stringTests :: Test
stringTests = testFactory LP.parseString
  [ ([r|"foo"|],       SString "foo")
  , ([r|"123"|],       SString "123")
  , ([r|"foo123"|],    SString "foo123")
  , ([r|"123foo"|],    SString "123foo")
  , ([r|"#50jf"|],     SString "#50jf")
  , ([r|"50jf%"|],     SString "50jf%")
  , ([r|" sdf "|],     SString " sdf ")
  , ([r|"foo bar"|],   SString "foo bar")
  , ([r|"\""|],        SString "\"")
  , ([r|"\t"|],        SString "\t")
  , ([r|"\n"|],        SString "\n")
  , ([r|"\r"|],        SString "\r")
  , ([r|"\\"|],        SString "\\")
  , ([r|"a\"b"|],      SString "a\"b")
  , ([r|"a\tb"|],      SString "a\tb")
  , ([r|"a\nb"|],      SString "a\nb")
  , ([r|"a\rb"|],      SString "a\rb")
  , ([r|"a\\b"|],      SString "a\\b")
  ]

integerTests :: Test
integerTests = testFactory LP.parseInteger
  [ ("0",      SInteger 0)
  , ("1",      SInteger 1)
  , ("01",     SInteger 1)
  , ("123",    SInteger 123)

  , ("-0",     SInteger 0)
  , ("-1",     SInteger (-1))
  , ("-01",    SInteger (-1))
  , ("-123",   SInteger (-123))

  , ("#d123",  SInteger 123)
  , ("#b111",  SInteger 7)
  , ("#o11",   SInteger 9)
  , ("#x11",   SInteger 17)

  , ("#D123",  SInteger 123)
  , ("#B111",  SInteger 7)
  , ("#O11",   SInteger 9)
  , ("#X11",   SInteger 17)
  ]

rationalTests :: Test
rationalTests = testFactory LP.parseRational
  [ ("123/123",  SRational 123 123)
  , ("1/123",    SRational 1 123)
  , ("0/123",    SRational 0 123)

  , ("-123/123", SRational (-123) 123)
  , ("-1/123",   SRational (-1) 123)
  , ("-0/123",   SRational 0 123)
  ]

realTests :: Test
realTests = testFactory LP.parseReal
  [ ("123.123",  SReal 123.123)
  , ("123.",     SReal 123)
  , ("123.0",    SReal 123)
  , (".123",     SReal 0.123)
  , ("0.123",    SReal 0.123)

  , ("-123.123", SReal (-123.123))
  , ("-123.",    SReal (-123))
  , ("-123.0",   SReal (-123))
  , ("-.123",    SReal (-0.123))
  , ("-0.123",   SReal (-0.123))
  ]

-- | left-associative version of `$`
($<) :: (a -> b) -> a -> b
($<) = ($)

complexTests :: Test
complexTests = testFactory LP.parseComplex
  [ ("1+1i",      SComplex $< SInteger 1    $< SInteger 1)
  , ("1.1+1.1i",  SComplex $< SReal 1.1     $< SReal 1.1)
  , (".1+.1i",    SComplex $< SReal 0.1     $< SReal 0.1)
  , ("1/2+.1i",   SComplex $< SRational 1 2 $< SReal 0.1)
  , (".1+1/2i",   SComplex $< SReal 0.1     $< SRational 1 2)

  , ("-1+1i",     SComplex $< SInteger (-1)    $< SInteger 1)
  , ("-1.1+1.1i", SComplex $< SReal (-1.1)     $< SReal 1.1)
  , ("-.1+.1i",   SComplex $< SReal (-0.1)     $< SReal 0.1)
  , ("-1/2+.1i",  SComplex $< SRational (-1) 2 $< SReal 0.1)
  , ("-.1+1/2i",  SComplex $< SReal (-0.1)     $< SRational 1 2)

  , ("1-1i",      SComplex $< SInteger 1    $< SInteger (-1))
  , ("1.1-1.1i",  SComplex $< SReal 1.1     $< SReal (-1.1))
  , (".1-.1i",    SComplex $< SReal 0.1     $< SReal (-0.1))
  , ("1/2-.1i",   SComplex $< SRational 1 2 $< SReal (-0.1))
  , (".1-1/2i",   SComplex $< SReal 0.1     $< SRational (-1) 2)
  ]

toSchemeInt :: Integer -> SchemeVal
toSchemeInt = SNumber . SInteger

toSchemeReal :: Float -> SchemeVal
toSchemeReal = SNumber . SReal

toSchemeRational :: Integer -> Integer -> SchemeVal
toSchemeRational a b = SNumber $ SRational a b

listTests :: Test
listTests = testFactory LP.parseListOrDottedList
  [ ("(1 2 3)",           SList [toSchemeInt 1, toSchemeInt 2, toSchemeInt 3])
  , ("()",                SList [])
  , ("(1)",               SList [toSchemeInt 1])
  , ("(.1 2/3)",          SList [toSchemeReal 0.1, toSchemeRational 2 3])

  , ([r|("foo")|],        SList [SString "foo"])
  , ([r|("bar" "baz")|],  SList [SString "bar", SString "baz"])

  , ("(foo)",             SList [SSymbol "foo"])
  , ("(bar baz)",         SList [SSymbol "bar", SSymbol "baz"])
  , ("(() ())",           SList [SList [], SList []])

  , ("(-1 (2 3) 4)",      SList [ toSchemeInt (-1)
                                   , SList [toSchemeInt 2, toSchemeInt 3]
                                   , toSchemeInt 4
                                   ])

  , ( [r|("asdf" (-2/3 #t) #\space)|]
    , SList [ SString "asdf"
               , SList [toSchemeRational (-2) 3, SBool True]
               , SChar ' '
               ]
    )
  ]

dottedListTests :: Test
dottedListTests = testFactory LP.parseListOrDottedList
  [ ("(1 . 2)",           SDottedList [toSchemeInt 1]  (toSchemeInt 2))

  , ("(1 . .2)",          SDottedList [toSchemeInt 1]  (toSchemeReal 0.2))

  , ("(1 . (2 3))",       SList [toSchemeInt 1, toSchemeInt 2, toSchemeInt 3])

  , ("(#f . (2 3))",      SList [SBool False, toSchemeInt 2, toSchemeInt 3])

  , ("(1 2 . 3)",         SDottedList [toSchemeInt 1, toSchemeInt 2] (toSchemeInt 3))

  , ("(1 .2 . -3)",       SDottedList [toSchemeInt 1, toSchemeReal 0.2] (toSchemeInt (-3)))

  , ("(1 . (2 . 3))" ,    SDottedList [toSchemeInt 1, toSchemeInt 2] (toSchemeInt 3))

  , ("(1 . (2 . 3/4))",   SDottedList [toSchemeInt 1, toSchemeInt 2] (toSchemeRational 3 4))
  ]

makeVector :: [SchemeVal] -> SchemeVal
makeVector elems = SVector $ listArray (0, length elems - 1) elems

vectorTests :: Test
vectorTests = testFactory LP.parseVector
  [ ("#(1 2)",  makeVector [toSchemeInt 1, toSchemeInt 2])
  , ("#(1)",    makeVector [toSchemeInt 1])
  , ("#()",     makeVector [])
  , ("#(#t 0)", makeVector [SBool True, toSchemeInt 0])

  , ("#(#(1 2) (3 xyz))", makeVector [ makeVector [toSchemeInt 1, toSchemeInt 2]
                                     , SList [toSchemeInt 3, SSymbol "xyz"]])
  ]

quotedTests :: Test
quotedTests = testFactory LP.parseQuoted
  [ ("'foo",         SList [SSymbol "quote", SSymbol "foo"])

  , ("'(1 2)",       SList [ SSymbol "quote"
                              , SList [toSchemeInt 1, toSchemeInt 2]
                              ])
  , ([r|'(#\' '5)|], SList [ SSymbol "quote"
                              , SList [ SChar '\''
                                         , SList [ SSymbol "quote",
                                                      toSchemeInt 5
                                                    ]
                                         ]
                              ])
  ]

quasiquotedTests :: Test
quasiquotedTests = testFactory LP.parseQuasiquoted
  [ ("`foo",           SList [SSymbol "quasiquote", SSymbol "foo"])

  , ("`(1 2)",         SList [ SSymbol "quasiquote"
                                , SList [toSchemeInt 1, toSchemeInt 2]
                                ])
  , ("``foo",          SList [ SSymbol "quasiquote"
                                , SList [ SSymbol "quasiquote"
                                           , SSymbol "foo"
                                           ]
                                ])
  , ([r|`("foo" `5)|], SList [ SSymbol "quasiquote"
                              , SList [ SString "foo"
                                         , SList [ SSymbol "quasiquote",
                                                      toSchemeInt 5
                                                    ]
                                         ]
                              ])
  ]

unquotedTests :: Test
unquotedTests = testFactory LP.parseUnquoted
  [ (",foo",        SList [SSymbol "unquote", SSymbol "foo"])
  , (",1",          SList [SSymbol "unquote", toSchemeInt 1])
  , (",()",         SList [SSymbol "unquote", SList []])

  , (",,()",        SList [ SSymbol "unquote"
                             , SList [ SSymbol "unquote"
                                        , SList []
                                        ]
                             ])
  , (",(1 ,foo)",   SList [ SSymbol "unquote"
                             , SList [ toSchemeInt 1
                                        , SList [ SSymbol "unquote"
                                                   , SSymbol "foo"
                                                   ]
                                        ]
                             ])
  ]

exprTests :: Test
exprTests = testFactory LP.parseExpr
  [ ("foo",             SSymbol "foo")
  , ("#t",              SBool True)
  , ("#f",              SBool False)
  , ("1",               toSchemeInt 1)
  , ("#b1",             toSchemeInt 1)
  , ("#o1",             toSchemeInt 1)
  , ("#d1",             toSchemeInt 1)
  , ("#x1",             toSchemeInt 1)
  , ("#(foo)",          makeVector [SSymbol "foo"])
  , ("(foo)",           SList [SSymbol "foo"])
  , ("(foo . bar)",     SDottedList $< [SSymbol "foo"] $< SSymbol "bar")
  , ([r|#\a|],          SChar 'a')
  , ([r|#\b|],          SChar 'b')
  , ([r|#\c|],          SChar 'c')
  , ([r|#\x|],          SChar 'x')
  , ([r|#\y|],          SChar 'y')
  , ([r|#\z|],          SChar 'z')
  , ([r|"foo"|],        SString "foo" )
  , ("(1 'foo)",        SList [ toSchemeInt 1
                                 , SList[SSymbol "quote", SSymbol "foo"]
                                 ])
  ]

tests :: Test
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
  , TestLabel  "DOTTED LIST" dottedListTests
  , TestLabel  "VECTOR"      vectorTests
  , TestLabel  "QUOTED"      quotedTests
  , TestLabel  "QUASIQUOTED" quasiquotedTests
  , TestLabel  "UNQUOTED"    unquotedTests
  , TestLabel  "EXPR"        exprTests
  ]
