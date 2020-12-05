module Types
  ( Sign (..)
  , Radix (..)
  , LispVal (..)
  ) where

import Data.Array (Array)

data Sign = Plus | Minus

data Radix = Binary | Octal | Decimal | Hex

data LispVal = LispSymbol String
             | LispBool Bool
             | LispCharacter Char
             | LispString String
             | LispInteger Integer
             | LispRational Integer Integer
             | LispReal Float
             | LispComplex LispVal LispVal
             | LispList [LispVal]
             | LispVector (Array Int LispVal)
             | LispDottedList [LispVal] LispVal
             deriving Show

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
