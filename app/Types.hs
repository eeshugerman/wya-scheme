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

-- instance Show LispVal
-- instance Show LispVal where show = T.unpack . TL.toStrict . pShow
