module Types
  ( Sign (..)
  , Radix (..)
  , LispVal (..)
  ) where

import qualified Data.Array as A

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
             | LispVector (A.Array Int LispVal)
             | LispDottedList [LispVal] LispVal
             deriving Eq

instance Show LispVal where show = showLispVal

-- instance Ord LispVal where (>)  = greaterThanLispVal

-- toFloat :: Integer -> Float
-- toFloat = fromInteger

-- greaterThanLispVal :: LispVal -> LispVal -> Bool
-- greaterThanLispVal (LispInteger a)    (LispInteger b) = a > b
-- greaterThanLispVal (LispReal a)       (LispInteger b) = a > toFloat b
-- greaterThanLispVal (LispRational a b) (LispInteger c) = toFloat a / toFloat b > toFloat c
-- greaterThanLispVal _                  _               = error "not implemented"



unwordsList :: [LispVal] -> String
unwordsList = unwords . map showLispVal

unwordsArray :: A.Array Int LispVal -> String
unwordsArray arr = unwords $ map showLispVal (A.elems arr)


showComplex :: LispVal -> LispVal -> String
showComplex real imag = show real ++ showWithSign imag ++ "i"
  where
    -- TODO: i think this would be much simpler if `>` were implemented on LispVal
    showWithSign :: LispVal -> String
    showWithSign (LispInteger val)          = maybePlusStr val ++ show val
    showWithSign (LispReal val)             = maybePlusStr val ++ show val
    showWithSign val@(LispRational numer _) = maybePlusStr numer ++ showLispVal val
    showWithSign _ = error "invalid value found for imaginary component of complex number"

    maybePlusStr :: (Num a, Ord a) => a -> String
    maybePlusStr val = if val > 0 then "+" else ""


showLispVal :: LispVal -> String
showLispVal (LispSymbol val)           = val
showLispVal (LispBool True)            = "#t"
showLispVal (LispBool False)           = "#f"
showLispVal (LispCharacter val)        = "#\\" ++ [val] -- TODO: named chars
showLispVal (LispString val)           = "\"" ++ val ++ "\""
showLispVal (LispInteger val)          = show val
showLispVal (LispRational numer denom) = show numer ++ "/" ++ show denom
showLispVal (LispReal val)             = show val
showLispVal (LispComplex real imag)    = showComplex real imag
showLispVal (LispList val)             = "(" ++ unwordsList val ++ ")"
showLispVal (LispVector val)           = "#(" ++ unwordsArray val ++ ")"
showLispVal (LispDottedList begin end) = "(" ++ unwordsList begin ++ " . " ++ show end ++ ")"


