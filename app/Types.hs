{-# LANGUAGE LambdaCase #-}
module Types
  ( LispVal (..)
  , LispError (..)
  , LispValOrError
  ) where

import qualified Data.Array as A
import Text.Parsec ( ParseError )


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showLispVal

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

showLispVal :: LispVal -> String
showLispVal = \case
  LispSymbol val           -> val
  LispBool True            -> "#t"
  LispBool False           -> "#f"
  LispCharacter val        -> "#\\" ++ [val] -- TODO: named chars
  LispString val           -> "\"" ++ val ++ "\""
  LispInteger val          -> show val
  LispRational numer denom -> show numer ++ "/" ++ show denom
  LispReal val             -> show val
  LispComplex real imag    -> showComplex real imag
  LispList val             -> "(" ++ unwordsList val ++ ")"
  LispVector val           -> "#(" ++ unwordsList (A.elems val) ++ ")"
  LispDottedList begin end -> "(" ++ unwordsList begin ++ " . " ++ show end ++ ")"
  where
    showComplex :: LispVal -> LispVal -> String
    showComplex real imag = show real ++ showWithSign imag ++ "i"

    -- TODO: i think this would be much simpler if `>` were implemented on LispVal
    showWithSign :: LispVal -> String
    showWithSign = \case
      LispInteger val            -> maybePlusStr val ++ show val
      LispReal val               -> maybePlusStr val ++ show val
      val@(LispRational numer _) -> maybePlusStr numer ++ showLispVal val
      _ -> error "invalid value found for imaginary component of complex number"

    maybePlusStr :: (Num a, Ord a) => a -> String
    maybePlusStr val = if val > 0 then "+" else ""



data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | ParseError ParseError
               | BadForm String LispVal
               | NotAFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showLispError

showLispError :: LispError -> String
showLispError = \case
  UnboundVar msg varname       -> msg ++ ": " ++ varname
  BadForm msg form             -> msg ++ ": " ++ show form
  NotAFunction msg func        -> msg ++ ": " ++ show func
  NumArgs expected found       -> "Expected " ++ show expected
                                  ++ " args; found values " ++ unwordsList found
  TypeMismatch expected found  -> "Invalid type: expected " ++ expected
                                  ++ ", found " ++ show found
  ParseError err               -> "Parse error at " ++ show err
  Default msg                  -> "Error: " ++ msg


type LispValOrError = Either LispError LispVal

