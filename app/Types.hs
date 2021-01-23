{-# LANGUAGE LambdaCase #-}
module Types
  ( LispNumber (..)
  , LispVal (..)
  , LispError (..)
  , LispValOrError
  , Env
  , nullEnv
  ) where

import Data.IORef
import qualified Data.Array as A
import Text.Parsec ( ParseError )


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showLispVal

data LispNumber = LispComplex LispNumber LispNumber  -- TODO: use Data.Complex.Complex
                | LispReal Float
                | LispRational Integer Integer  -- TODO: use Data.Ratio.Rational
                | LispInteger Integer
                deriving Eq

instance Show LispNumber where show = showLispNumber

showLispNumber :: LispNumber -> String
showLispNumber = \case
  LispInteger val          -> show val
  LispRational numer denom -> show numer ++ "/" ++ show denom
  LispReal val             -> show val
  LispComplex real imag    -> showComplex real imag
  where
    showComplex :: LispNumber -> LispNumber -> String
    showComplex real imag = show real ++ showWithSign imag ++ "i"

    showWithSign :: LispNumber -> String
    showWithSign = \case
      LispInteger val            -> maybePlusStr val ++ show val
      LispReal val               -> maybePlusStr val ++ show val
      val@(LispRational numer _) -> maybePlusStr numer ++ showLispNumber val
      LispComplex _ _            -> error "internal error: `imag` is a complex number. "
                                       ++ "this expression should not have parsed."

    maybePlusStr :: (Num a, Ord a) => a -> String
    maybePlusStr val = if val > 0 then "+" else ""

data LispVal = LispSymbol String
             | LispBool Bool
             | LispCharacter Char
             | LispString String
             | LispNumber LispNumber
             | LispList [LispVal]
             | LispVector (A.Array Int LispVal)
             | LispDottedList [LispVal] LispVal
             | LispPrimitiveProc ([LispVal] -> LispValOrError)
             | LispProc { procParams    :: [String]
                        , procVarParams :: Maybe String
                        , procBody      :: [LispVal]
                        , procClosure   :: Env
                        }

instance Show LispVal where show = showLispVal

showLispVal :: LispVal -> String
showLispVal = \case
  LispSymbol val           -> val
  LispBool True            -> "#t"
  LispBool False           -> "#f"
  LispCharacter val        -> "#\\" ++ [val] -- TODO: named chars
  LispString val           -> "\"" ++ val ++ "\""
  LispNumber val           -> show val
  LispList val             -> "(" ++ unwordsList val ++ ")"
  LispVector val           -> "#(" ++ unwordsList (A.elems val) ++ ")"
  LispDottedList begin end -> "(" ++ unwordsList begin ++ " . " ++ show end ++ ")"
  LispPrimitiveProc _      -> "<primative>"
  LispProc {procParams=params, procVarParams=varParams} ->
    "(lambda (" ++ unwords params ++ showVarArgs ++ ") ...)"
    where showVarArgs = case varParams of
            Nothing -> ""
            Just val  -> ". " ++ val



data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | ParseError ParseError
               | BadForm String LispVal
               | UnboundVar String
               | Default String

instance Show LispError where show = showLispError

showLispError :: LispError -> String
showLispError = \case
  UnboundVar varname           -> "Unbound variable: " ++ varname
  BadForm msg form             -> msg ++ ": " ++ show form
  NumArgs expected found       -> "Expected " ++ show expected
                                  ++ " args; found values " ++ unwordsList found
  TypeMismatch expected found  -> "Invalid type: expected " ++ expected
                                  ++ ", found " ++ show found
  ParseError err               -> "Parse error at " ++ show err
  Default msg                  -> "Error: " ++ msg


type LispValOrError = Either LispError LispVal

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []
