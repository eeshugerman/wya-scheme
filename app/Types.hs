{-# LANGUAGE LambdaCase #-}
module Types
  ( SchemeNumber (..)
  , SchemeVal (..)
  , SchemeError (..)
  , SchemeValOrError
  , IOSchemeValOrError
  , IONilOrError
  , Env
  ) where

import Data.IORef
import qualified Data.Array as A
import Text.Parsec ( ParseError )
import Control.Monad.Except (ExceptT)
import GHC.IO.Handle (Handle)


unwordsList :: [SchemeVal] -> String
unwordsList = unwords . map showSchemeVal

data SchemeNumber = SComplex SchemeNumber SchemeNumber  -- TODO: use Data.Complex.Complex
                  | SReal Float
                  | SRational Integer Integer  -- TODO: use Data.Ratio.Rational
                  | SInteger Integer
                  deriving Eq

instance Show SchemeNumber where show = showSchemeNumber

showSchemeNumber :: SchemeNumber -> String
showSchemeNumber = \case
  SInteger val          -> show val
  SRational numer denom -> show numer ++ "/" ++ show denom
  SReal val             -> show val
  SComplex real imag    -> showComplex real imag
  where
    showComplex :: SchemeNumber -> SchemeNumber -> String
    showComplex real imag = show real ++ showWithSign imag ++ "i"

    showWithSign :: SchemeNumber -> String
    showWithSign = \case
      SInteger val            -> maybePlusStr val ++ show val
      SReal val               -> maybePlusStr val ++ show val
      val@(SRational numer _) -> maybePlusStr numer ++ showSchemeNumber val
      SComplex _ _            -> error "internal error: `imag` is a complex number. "
                                    ++ "this expression should not have parsed."

    maybePlusStr :: (Num a, Ord a) => a -> String
    maybePlusStr val = if val > 0 then "+" else ""

data SchemeVal = SSymbol String
               | SBool Bool
               | SChar Char
               | SString String
               | SNumber SchemeNumber
               | SList [SchemeVal]
               | SVector (A.Array Int SchemeVal)
               | SDottedList [SchemeVal] SchemeVal
               | SPort Handle
               | SPrimativeProc ([SchemeVal] -> SchemeValOrError)
               | SIOProc ([SchemeVal] -> IOSchemeValOrError)
               | SProc { procParams    :: [String]
                       , procVarParam  :: Maybe String
                       , procBody      :: [SchemeVal]
                       , procClosure   :: Env
                       }

instance Show SchemeVal where show = showSchemeVal

showSchemeVal :: SchemeVal -> String
showSchemeVal = \case
  SSymbol val           -> val
  SBool True            -> "#t"
  SBool False           -> "#f"
  SChar val             -> "#\\" ++ [val] -- TODO: named chars
  SString val           -> "\"" ++ val ++ "\""
  SNumber val           -> show val
  SList val             -> "(" ++ unwordsList val ++ ")"
  SVector val           -> "#(" ++ unwordsList (A.elems val) ++ ")"
  SDottedList begin end -> "(" ++ unwordsList begin ++ " . " ++ show end ++ ")"
  SPort _               -> "<IO port>"
  SPrimativeProc _      -> "<primitive>"
  SIOProc _             -> "<IO primitive>"
  SProc {procParams=params, procVarParam=varParam} ->
    "(lambda (" ++ unwords params ++ showVarArgs ++ ") ...)"
    where
      showVarArgs = case varParam of
        Nothing -> ""
        Just val  -> ". " ++ val


data SchemeError = NumArgs Integer [SchemeVal]
                 | TypeMismatch String SchemeVal
                 | ParseError ParseError
                 | BadForm String SchemeVal
                 | UnboundVar String
                 | Default String

instance Show SchemeError where show = showSchemeError

showSchemeError :: SchemeError -> String
showSchemeError = \case
  UnboundVar varname           -> "Unbound variable: " ++ varname
  BadForm msg form             -> msg ++ ": " ++ show form
  NumArgs expected found       -> "Expected " ++ show expected
                                  ++ " args; found values " ++ unwordsList found
  TypeMismatch expected found  -> "Invalid type: expected " ++ expected
                                  ++ ", found " ++ show found
  ParseError err               -> "Parse error at " ++ show err
  Default msg                  -> "Error: " ++ msg



type Env = IORef [(String, IORef SchemeVal)]

type SchemeValOrError = Either SchemeError SchemeVal
type IOSchemeValOrError = ExceptT SchemeError IO SchemeVal
type IONilOrError = ExceptT SchemeError IO ()
