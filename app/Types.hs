{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
module Types
  ( SchemeReal(..)
  , SchemeComplex
  , SchemeNumber
    ( ..
    , SInteger
    , SRational
    , SReal
    , SComplex
    , SComplex'
    )
  , SchemeVal (..)
  , SchemeError (..)
  , SchemeValOrError
  , IOSchemeValOrError
  , IONilOrError
  , Env
  ) where

import Data.IORef
import qualified Data.Array as A
import Text.Parsec (ParseError)
import Control.Monad.Except (ExceptT)
import GHC.IO.Handle (Handle)
import Data.Ratio (numerator, denominator)
import Data.Complex (Complex, Complex((:+)))

unwordsList :: [SchemeVal] -> String
unwordsList = unwords . map show

-- naming convention: `Foo'` is a real constructor, `Foo` is a pattern synonym
-- exception: `SComplex'`

data SchemeReal
  = SReal' Float
  | SRational' Rational
  | SInteger' Integer

type SchemeComplex = Complex SchemeReal

data SchemeNumber
  = Real' SchemeReal
  | Complex' SchemeComplex

pattern SComplex' :: SchemeReal -> SchemeReal -> SchemeNumber
pattern SComplex' real imag = Complex' (real :+ imag)

pattern SComplex :: SchemeComplex -> SchemeNumber
pattern SComplex val = Complex' val

pattern SReal :: Float -> SchemeNumber
pattern SReal val = Real' (SReal' val)

pattern SRational :: Rational -> SchemeNumber
pattern SRational val = Real' (SRational' val)

pattern SInteger :: Integer -> SchemeNumber
pattern SInteger val = Real' (SInteger' val)

{-# COMPLETE SComplex, SReal, SRational, SInteger #-}
{-# COMPLETE SComplex', SReal, SRational, SInteger #-}

instance Show SchemeReal where
  show = \case
    SInteger' val            -> show val
    SRational' val           -> showRational val
    SReal' val               -> show val
    where
      showRational val =
        let numer = show $ numerator val
            denom = show $ denominator val
        in numer ++ "/" ++ denom

instance Show SchemeNumber where
  show = \case
    SInteger val        -> show val
    SRational val       -> show val
    SReal val           -> show val
    SComplex' real imag -> showComplex real imag
    where
      showComplex real imag =
        let maybePlus = if imag > SInteger' 0 then "+" else ""
        in show real ++ maybePlus ++ show imag ++ "i"

instance Eq SchemeReal where
  (==) a b = case (a, b) of
    (SReal' a', SReal' b')         -> a' == b'
    (SReal' a', SRational' b')     -> a' == fromRational b'
    (SReal' a', SInteger' b')      -> a' == fromInteger b'

    (SRational' a',  SReal' b')    -> fromRational a' == b'
    (SRational' a', SRational' b') -> a' == b'
    (SRational' a', SInteger' b')  -> a' == fromInteger b'

    (SInteger' a',  SReal' b')     -> fromInteger a' == b'
    (SInteger' a',  SRational' b') -> fromInteger a' == b'
    (SInteger' a', SInteger' b')   -> a' == b'

instance Ord SchemeReal where
  compare a b = case (a, b) of
    (SReal' a', SReal' b')         -> compare a' b'
    (SReal' a', SRational' b')     -> compare a' (fromRational b')
    (SReal' a', SInteger' b')      -> compare a' (fromInteger b')

    (SRational' a', SReal' b')     -> compare (fromRational a') b'
    (SRational' a', SRational' b') -> compare a' b'
    (SRational' a', SInteger' b')  -> compare a' (fromInteger b')

    (SInteger' a', SReal' b')      -> compare (fromInteger a') b'
    (SInteger' a', SRational' b')  -> compare (fromInteger a') b'
    (SInteger' a', SInteger' b')   -> compare a' b'


instance Eq SchemeNumber where
  (==) a b = case (a, b) of
    (Complex' a', Complex' b')  -> a' == b'
    (Complex' a', Real' b')     -> a' == toComplex b'
    (Real' a',    Complex' b')  -> toComplex a' == b'
    (Real' a',    Real' b')     -> a' == b'

    where
      toComplex :: SchemeReal -> SchemeComplex
      toComplex val = val :+ SInteger' 0




data SchemeVal
  = SSymbol String
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
  | SProc
    { procParams    :: [String]
    , procVarParam  :: Maybe String
    , procBody      :: [SchemeVal]
    , procClosure   :: Env
    }

instance Show SchemeVal where
  show = \case
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


data SchemeError
  = NumArgs Integer [SchemeVal]
  | TypeMismatch String SchemeVal
  | ParseError ParseError
  | BadForm String SchemeVal
  | UnboundVar String
  -- | InternalError String
  | Default String

instance Show SchemeError where
  show = \case
    UnboundVar varname ->
      "Unbound variable: " ++ varname
    BadForm msg form ->
      msg ++ ": " ++ show form
    NumArgs expected found ->
      "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
    TypeMismatch expected found ->
      "Invalid type: expected " ++ expected ++ ", found " ++ show found
    ParseError err ->
      "Parse error at " ++ show err
    -- InternalError msg ->
    --   "Internal error (read: wtf this shouldn't be possible): " ++ show msg
    Default msg ->
      "Error: " ++ msg



type Env = IORef [(String, IORef SchemeVal)]

type SchemeValOrError = Either SchemeError SchemeVal
type IOSchemeValOrError = ExceptT SchemeError IO SchemeVal
type IONilOrError = ExceptT SchemeError IO ()
