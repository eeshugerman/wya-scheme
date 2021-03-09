{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
module Types
  ( ComplexComponent (..)
  , SchemeNumber (..)
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

showRational :: Rational -> String
showRational val = show (numerator val) ++ "/" ++ show (denominator val)

data ComplexComponent
  = CCReal Float
  | CCRational Rational
  | CCInteger Integer

instance Eq ComplexComponent where
  (==) a b = case (a, b) of
    (CCReal a', CCReal b')     -> a' == b'
    (CCReal a', CCRational b') -> a' == fromRational b'
    (CCReal a', CCInteger b')  -> a' == fromInteger b'

    (CCRational _,  CCReal _)      -> b == a
    (CCRational a', CCRational b') -> a' == b'
    (CCRational a', CCInteger b')  -> a' == fromInteger b'

    (CCInteger _,  CCReal _)     -> b == a
    (CCInteger _,  CCRational _) -> b == a
    (CCInteger a', CCInteger b') -> a' == b'


data SchemeReal
  = SReal Float
  | SRational Rational
  | SInteger Integer

data SchemeNumber
  = NonComplex SchemeReal
  | Complex (Complex SchemeReal)

pattern SComplex' :: Complex SchemeReal -> SchemeNumber
pattern SComplex' val <- Complex val

pattern SReal' :: Float -> SchemeNumber
pattern SReal' val <- NonComplex (SReal val)

pattern SRational' :: Rational -> SchemeNumber
pattern SRational' val <- NonComplex (SRational val)

pattern SInteger' :: Integer -> SchemeNumber
pattern SInteger' val <- NonComplex (SInteger val)


instance Show SchemeNumber where
  show = \case
    SInteger' val            -> show val
    SRational' val           -> showRational val
    SReal' val               -> show val
    SComplex' (real :+ imag) -> showComplexComponent False real ++
                                showComplexComponent True imag ++ "i"
    where
      showComplexComponent withPlusSign = \case
        SInteger val  -> maybePlusSign val ++ show val
        SRational val -> maybePlusSign val ++ showRational val
        SReal val     -> maybePlusSign val ++ show val
        where
          maybePlusSign val =
            if withPlusSign && (val >= 0)
            then "+"
            else ""

instance Eq SchemeNumber where
  (==) a b = case (a, b) of
    (SComplex' a', SComplex' b')   -> a' == b'
    (SComplex' a', SInteger' b')   -> a' == CCInteger b' :+ CCInteger 0
    (SComplex' a', SRational' b')  -> a' == CCRational b' :+ CCInteger 0
    (SComplex' a', SReal' b')      -> a' == CCReal b' :+ CCInteger 0
    (_,           SComplex' _)    -> b == a

    (SReal a', SReal b')         -> a' == b'
    (SReal a', SRational b')     -> a' == fromRational b'
    (SReal a', SInteger b')      -> a' == fromInteger b'

    (SRational _,  SReal _)      -> b == a
    (SRational a', SRational b') -> a' == b'
    (SRational a', SInteger b')  -> a' == fromInteger b'

    (SInteger _,  SReal _)       -> b == a
    (SInteger _,  SRational _)   -> b == a
    (SInteger a', SInteger b')   -> a' == b'


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
