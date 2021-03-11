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
import Data.Ratio (numerator, denominator, (%))
import Data.Complex (Complex, Complex((:+)))

unwordsList :: [SchemeVal] -> String
unwordsList = unwords . map show

{- naming convention:
     SchemeFoo | a type
     SFoo'     | a real constructor
     SFoo      | pattern synonym
     Foo'      | a real constructor that we try to forget about
-}


data SchemeReal
  = SReal' Float
  | SRational' Rational
  | SInteger' Integer


newtype SchemeComplex = SComplex' (Complex SchemeReal)
  deriving Eq


data SchemeNumber
  = Real' SchemeReal
  | Complex' SchemeComplex

pattern SComplex :: Complex SchemeReal -> SchemeNumber
pattern SComplex val = Complex' (SComplex' val)

pattern SReal :: Float -> SchemeNumber
pattern SReal val = Real' (SReal' val)

pattern SRational :: Rational -> SchemeNumber
pattern SRational val = Real' (SRational' val)

pattern SInteger :: Integer -> SchemeNumber
pattern SInteger val = Real' (SInteger' val)

{-# COMPLETE SComplex, SReal, SRational, SInteger #-}

instance Show SchemeReal where
  show = \case
    SInteger'  val -> show val
    SReal'     val -> show val
    SRational' val ->
      let numer = show $ numerator val
          denom = show $ denominator val
      in numer ++ "/" ++ denom

instance Eq SchemeReal where
  (==) a b = case (a, b) of
    (SReal' a',     SReal' b')     -> a' == b'
    (SReal' a',     SRational' b') -> a' == fromRational b'
    (SReal' a',     SInteger' b')  -> a' == fromInteger b'

    (SRational' a', SReal' b')     -> fromRational a' == b'
    (SRational' a', SRational' b') -> a' == b'
    (SRational' a', SInteger' b')  -> a' == fromInteger b'

    (SInteger' a',  SReal' b')     -> fromInteger a' == b'
    (SInteger' a',  SRational' b') -> fromInteger a' == b'
    (SInteger' a',  SInteger' b')  -> a' == b'

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

instance Num SchemeReal where
  (+) a b = case (a, b) of
    (SReal' a', SReal' b')         -> SReal' $ a' + b'
    (SReal' a', SRational' b')     -> SReal' $ a' + fromRational b'
    (SReal' a', SInteger' b')      -> SReal' $ a' + fromInteger b'

    (SRational' a', SReal' b')     -> SReal' $ fromRational a' + b'
    (SRational' a', SRational' b') -> SRational' $ a' + b'
    (SRational' a', SInteger' b')  -> SRational' $ a' + fromInteger b'

    (SInteger' a', SReal' b')      -> SReal' $ fromInteger a' + b'
    (SInteger' a', SRational' b')  -> SRational' $ fromInteger a' + b'
    (SInteger' a', SInteger' b')   -> SInteger' $ a' + b'

  -- can * and + be deduped somehow? exactly the same except
  -- for the operator...
  (*) a b = case (a, b) of
    (SReal' a', SReal' b')         -> SReal' $ a' * b'
    (SReal' a', SRational' b')     -> SReal' $ a' * fromRational b'
    (SReal' a', SInteger' b')      -> SReal' $ a' * fromInteger b'

    (SRational' a', SReal' b')     -> SReal' $ fromRational a' * b'
    (SRational' a', SRational' b') -> SRational' $ a' * b'
    (SRational' a', SInteger' b')  -> SRational' $ a' * fromInteger b'

    (SInteger' a', SReal' b')      -> SReal' $ fromInteger a' * b'
    (SInteger' a', SRational' b')  -> SRational' $ fromInteger a' * b'
    (SInteger' a', SInteger' b')   -> SInteger' $ a' * b'

  -- again: can these be deduped?
  abs = \case
    SReal' val'     -> SReal' $ abs val'
    SRational' val' -> SRational' $ abs val'
    SInteger' val'  -> SInteger' $ abs val'

  signum = \case
    SReal' val'     -> SReal' $ signum val'
    SRational' val' -> SRational' $ signum val'
    SInteger' val'  -> SInteger' $ signum val'

  negate = \case
    SReal' val'     -> SReal' $ negate val'
    SRational' val' -> SRational' $ negate val'
    SInteger' val'  -> SInteger' $ negate val'

  fromInteger = SInteger'

instance Fractional SchemeReal where
  fromRational val = SRational' val
  recip = \case
    SReal' val -> SReal' $ 1/val
    SRational' val -> SRational' $ 1/val
    SInteger' val -> SRational' $ 1%val


instance Show SchemeComplex where
  show (SComplex' (real :+ imag)) =
    let maybePlus = if imag > SInteger' 0 then "+" else ""
    in show real ++ maybePlus ++ show imag ++ "i"

toFloat :: SchemeReal -> Float
toFloat = \case
  SInteger' val -> fromInteger val
  SReal' val -> val
  SRational' val -> fromRational  val

complexMag :: SchemeReal -> SchemeReal -> SchemeReal
complexMag real imag =
  let realF = toFloat real
      imagF = toFloat imag
  in SReal' $ sqrt $ realF ** 2 + imagF ** 2

-- alternatively, could implement RealFloat SchemeReal
instance Num SchemeComplex where
  (+) (SComplex' (aReal :+ aImag)) (SComplex' (bReal :+ bImag)) =
    let real = aReal + bReal
        imag = aImag + bImag
    in SComplex' $ real :+ imag

  (*) (SComplex' (aReal :+ aImag)) (SComplex' (bReal :+ bImag)) =
    let real = (aReal * bImag) + (bImag * aReal)
        imag = (aReal * bReal) - (aImag * bImag)
    in SComplex' $ real :+ imag

  abs (SComplex' (real :+ imag)) =
    SComplex' $ complexMag real imag :+ SReal' 0

  signum (SComplex' (real :+ imag)) =
    let mag = complexMag real imag
    in SComplex' $ real/mag :+ imag/mag

  fromInteger val = SComplex' $ SInteger' val :+ SInteger' 0

  negate (SComplex' (real :+ imag)) =
    SComplex' $ negate real :+ negate imag

toComplex :: SchemeReal -> SchemeComplex
toComplex val = SComplex' $ val :+ SInteger' 0

instance Show SchemeNumber where
  show = \case
    Real' val -> show val
    Complex' val -> show val

instance Eq SchemeNumber where
  (==) a b = case (a, b) of
    (Complex' a', Complex' b')  -> a' == b'
    (Complex' a', Real' b')     -> a' == toComplex b'
    (Real' a',    Complex' b')  -> toComplex a' == b'
    (Real' a',    Real' b')     -> a' == b'


-- need SchemeComplex implementation first
instance Num SchemeNumber where
  (+) a b = case (a, b) of
    (Complex' a', Complex' b')  -> Complex' $ a' + b'
    (Complex' a', Real' b')     -> Complex' $ a' + toComplex b'
    (Real' a',    Complex' b')  -> Complex' $ toComplex a' + b'
    (Real' a',    Real' b')     -> Real' $ a' + b'

  (*) a b = case (a, b) of
    (Complex' a', Complex' b')  -> Complex' $ a' * b'
    (Complex' a', Real' b')     -> Complex' $ a' * toComplex b'
    (Real' a',    Complex' b')  -> Complex' $ toComplex a' * b'
    (Real' a',    Real' b')     -> Real' $ a' * b'

  abs = \case
    Complex' val -> Complex' $ abs val
    Real' val -> Real' $ abs val

  signum = \case
    Complex' val -> Complex' $ signum val
    Real' val -> Real' $ signum val

  negate = \case
    Complex' val -> Complex' $ negate val
    Real' val -> Real' $ negate val

  fromInteger val = SInteger val



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
