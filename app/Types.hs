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
import Data.Complex (Complex, Complex((:+)), conjugate)

{- naming convention:
     SchemeFoo   | a type and (sometimes) its constructor
     SchemeFoo'  | a constructor wrapping SchemeFoo
     SFoo'       | a (most granular) constructor
     SFoo        | pattern synonym for convenient access to SFoo'
-}

---------------------------------------------------------------------------------
-- types
---------------------------------------------------------------------------------

data SchemeReal
  = SReal' Float
  | SRational' Rational
  | SInteger' Integer

newtype SchemeComplex = SComplex' (Complex SchemeReal)
  deriving Eq

-- TODO: #e / #i
data SchemeNumber
  = SchemeReal SchemeReal
  | SchemeComplex SchemeComplex

data SchemeVal
  = SSymbol String
  | SBool Bool
  | SChar Char
  | SString String
  | SchemeNumber SchemeNumber
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

data SchemeError
  = NumArgs Integer [SchemeVal]
  | TypeMismatch String SchemeVal
  | ParseError ParseError
  | BadForm String SchemeVal
  | UnboundVar String
  -- | InternalError String
  | Default String

type SchemeValOrError = Either SchemeError SchemeVal
type IOSchemeValOrError = ExceptT SchemeError IO SchemeVal
type IONilOrError = ExceptT SchemeError IO ()

type Env = IORef [(String, IORef SchemeVal)]

---------------------------------------------------------------------------------
-- patterns
---------------------------------------------------------------------------------

pattern SComplex :: Complex SchemeReal -> SchemeNumber
pattern SComplex val = SchemeComplex (SComplex' val)

pattern SReal :: Float -> SchemeNumber
pattern SReal val = SchemeReal (SReal' val)

pattern SRational :: Rational -> SchemeNumber
pattern SRational val = SchemeReal (SRational' val)

pattern SInteger :: Integer -> SchemeNumber
pattern SInteger val = SchemeReal (SInteger' val)

{-# COMPLETE SComplex, SReal, SRational, SInteger #-}

---------------------------------------------------------------------------------
-- instantiations
---------------------------------------------------------------------------------

---- SchemeReal ----

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
    SReal' val -> SReal' $ 1 / val
    SRational' val -> SRational' $ 1 / val
    SInteger' val -> SRational' $ 1 % val


---- SchemeComplex ----

instance Show SchemeComplex where
  show (SComplex' (real :+ imag)) =
    let maybePlus = if imag > SInteger' 0 then "+" else ""
    in show real ++ maybePlus ++ show imag ++ "i"


complexMag :: SchemeReal -> SchemeReal -> SchemeReal
complexMag real imag =
  SReal' $ sqrt $ toFloat real ** 2 + toFloat imag ** 2
  where
    toFloat :: SchemeReal -> Float
    toFloat = \case
      SInteger' val -> fromInteger val
      SReal' val -> val
      SRational' val -> fromRational  val

toComplex :: SchemeReal -> SchemeComplex
toComplex val = SComplex' $ val :+ SInteger' 0

-- alternatively, could implement RealFloat SchemeReal
instance Num SchemeComplex where
  (+) (SComplex' (aReal :+ aImag)) (SComplex' (bReal :+ bImag)) =
    let real = aReal + bReal
        imag = aImag + bImag
    in SComplex' $ real :+ imag

  (*) (SComplex' (aReal :+ aImag)) (SComplex' (bReal :+ bImag)) =
    let real = aReal * bReal - aImag * bImag
        imag = aReal * bImag + bReal * aImag
    in SComplex' $ real :+ imag

  abs (SComplex' (real :+ imag)) =
    SComplex' $ complexMag real imag :+ SReal' 0

  signum (SComplex' (real :+ imag)) =
    let mag = complexMag real imag
    in SComplex' $ real/mag :+ imag/mag

  fromInteger val = toComplex $ SInteger' val

  negate (SComplex' (real :+ imag)) =
    SComplex' $ negate real :+ negate imag

instance Fractional SchemeComplex where
  fromRational val = toComplex $ SRational' val
  recip (SComplex' val) =
    let numerReal :+ numerImag = conjugate val
        SComplex' (denom :+ _) = SComplex' val * SComplex' (conjugate val)
    in SComplex' $ (numerReal / denom) :+ (numerImag / denom)


---- SchemeNumber ----

instance Show SchemeNumber where
  show = \case
    SchemeReal val -> show val
    SchemeComplex val -> show val

instance Eq SchemeNumber where
  (==) a b = case (a, b) of
    (SchemeComplex a', SchemeComplex b')  -> a' == b'
    (SchemeComplex a', SchemeReal b')     -> a' == toComplex b'
    (SchemeReal a',    SchemeComplex b')  -> toComplex a' == b'
    (SchemeReal a',    SchemeReal b')     -> a' == b'

instance Num SchemeNumber where
  (+) a b = case (a, b) of
    (SchemeComplex a', SchemeComplex b')  -> SchemeComplex $ a' + b'
    (SchemeComplex a', SchemeReal b')     -> SchemeComplex $ a' + toComplex b'
    (SchemeReal a',    SchemeComplex b')  -> SchemeComplex $ toComplex a' + b'
    (SchemeReal a',    SchemeReal b')     -> SchemeReal $ a' + b'

  (*) a b = case (a, b) of
    (SchemeComplex a', SchemeComplex b')  -> SchemeComplex $ a' * b'
    (SchemeComplex a', SchemeReal b')     -> SchemeComplex $ a' * toComplex b'
    (SchemeReal a',    SchemeComplex b')  -> SchemeComplex $ toComplex a' * b'
    (SchemeReal a',    SchemeReal b')     -> SchemeReal $ a' * b'

  abs = \case
    SchemeComplex val -> SchemeComplex $ abs val
    SchemeReal val -> SchemeReal $ abs val

  signum = \case
    SchemeComplex val -> SchemeComplex $ signum val
    SchemeReal val -> SchemeReal $ signum val

  negate = \case
    SchemeComplex val -> SchemeComplex $ negate val
    SchemeReal val -> SchemeReal $ negate val

  fromInteger val = SInteger val


instance Fractional SchemeNumber where
  fromRational val = SRational val

  recip = \case
    SchemeReal val -> SchemeReal $ recip val
    SchemeComplex val -> SchemeComplex $ recip val


---- SchemeVal ----

unwordsList :: [SchemeVal] -> String
unwordsList = unwords . map show

instance Show SchemeVal where
  show = \case
    SSymbol val           -> val
    SBool True            -> "#t"
    SBool False           -> "#f"
    SChar val             -> "#\\" ++ [val] -- TODO: named chars
    SString val           -> "\"" ++ val ++ "\""
    SchemeNumber val           -> show val
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


---- SchemeError ----

instance Show SchemeError where
  show = \case
    UnboundVar varname ->
      "Unbound variable: " ++ varname
    BadForm msg form ->
      msg ++ ": " ++ show form
    NumArgs expected found ->
      "Expected " ++ show expected ++ " args; found values " ++ show found
    TypeMismatch expected found ->
      "Invalid type: expected " ++ expected ++ ", found " ++ show found
    ParseError err ->
      "Parse error at " ++ show err
    -- InternalError msg ->
    --   "Internal error (read: wtf this shouldn't be possible): " ++ show msg
    Default msg ->
      "Error: " ++ msg
