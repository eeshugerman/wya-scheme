{-# LANGUAGE RankNTypes #-}
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
  , SchemeVal'(..)
  , SchemeVal
    ( ..
    , SSymbol
    , SBool
    , SChar
    , SString
    , SchemeNumber
    , SList
    , SVector
    , SDottedList
    , SPort
    , SPrimativeProc
    , SIOProc
    , SProc
    , SMacro
    , SError
    , Quote
    , Quasiquote
    , Unquote
    , UnquoteSplicing
    )
  , SchemeError (..)
  , SchemeValOrError
  , IOSchemeValOrError
  , IONilOrError
  , Env
  , CallableSpec (..)
  ) where

import qualified Data.Array as A
import Text.Parsec (ParseError)
import Control.Monad.Except (ExceptT)
import GHC.IO.Handle (Handle)
import Data.Ratio (numerator, denominator, (%))
import Data.Complex (Complex, Complex((:+)), conjugate)
import qualified Data.Map.Strict as Map
import Data.Unique as U
import Data.IORef (IORef)

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
  = SchemeReal    SchemeReal
  | SchemeComplex SchemeComplex

data CallableSpec
  = CallableSpec
  { cClosure   :: Env
  , cParams    :: [String]
  , cVarParam  :: Maybe String
  , cBody      :: [SchemeVal]
  }

type LocationTag = Maybe U.Unique

data SchemeVal'
  = SSymbol'        String
  | SBool'          Bool
  | SChar'          Char
  | SString'        String
  | SchemeNumber'   SchemeNumber
  | SList'          [SchemeVal]
  | SVector'        (A.Array Int SchemeVal)
  | SDottedList'    [SchemeVal] SchemeVal
  | SPort'          Handle
  | SPrimativeProc' ([SchemeVal] -> SchemeValOrError)
  | SIOProc'        ([SchemeVal] -> IOSchemeValOrError)
  | SProc'          CallableSpec
  | SMacro'         CallableSpec
  | SError'         SchemeError

data SchemeVal = SchemeVal LocationTag SchemeVal'

data SchemeError
  = NumArgs      Integer [SchemeVal]
  | TypeMismatch String SchemeVal
  | ParseError   ParseError
  | BadForm      String SchemeVal
  | UnboundVar   String

type SchemeValOrError   = Either SchemeError SchemeVal
type IOSchemeValOrError = ExceptT SchemeError IO SchemeVal
type IONilOrError       = ExceptT SchemeError IO ()

-- TODO: rename to EnvRef
type Env = IORef (Map.Map String (IORef SchemeVal))

---------------------------------------------------------------------------------
-- patterns
---------------------------------------------------------------------------------

pattern SSymbol :: String -> SchemeVal
pattern SSymbol val <- SchemeVal _ (SSymbol' val)
  where SSymbol val = SchemeVal Nothing (SSymbol' val)

pattern SBool :: Bool -> SchemeVal
pattern SBool val <- SchemeVal _ (SBool' val)
  where SBool val = SchemeVal Nothing (SBool' val)

pattern SChar :: Char -> SchemeVal
pattern SChar val <- SchemeVal _ (SChar' val)
  where SChar val = SchemeVal Nothing (SChar' val)

pattern SString :: String -> SchemeVal
pattern SString val <- SchemeVal _ (SString' val)
  where SString val = SchemeVal Nothing (SString' val)

pattern SchemeNumber :: SchemeNumber -> SchemeVal
pattern SchemeNumber val <- SchemeVal _ (SchemeNumber' val)
  where SchemeNumber val = SchemeVal Nothing (SchemeNumber' val)

pattern SList :: [SchemeVal] -> SchemeVal
pattern SList val <- SchemeVal _ (SList' val)
  where SList val = SchemeVal Nothing (SList' val)

pattern SVector :: A.Array Int SchemeVal -> SchemeVal
pattern SVector val <- SchemeVal _ (SVector' val)
  where SVector val = SchemeVal Nothing (SVector' val)

pattern SDottedList :: [SchemeVal] -> SchemeVal -> SchemeVal
pattern SDottedList bleep bloop <- SchemeVal _ (SDottedList' bleep bloop)
  where SDottedList bleep bloop = SchemeVal Nothing (SDottedList' bleep bloop)

pattern SPort :: Handle -> SchemeVal
pattern SPort val <- SchemeVal _ (SPort' val)
  where SPort val = SchemeVal Nothing (SPort' val)

pattern SPrimativeProc :: ([SchemeVal] -> SchemeValOrError) -> SchemeVal
pattern SPrimativeProc val <- SchemeVal _ (SPrimativeProc' val)
  where SPrimativeProc val = SchemeVal Nothing (SPrimativeProc' val)

pattern SIOProc :: ([SchemeVal] -> IOSchemeValOrError) -> SchemeVal
pattern SIOProc val <- SchemeVal _ (SIOProc' val)
  where SIOProc val = SchemeVal Nothing (SIOProc' val)

pattern SProc :: CallableSpec -> SchemeVal
pattern SProc val <- SchemeVal _ (SProc' val)
  where SProc val = SchemeVal Nothing (SProc' val)

pattern SMacro :: CallableSpec -> SchemeVal
pattern SMacro val <- SchemeVal _ (SMacro' val)
  where SMacro val = SchemeVal Nothing (SMacro' val)

pattern SError :: SchemeError -> SchemeVal
pattern SError val <- SchemeVal _ (SError' val)
  where SError val = SchemeVal Nothing (SError' val)

{-# COMPLETE SSymbol, SBool, SChar, SString, SchemeNumber,
             SList, SVector, SDottedList,
             SPort,
             SPrimativeProc, SIOProc, SProc, SMacro,
             SError #-}

-- numbers
pattern SComplex :: Complex SchemeReal -> SchemeNumber
pattern SComplex val = SchemeComplex (SComplex' val)

pattern SReal :: Float -> SchemeNumber
pattern SReal val = SchemeReal (SReal' val)

pattern SRational :: Rational -> SchemeNumber
pattern SRational val = SchemeReal (SRational' val)

pattern SInteger :: Integer -> SchemeNumber
pattern SInteger val = SchemeReal (SInteger' val)

{-# COMPLETE SComplex, SReal, SRational, SInteger #-}

-- quoting stuff
pattern Quote :: SchemeVal -> SchemeVal
pattern Quote val = SList [SSymbol "quote", val]

pattern Quasiquote :: SchemeVal -> SchemeVal
pattern Quasiquote val = SList [SSymbol "quasiquote", val]

pattern Unquote :: SchemeVal -> SchemeVal
pattern Unquote val = SList [SSymbol "unquote", val]

pattern UnquoteSplicing :: SchemeVal -> SchemeVal
pattern UnquoteSplicing val = SList [SSymbol "unquote-splicing", val]

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

schemeRealRawBinOp
  :: (forall a. (Eq a, Ord a) => a -> a -> b)
  -> (SchemeReal -> SchemeReal -> b)
schemeRealRawBinOp op a b = case (a, b) of
    (SReal' a',     SReal' b')     -> a' `op` b'
    (SReal' a',     SRational' b') -> a' `op` fromRational b'
    (SReal' a',     SInteger' b')  -> a' `op` fromInteger b'

    (SRational' a', SReal' b')     -> fromRational a' `op` b'
    (SRational' a', SRational' b') -> a' `op` b'
    (SRational' a', SInteger' b')  -> a' `op` fromInteger b'

    (SInteger' a',  SReal' b')     -> fromInteger a' `op` b'
    (SInteger' a',  SRational' b') -> fromInteger a' `op` b'
    (SInteger' a',  SInteger' b')  -> a' `op` b'

instance Eq SchemeReal where
  (==) = schemeRealRawBinOp (==)

instance Ord SchemeReal where
  compare = schemeRealRawBinOp compare

-- can this be defined in terms of schemeRealRawBinOp ?
schemeRealBinOp
  :: (forall a. Num a => a -> a -> a)
  -> (SchemeReal -> SchemeReal -> SchemeReal)
schemeRealBinOp op a b = case (a, b) of
    (SReal' a',     SReal' b')     -> SReal' $ a' `op` b'
    (SReal' a',     SRational' b') -> SReal' $ a' `op` fromRational b'
    (SReal' a',     SInteger' b')  -> SReal' $ a' `op` fromInteger b'

    (SRational' a', SReal' b')     -> SReal' $ fromRational a' `op` b'
    (SRational' a', SRational' b') -> SRational' $ a' `op` b'
    (SRational' a', SInteger' b')  -> SRational' $ a' `op` fromInteger b'

    (SInteger' a',  SReal' b')     -> SReal' $ fromInteger a' `op` b'
    (SInteger' a',  SRational' b') -> SRational' $ fromInteger a' `op` b'
    (SInteger' a',  SInteger' b')  -> SInteger' $ a' `op` b'

schemeRealMonoOp
  :: (forall a. Num a => a -> a)
  -> (SchemeReal -> SchemeReal)
schemeRealMonoOp op = \case
  SReal' val     -> SReal' $ op val
  SRational' val -> SRational' $ op val
  SInteger' val  -> SInteger' $ op val

instance Num SchemeReal where
  (+) = schemeRealBinOp (+)
  (*) = schemeRealBinOp (*)
  abs = schemeRealMonoOp abs
  signum = schemeRealMonoOp signum
  negate = schemeRealMonoOp negate
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

schemeNumberBinOp
  :: (forall a. Num a => a -> a -> a)
  -> (SchemeNumber -> SchemeNumber -> SchemeNumber)
schemeNumberBinOp op a b = case (a, b) of
    (SchemeComplex a', SchemeComplex b')  -> SchemeComplex $ a' `op` b'
    (SchemeComplex a', SchemeReal b')     -> SchemeComplex $ a' `op` toComplex b'
    (SchemeReal a',    SchemeComplex b')  -> SchemeComplex $ toComplex a' `op` b'
    (SchemeReal a',    SchemeReal b')     -> SchemeReal $ a' `op` b'

schemeNumberMonoOp
  :: (forall a. (Num a, Fractional a) => a -> a)
  -> (SchemeNumber -> SchemeNumber)
schemeNumberMonoOp op = \case
  SchemeComplex val -> SchemeComplex $ op val
  SchemeReal val -> SchemeReal $ op val

instance Num SchemeNumber where
  (+) = schemeNumberBinOp (+)
  (*) = schemeNumberBinOp (*)
  abs = schemeNumberMonoOp abs
  signum = schemeNumberMonoOp signum
  negate = schemeNumberMonoOp negate
  fromInteger val = SInteger val

instance Fractional SchemeNumber where
  fromRational val = SRational val
  recip = schemeNumberMonoOp recip

---- SchemeVal ----

unwordsList :: [SchemeVal] -> String
unwordsList = unwords . map show

instance Show SchemeVal where
  show = \case
    SSymbol val           -> val
    SBool True            -> "#t"
    SBool False           -> "#f"
    SChar ' '             -> "#\\space"
    SChar '\n'            -> "#\\newline"
    SChar '\t'            -> "#\\tab"
    SChar val             -> "#\\" ++ [val]
    SString val           -> "\"" ++ val ++ "\""
    SchemeNumber val      -> show val
    Quote val             -> "'" ++ show val
    Quasiquote val        -> "`" ++ show val
    Unquote val           -> "," ++ show val
    UnquoteSplicing val   -> ",@" ++ show val
    SList val             -> "(" ++ unwordsList val ++ ")"
    SVector val           -> "#(" ++ unwordsList (A.elems val) ++ ")"
    SDottedList begin end -> "(" ++ unwordsList begin ++ " . " ++ show end ++ ")"
    SPort _               -> "<IO port>"
    SPrimativeProc _      -> "<primitive>"
    SIOProc _             -> "<IO primitive>"
    SMacro {}             -> "<macro>"
    SProc CallableSpec {cParams=params, cVarParam=varParam} ->
      "(lambda (" ++ unwords params ++ showVarArgs ++ ") ...)"
      where
        showVarArgs = case varParam of
          Nothing -> ""
          Just val  -> " . " ++ val
    SError err -> "<" ++ show err ++ ">"


---- SchemeError ----

instance Show SchemeError where
  show val = "Error: " ++ case val of
    UnboundVar varname ->
      "Unbound variable: " ++ varname
    BadForm msg form ->
      msg ++ ": " ++ show form
    NumArgs expected found ->
      "Expected " ++ show expected ++ " arg(s); found value(s) " ++ show found
    TypeMismatch expected found ->
      "Invalid type: expected " ++ expected ++ ", found " ++ show found
    ParseError err ->
      "Parse error at " ++ show err
