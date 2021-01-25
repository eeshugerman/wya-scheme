module Primitives ( primitives ,ioPrimitives) where

import Control.Monad.Except ( throwError )

import Types
  ( IOSchemeValOrError
  , SchemeNumber (..)
  , SchemeVal (..)
  , SchemeError (..)
  , SchemeValOrError
  )
import Eval (eval, apply)

import Data.Complex (Complex((:+)), realPart, imagPart)
import Control.Monad (zipWithM)

ioPrimitives :: [(String, SchemeVal)]
ioPrimitives = [ ("apply", SIOProc wrappedApply)
               -- , ("eval",  SIOProc $ wrap eval) -- need env type
               ]
  where
    wrappedApply [proc', SList args] = apply proc' args
    -- TODO: remaining cases

primitives :: [(String, SchemeVal)]
primitives = map (\ (name, f) -> (name, SPrimativeProc f))
  [ ("+",         numericFoldableOp add)
  , ("-",         numericFoldableOp subtract_)
  , ("*",         numericFoldableOp multiply)
  , ("/",         numericFoldableOp divide)
  -- , ("mod",       numericFoldableOp mod)
  -- , ("quotient",  numericFoldableOp quot)
  -- , ("remainder", numericFoldableOp rem)

  , ("symbol?",      isSymbol)
  , ("boolean?",     isBoolean)
  , ("character?",   isCharacter)
  , ("string?",      isString)
  , ("number?",      isNumber)
  , ("integer?",     isInteger)
  , ("rational?",    isRational)
  , ("real?",        isReal)
  , ("complex?",     isComplex)
  , ("list?",        isList)
  , ("vector?",      isVector)

  , ("symbol->string", symbolToString)
  , ("string->symbol", stringToSymbol)

  , ("=", numBoolBinOp (==))
  , ("<", numBoolBinOp (<))
  , (">", numBoolBinOp (>))
  , ("/=", numBoolBinOp (/=))
  , (">=", numBoolBinOp (>=))
  , ("<=", numBoolBinOp (<=))
  , ("and", boolBoolBinOp (&&))
  , ("or", boolBoolBinOp (||))
  , ("string=?", strBoolBinOp (==))
  , ("string<?", strBoolBinOp (<))
  , ("string>?", strBoolBinOp (>))
  , ("string<=?", strBoolBinOp (<=))
  , ("string>=?", strBoolBinOp (>=))

  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)

  , ("eqv?", eqv)
  , ("equal?", equal)
  ]



------------------------------------------
-- helpers (not actual scheme primitives)
------------------------------------------
numericFoldableOp
  :: (SchemeNumber -> SchemeNumber -> SchemeNumber)
  -> ([SchemeVal] -> SchemeValOrError )
numericFoldableOp _ []              = throwError $ NumArgs 2 []
numericFoldableOp _ singleArg@[_]   = throwError $ NumArgs 2 singleArg
numericFoldableOp op args           = foldl1M wrappedOp args
  where
    foldl1M f (x:xs) = foldlM f x xs
    foldl1M _ [] = error "internal error in foldl1M"

    foldlM _ acc [] = return acc
    foldlM f acc (x:xs) = f acc x >>= \ acc' -> foldlM f acc' xs

    wrappedOp :: SchemeVal -> SchemeVal -> SchemeValOrError
    wrappedOp (SNumber a) (SNumber b) = return $ SNumber $ op a b
    wrappedOp a              (SNumber _) = throwError $ TypeMismatch "number" a
    wrappedOp (SNumber _) b              = throwError $ TypeMismatch "number" b
    wrappedOp a              _              = throwError $ TypeMismatch "number" a

boolBinOp
  :: (SchemeVal -> Either SchemeError a)  -- unpacker
  -> (a -> a -> Bool)                 -- op
  -> [SchemeVal]                        -- args
  -> SchemeValOrError                   -- result
boolBinOp unpacker op args =
  if length args /= 2
  then throwError $ NumArgs 2 args
  else do left <- unpacker $ args !! 0
          right <- unpacker $ args !! 1
          return $ SBool $ left `op` right

type BoolBinOpBuilder a
  = (a -> a -> Bool)
  -> [SchemeVal]
  -> SchemeValOrError

numBoolBinOp :: BoolBinOpBuilder Float
numBoolBinOp = boolBinOp unpackNum
  where
    unpackNum :: SchemeVal -> Either SchemeError Float
    unpackNum (SNumber num) = case num of
      SInteger val          -> return $ fromInteger val
      SRational numer denom -> return (fromInteger numer / fromInteger denom)
      SReal val             -> return val
      SComplex _ _          -> throwError $
        Default "operation not implemented for complex numbers" -- TODO
    unpackNum nonNum = throwError $ TypeMismatch "number" nonNum

boolBoolBinOp :: BoolBinOpBuilder Bool
boolBoolBinOp = boolBinOp unpackBool
  where
    unpackBool :: SchemeVal -> Either SchemeError Bool
    unpackBool (SBool val) = return val
    unpackBool nonBool = throwError $ TypeMismatch "boolean" nonBool

strBoolBinOp :: BoolBinOpBuilder String
strBoolBinOp = boolBinOp unpackString
  where
    unpackString :: SchemeVal -> Either SchemeError String
    unpackString (SString val) = return val
    unpackString nonString = throwError $ TypeMismatch "string" nonString


-----------------------------------
add :: SchemeNumber  -> SchemeNumber -> SchemeNumber
-----------------------------------

add (SInteger a)              (SInteger b)              = SInteger (a + b)
add (SInteger a)              (SReal b)                 = SReal (fromInteger a + b)
add (SInteger a)              (SRational bNumer bDenom) = SRational ((a * bDenom) + bNumer) bDenom
add a@(SInteger _)            (SComplex bReal bImag)    = SComplex (add a bReal) bImag

add a@(SReal _)               b@(SInteger _)            = add b a
add (SReal a)                 (SReal b)                 = SReal (a + b)
add (SReal a)                 (SRational bNumer bDenom) = SReal (a + (fromInteger bNumer / fromInteger bDenom))
add a@(SReal _)               (SComplex bReal bImag)    = SComplex (add a bReal) bImag

add a@(SRational _ _)         b@(SInteger _)            = add b a
add a@(SRational _ _)         b@(SReal _)               = add b a
add (SRational an ad)         (SRational  bn bd)        = addRationals an ad bn bd
add a@(SRational _ _)         (SComplex bReal bImag)    = SComplex (add a bReal) bImag

add a@(SComplex _ _)          b@(SInteger _)            = add b a
add a@(SComplex _ _)          b@(SReal _)               = add b a
add a@(SComplex _ _)          b@(SRational _ _)         = add b a
add (SComplex aReal aImag)    (SComplex bReal bImag)    = SComplex (add aReal bReal) (add aImag bImag)

addRationals
  :: Integer -> Integer -- a
  -> Integer -> Integer -- b
  -> SchemeNumber         -- res
addRationals aNumer aDenom bNumer bDenom =
  let numer = (aNumer * bDenom) + (bNumer * aDenom)
      denom = aDenom * bDenom
  in SRational numer denom

-----------------------------------------
multiply :: SchemeNumber -> SchemeNumber -> SchemeNumber
-----------------------------------------

multiply (SInteger a)              (SInteger b)              = SInteger (a * b)
multiply (SInteger a)              (SReal b)                 = SReal (fromInteger a * b)
multiply (SInteger a)              (SRational bNumer bDenom) = SRational (a * bNumer) bDenom
multiply a@(SInteger _)            (SComplex bReal bImag)    = SComplex (multiply a bReal) (multiply a bImag)

multiply a@(SReal _)               b@(SInteger _)            = multiply b a
multiply (SReal a)                 (SReal b)                 = SReal (a * b)
multiply (SReal a)                 (SRational bNumer bDenom) = SReal (a * (fromInteger bNumer / fromInteger bDenom))
multiply a@(SReal _)               (SComplex bReal bImag)    = SComplex (multiply a bReal) (multiply a bImag)

multiply a@(SRational _ _)         b@(SInteger _)            = multiply b a
multiply a@(SRational _ _)         b@(SReal _)               = multiply b a
multiply (SRational aNumer aDenom) (SRational bNumer bDenom) = SRational (aNumer * bNumer) (aDenom * bDenom)
multiply a@(SRational _ _)         (SComplex bReal bImag)    = SComplex (multiply a bReal) (multiply a bImag)

multiply a@(SComplex _ _)          b@(SInteger _)            = add b a
multiply a@(SComplex _ _)          b@(SReal _)               = add b a
multiply a@(SComplex _ _)          b@(SRational _ _)         = add b a
multiply (SComplex ar ai)          (SComplex br bi)          = multiplyComplexes ar ai br bi

multiplyComplexes
  :: SchemeNumber -> SchemeNumber  -- a
  -> SchemeNumber -> SchemeNumber  -- b
  -> SchemeNumber                -- res
multiplyComplexes aReal aImag bReal bImag =
  let real = add (multiply aReal bImag) (multiply bImag aReal)
      imag = subtract_ (multiply aReal bReal) (multiply aImag bImag)
  in SComplex real imag

-----------------------------------------
subtract_ :: SchemeNumber -> SchemeNumber -> SchemeNumber
-----------------------------------------

subtract_ a b = let negativeB = multiply (SInteger (-1)) b
                in add a negativeB

-----------------------------------------
divide :: SchemeNumber -> SchemeNumber -> SchemeNumber
-----------------------------------------

divide (SInteger a)       (SInteger b)              = SRational a b
divide (SInteger a)       (SReal b)                 = SReal (fromInteger a / b)
divide a@(SInteger _)     (SRational bNumer bDenom) = multiply a (SRational bDenom bNumer)
divide (SInteger a)       (SComplex bReal bImag)    = divideByComplex a bReal bImag
divide a                  b                         = multiply a (divide (SInteger 1) b)

divideByComplex :: Integer -> SchemeNumber -> SchemeNumber -> SchemeNumber
divideByComplex a bReal bImag =
  let aRealFloat = fromInteger a
      aImagFloat = 0.0
      bRealFloat = toFloat bReal
      bImagFloat = toFloat bImag
      res = (aRealFloat :+ aImagFloat) / (bRealFloat :+ bImagFloat)
  in SComplex (SReal $ realPart res) (SReal $ imagPart res)
  where
    toFloat :: SchemeNumber -> Float
    toFloat (SInteger val)             = fromInteger val
    toFloat (SReal val)                = val
    toFloat (SRational numer denom)    = fromInteger numer / fromInteger denom
    toFloat (SComplex _ _)             = error $ "internal error: nested complex number. "
                                                 ++ "this expression should not have parsed."

-----------------------------------------
-- type testing
-----------------------------------------

isSymbol :: [SchemeVal] -> SchemeValOrError
isSymbol [SSymbol _]  = return $ SBool True
isSymbol [_]          = return $ SBool False
isSymbol args         = throwError $ NumArgs 1 args

isBoolean :: [SchemeVal] -> SchemeValOrError
isBoolean [SBool _]   = return $ SBool True
isBoolean [_]         = return $ SBool False
isBoolean args        = throwError $ NumArgs 1 args

isCharacter :: [SchemeVal] -> SchemeValOrError
isCharacter [SChar _] = return $ SBool True
isCharacter [_]       = return $ SBool False
isCharacter args      = throwError $ NumArgs 1 args

isString :: [SchemeVal] -> SchemeValOrError
isString [SString _]  = return $ SBool True
isString [_]          = return $ SBool False
isString args         = throwError $ NumArgs 1 args

isList :: [SchemeVal] -> SchemeValOrError
isList [SList _]      = return $ SBool True
isList [_]            = return $ SBool False
isList args           = throwError $ NumArgs 1 args

isVector :: [SchemeVal] -> SchemeValOrError
isVector [SVector _]  = return $ SBool True
isVector [_]          = return $ SBool False
isVector args         = throwError $ NumArgs 1 args

-- num type stuff
-- todo: this is dumb, arguably. maybe don't be fancy.
data NumType = Number | Complex | Real | Rational | Integer
isNumType :: NumType -> SchemeNumber -> Bool
isNumType Number   _   = True
isNumType Complex  _   = True
isNumType Real     val = case val of SComplex _ _  -> False
                                     _             -> isNumType Complex val
isNumType Rational val = case val of SReal _       -> False
                                     _             -> isNumType Real val
isNumType Integer  val = case val of SRational _ _ -> False
                                     _             -> isNumType Rational val

isNumTypeWrapper :: NumType -> [SchemeVal] -> SchemeValOrError
isNumTypeWrapper numType [SNumber num] = return $ SBool $ isNumType numType num
isNumTypeWrapper _       [_]           = return $ SBool False
isNumTypeWrapper _       args          = throwError $ NumArgs 1 args

isNumber :: [SchemeVal] -> SchemeValOrError
isNumber = isNumTypeWrapper Number

isComplex :: [SchemeVal] -> SchemeValOrError
isComplex = isNumTypeWrapper Complex

isReal :: [SchemeVal] -> SchemeValOrError
isReal = isNumTypeWrapper Real

isRational :: [SchemeVal] -> SchemeValOrError
isRational = isNumTypeWrapper Rational

isInteger :: [SchemeVal] -> SchemeValOrError
isInteger = isNumTypeWrapper Integer

-----------------------------------------
-- type conversion
-----------------------------------------

symbolToString :: [SchemeVal] -> SchemeValOrError
symbolToString [SSymbol arg] = return $ SString arg
symbolToString [arg]         = throwError $ TypeMismatch "symbol" arg
symbolToString args          = throwError $ NumArgs 1 args

stringToSymbol :: [SchemeVal] -> SchemeValOrError
stringToSymbol [SString arg] = return $ SSymbol arg
stringToSymbol [arg]         = throwError $ TypeMismatch "string" arg
stringToSymbol args          = throwError $ NumArgs 1 args


-----------------------------------------
-- list ops
-----------------------------------------

car :: [SchemeVal] -> SchemeValOrError
car []                     = throwError $ NumArgs 1 []
car [arg] = case arg of
  SList (x:_)             -> return x
  empty@(SList [])        -> throwError $ TypeMismatch "pair" empty
  SDottedList (x:_) _     -> return x
  wtf@(SDottedList [] _)  -> throwError $ TypeMismatch "pair" wtf
  val                     -> throwError $ TypeMismatch "pair" val
car args                   = throwError $ NumArgs 1 args

cdr :: [SchemeVal] -> SchemeValOrError
cdr empty@[]                   = throwError $ NumArgs 1 empty
cdr [arg] = case arg of
  SList (_:xs)              -> return $ SList xs
  empty@(SList [])          -> throwError $ TypeMismatch "pair" empty
  SDottedList [_] bloop     -> return bloop
  SDottedList (_:xs) bloop  -> return $ SDottedList xs bloop
  SDottedList _ bloop       -> return bloop
  val                       -> throwError  $ TypeMismatch "pair" val
cdr args                     = throwError $ NumArgs 1 args

cons :: [SchemeVal] -> SchemeValOrError
cons []                           = throwError $ NumArgs 2 []
cons singleArg@[_]                = throwError $ NumArgs 2 singleArg
cons [x, SList xs]                = return $ SList (x:xs)
cons [x, SDottedList bleep bloop] = return $ SDottedList (x:bleep) bloop
cons [a, b]                       = return $ SDottedList [a] b
cons args                         = throwError $ NumArgs 2 args

-----------------------------------------
-- equality
-----------------------------------------

-- TODO
-- eq :: [SchemeVal] -> SchemeValOrError

eqv :: [SchemeVal] -> SchemeValOrError
eqv []                = throwError $ NumArgs 2 []
eqv singleArg@[_]     = throwError $ NumArgs 2 singleArg
eqv [a, b] = return $ SBool $ case (a, b) of
 (SSymbol a',    SSymbol b')    -> a' == b'
 (SChar a', SChar b')           -> a' == b'
 (SString a',    SString b')    -> a' == b'
 (SBool a',      SBool b')      -> a' == b'
 (SNumber a',    SNumber b')    ->
   case (a', b') of
     (SInteger a'',  SInteger b'')  -> a'' == b''
     (SRational _ _, SRational _ _) -> error "not implemented" -- TODO: use the std lib's Ratio
     (SReal a'',     SReal b'')     -> a'' == b''
     (SComplex _ _, SComplex _ _)   -> error "not implemented" -- TODO: use the std lib's Complex
     (_, _)                         -> False
 (_, _)                             -> error "not implemented" -- TODO: iterables
eqv args              = throwError $ NumArgs 2 args


-- TODO: this is gross
isTrue :: SchemeVal -> Bool
isTrue (SBool val) = val
isTrue _ = False


equalLists :: [SchemeVal] -> [SchemeVal] -> SchemeValOrError
equalLists a b =
    let allTrue :: [SchemeVal] -> SchemeVal
        allTrue = SBool . all isTrue
        lispBools = zipWithM (\ a' b' -> equal [a', b']) a b
    in fmap allTrue lispBools


equal :: [SchemeVal] -> SchemeValOrError
equal []                = throwError $ NumArgs 2 []
equal singleArg@[_]     = throwError $ NumArgs 2 singleArg
equal [a, b] = case (a, b) of
  (SList a', SList b') -> equalLists a' b'

  (SDottedList aBleep aBloop, SDottedList bBleep bBloop) -> do
    bleepsEqual <- equalLists aBleep bBleep
    bloopsEqual <- equal [aBloop, bBloop]
    return $ SBool $ isTrue bleepsEqual && isTrue bloopsEqual

  (_, _) -> eqv [a, b]

equal args              = throwError $ NumArgs 2 args
