{-# LANGUAGE LambdaCase #-}
module Primatives ( primatives ) where
import Control.Monad.Except ( throwError )

import Types
  ( LispNumber (..)
  , LispVal (..)
  , LispError (..)
  , LispValOrError
  )

import Data.Complex (Complex((:+)), realPart, imagPart)
import Control.Monad (zipWithM)

primatives :: [(String, [LispVal] -> LispValOrError)]
primatives =
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
-- helpers (not actual scheme primatives)
------------------------------------------
numericFoldableOp
  :: (LispNumber -> LispNumber -> LispNumber)
  -> ([LispVal] -> LispValOrError )
numericFoldableOp _ []              = throwError $ NumArgs 2 []
numericFoldableOp _ singleArg@[_]   = throwError $ NumArgs 2 singleArg
numericFoldableOp op args           = foldl1M wrappedOp args
  where
    foldl1M f (x:xs) = foldlM f x xs
    foldl1M _ [] = error "internal error in foldl1M"

    foldlM _ acc [] = return acc
    foldlM f acc (x:xs) = f acc x >>= \ acc' -> foldlM f acc' xs

    wrappedOp :: LispVal -> LispVal -> LispValOrError
    wrappedOp (LispNumber a) (LispNumber b) = return $ LispNumber $ op a b
    wrappedOp a              (LispNumber _) = throwError $ TypeMismatch "number" a
    wrappedOp (LispNumber _) b              = throwError $ TypeMismatch "number" b
    wrappedOp a              _              = throwError $ TypeMismatch "number" a

boolBinOp
  :: (LispVal -> Either LispError a)  -- unpacker
  -> (a -> a -> Bool)                 -- op
  -> [LispVal]                        -- args
  -> LispValOrError                   -- result
boolBinOp unpacker op args =
  if length args /= 2
  then throwError $ NumArgs 2 args
  else do left <- unpacker $ args !! 0
          right <- unpacker $ args !! 1
          return $ LispBool $ left `op` right

type BoolBinOpBuilder a
  = (a -> a -> Bool)
  -> [LispVal]
  -> LispValOrError

numBoolBinOp :: BoolBinOpBuilder Float
numBoolBinOp = boolBinOp unpackNum
  where
    unpackNum :: LispVal -> Either LispError Float
    unpackNum (LispNumber num) = case num of
      LispInteger val          -> return $ fromInteger val
      LispRational numer denom -> return (fromInteger numer / fromInteger denom)
      LispReal val             -> return val
      LispComplex _ _          -> throwError $
        Default "operation not implemented for complex numbers" -- TODO
    unpackNum nonNum = throwError $ TypeMismatch "number" nonNum

boolBoolBinOp :: BoolBinOpBuilder Bool
boolBoolBinOp = boolBinOp unpackBool
  where
    unpackBool :: LispVal -> Either LispError Bool
    unpackBool (LispBool val) = return val
    unpackBool nonBool = throwError $ TypeMismatch "boolean" nonBool

strBoolBinOp :: BoolBinOpBuilder String
strBoolBinOp = boolBinOp unpackString
  where
    unpackString :: LispVal -> Either LispError String
    unpackString (LispString val) = return val
    unpackString nonString = throwError $ TypeMismatch "string" nonString


-----------------------------------
add :: LispNumber  -> LispNumber -> LispNumber
-----------------------------------

add (LispInteger a)              (LispInteger b)              = LispInteger (a + b)
add (LispInteger a)              (LispReal b)                 = LispReal (fromInteger a + b)
add (LispInteger a)              (LispRational bNumer bDenom) = LispRational ((a * bDenom) + bNumer) bDenom
add a@(LispInteger _)            (LispComplex bReal bImag)    = LispComplex (add a bReal) bImag

add a@(LispReal _)               b@(LispInteger _)            = add b a
add (LispReal a)                 (LispReal b)                 = LispReal (a + b)
add (LispReal a)                 (LispRational bNumer bDenom) = LispReal (a + (fromInteger bNumer / fromInteger bDenom))
add a@(LispReal _)               (LispComplex bReal bImag)    = LispComplex (add a bReal) bImag

add a@(LispRational _ _)         b@(LispInteger _)            = add b a
add a@(LispRational _ _)         b@(LispReal _)               = add b a
add (LispRational an ad)         (LispRational  bn bd)        = addRationals an ad bn bd
add a@(LispRational _ _)         (LispComplex bReal bImag)    = LispComplex (add a bReal) bImag

add a@(LispComplex _ _)          b@(LispInteger _)            = add b a
add a@(LispComplex _ _)          b@(LispReal _)               = add b a
add a@(LispComplex _ _)          b@(LispRational _ _)         = add b a
add (LispComplex aReal aImag)    (LispComplex bReal bImag)    = LispComplex (add aReal bReal) (add aImag bImag)

addRationals
  :: Integer -> Integer -- a
  -> Integer -> Integer -- b
  -> LispNumber         -- res
addRationals aNumer aDenom bNumer bDenom =
  let numer = (aNumer * bDenom) + (bNumer * aDenom)
      denom = aDenom * bDenom
  in LispRational numer denom

-----------------------------------------
multiply :: LispNumber -> LispNumber -> LispNumber
-----------------------------------------

multiply (LispInteger a)              (LispInteger b)              = LispInteger (a * b)
multiply (LispInteger a)              (LispReal b)                 = LispReal (fromInteger a * b)
multiply (LispInteger a)              (LispRational bNumer bDenom) = LispRational (a * bNumer) bDenom
multiply a@(LispInteger _)            (LispComplex bReal bImag)    = LispComplex (multiply a bReal) (multiply a bImag)

multiply a@(LispReal _)               b@(LispInteger _)            = multiply b a
multiply (LispReal a)                 (LispReal b)                 = LispReal (a * b)
multiply (LispReal a)                 (LispRational bNumer bDenom) = LispReal (a * (fromInteger bNumer / fromInteger bDenom))
multiply a@(LispReal _)               (LispComplex bReal bImag)    = LispComplex (multiply a bReal) (multiply a bImag)

multiply a@(LispRational _ _)         b@(LispInteger _)            = multiply b a
multiply a@(LispRational _ _)         b@(LispReal _)               = multiply b a
multiply (LispRational aNumer aDenom) (LispRational bNumer bDenom) = LispRational (aNumer * bNumer) (aDenom * bDenom)
multiply a@(LispRational _ _)         (LispComplex bReal bImag)    = LispComplex (multiply a bReal) (multiply a bImag)

multiply a@(LispComplex _ _)          b@(LispInteger _)            = add b a
multiply a@(LispComplex _ _)          b@(LispReal _)               = add b a
multiply a@(LispComplex _ _)          b@(LispRational _ _)         = add b a
multiply (LispComplex ar ai)          (LispComplex br bi)          = multiplyComplexes ar ai br bi

multiplyComplexes
  :: LispNumber -> LispNumber  -- a
  -> LispNumber -> LispNumber  -- b
  -> LispNumber                -- res
multiplyComplexes aReal aImag bReal bImag =
  let real = add (multiply aReal bImag) (multiply bImag aReal)
      imag = subtract_ (multiply aReal bReal) (multiply aImag bImag)
  in LispComplex real imag

-----------------------------------------
subtract_ :: LispNumber -> LispNumber -> LispNumber
-----------------------------------------

subtract_ a b = let negativeB = multiply (LispInteger (-1)) b
                in add a negativeB

-----------------------------------------
divide :: LispNumber -> LispNumber -> LispNumber
-----------------------------------------

divide (LispInteger a)       (LispInteger b)              = LispRational a b
divide (LispInteger a)       (LispReal b)                 = LispReal (fromInteger a / b)
divide a@(LispInteger _)     (LispRational bNumer bDenom) = multiply a (LispRational bDenom bNumer)
divide (LispInteger a)       (LispComplex bReal bImag)    = divideByComplex a bReal bImag
divide a                     b                            = multiply a (divide (LispInteger 1) b)

divideByComplex :: Integer -> LispNumber -> LispNumber -> LispNumber
divideByComplex a bReal bImag =
  let aRealFloat = fromInteger a
      aImagFloat = 0.0
      bRealFloat = toFloat bReal
      bImagFloat = toFloat bImag
      res = (aRealFloat :+ aImagFloat) / (bRealFloat :+ bImagFloat)
  in LispComplex (LispReal $ realPart res) (LispReal $ imagPart res)
  where
    toFloat :: LispNumber -> Float
    toFloat (LispInteger val)             = fromInteger val
    toFloat (LispReal val)                = val
    toFloat (LispRational numer denom)    = fromInteger numer / fromInteger denom
    toFloat (LispComplex _ _)             = error $ "internal error: nested complex number. "
                                                 ++ "this expression should not have parsed."

-----------------------------------------
-- type testing
-----------------------------------------

isSymbol :: [LispVal] -> LispValOrError
isSymbol [LispSymbol _]       = return $ LispBool True
isSymbol [_]                  = return $ LispBool False
isSymbol args                 = throwError $ NumArgs 1 args

isBoolean :: [LispVal] -> LispValOrError
isBoolean [LispBool _]        = return $ LispBool True
isBoolean [_]                 = return $ LispBool False
isBoolean args                = throwError $ NumArgs 1 args

isCharacter :: [LispVal] -> LispValOrError
isCharacter [LispCharacter _] = return $ LispBool True
isCharacter [_]               = return $ LispBool False
isCharacter args              = throwError $ NumArgs 1 args

isString :: [LispVal] -> LispValOrError
isString [LispString _]       = return $ LispBool True
isString [_]                  = return $ LispBool False
isString args                 = throwError $ NumArgs 1 args

isList :: [LispVal] -> LispValOrError
isList [LispList _]           = return $ LispBool True
isList [_]                    = return $ LispBool False
isList args                   = throwError $ NumArgs 1 args

isVector :: [LispVal] -> LispValOrError
isVector [LispVector _]       = return $ LispBool True
isVector [_]                  = return $ LispBool False
isVector args                 = throwError $ NumArgs 1 args

-- num type stuff
-- todo: this is dumb, arguably. maybe don't be fancy.
data NumType = Number | Complex | Real | Rational | Integer
isNumType :: NumType -> LispNumber -> Bool
isNumType Number   _   = True
isNumType Complex  _   = True
isNumType Real     val = case val of LispComplex _ _  -> False
                                     _                -> isNumType Complex val
isNumType Rational val = case val of LispReal _       -> False
                                     _                -> isNumType Real val
isNumType Integer  val = case val of LispRational _ _ -> False
                                     _                -> isNumType Rational val

isNumTypeWrapper :: NumType -> [LispVal] -> LispValOrError
isNumTypeWrapper numType [LispNumber num] = return $ LispBool $ isNumType numType num
isNumTypeWrapper _       [_]              = return $ LispBool False
isNumTypeWrapper _       args             = throwError $ NumArgs 1 args

isNumber :: [LispVal] -> LispValOrError
isNumber = isNumTypeWrapper Number

isComplex :: [LispVal] -> LispValOrError
isComplex = isNumTypeWrapper Complex

isReal :: [LispVal] -> LispValOrError
isReal = isNumTypeWrapper Real

isRational :: [LispVal] -> LispValOrError
isRational = isNumTypeWrapper Rational

isInteger :: [LispVal] -> LispValOrError
isInteger = isNumTypeWrapper Integer

-----------------------------------------
-- type conversion
-----------------------------------------

symbolToString :: [LispVal] -> LispValOrError
symbolToString [LispSymbol arg] = return $ LispString arg
symbolToString [arg]            = throwError $ TypeMismatch "symbol" arg
symbolToString args             = throwError $ NumArgs 1 args

stringToSymbol :: [LispVal] -> LispValOrError
stringToSymbol [LispString arg] = return $ LispSymbol arg
stringToSymbol [arg]            = throwError $ TypeMismatch "string" arg
stringToSymbol args             = throwError $ NumArgs 1 args


-----------------------------------------
-- list ops
-----------------------------------------

car :: [LispVal] -> LispValOrError
car []                        = throwError $ NumArgs 1 []
car [arg] = case arg of
  LispList (x:_)             -> return x
  empty@(LispList [])        -> throwError $ TypeMismatch "pair" empty
  LispDottedList (x:_) _     -> return x
  wtf@(LispDottedList [] _)  -> throwError $ TypeMismatch "pair" wtf
  val                        -> throwError $ TypeMismatch "pair" val
car args                      = throwError $ NumArgs 1 args

cdr :: [LispVal] -> LispValOrError
cdr empty@[]                   = throwError $ NumArgs 1 empty
cdr [arg] = case arg of
  LispList (_:xs)              -> return $ LispList xs
  empty@(LispList [])          -> throwError $ TypeMismatch "pair" empty
  LispDottedList [_] bloop     -> return bloop
  LispDottedList (_:xs) bloop  -> return $ LispDottedList xs bloop
  LispDottedList _ bloop       -> return bloop
  val                          -> throwError  $ TypeMismatch "pair" val
cdr args                        = throwError $ NumArgs 1 args

cons :: [LispVal] -> LispValOrError
cons []                              = throwError $ NumArgs 2 []
cons singleArg@[_]                   = throwError $ NumArgs 2 singleArg
cons [x, LispList xs]                = return $ LispList (x:xs)
cons [x, LispDottedList bleep bloop] = return $ LispDottedList (x:bleep) bloop
cons [a, b]                          = return $ LispDottedList [a] b
cons args                            = throwError $ NumArgs 2 args

-----------------------------------------
-- equality
-----------------------------------------

-- TODO
-- eq :: [LispVal] -> LispValOrError

eqv :: [LispVal] -> LispValOrError
eqv []                = throwError $ NumArgs 2 []
eqv singleArg@[_]     = throwError $ NumArgs 2 singleArg
eqv [a, b] = return $ LispBool $ case (a, b) of
 (LispSymbol a',    LispSymbol b')    -> a' == b'
 (LispCharacter a', LispCharacter b') -> a' == b'
 (LispString a',    LispString b')    -> a' == b'
 (LispBool a',      LispBool b')      -> a' == b'
 (LispNumber a',    LispNumber b') ->
   case (a', b') of
     (LispInteger a'',  LispInteger b'')  -> a'' == b''
     (LispRational _ _, LispRational _ _) -> error "not implemented" -- TODO: use the std lib's Ratio
     (LispReal a'',     LispReal b'')     -> a'' == b''
     (LispComplex _ _, LispComplex _ _)   -> error "not implemented" -- TODO: use the std lib's Complex
     (_, _)                               -> False
 (_, _)                                   -> error "not implemented" -- TODO: iterables
eqv args              = throwError $ NumArgs 2 args


-- TODO: this is gross
isTrue :: LispVal -> Bool
isTrue (LispBool val) = val
isTrue _ = False


equalLists :: [LispVal] -> [LispVal] -> LispValOrError
equalLists a b =
    let allTrue :: [LispVal] -> LispVal
        allTrue = LispBool . all isTrue
        lispBools = zipWithM (\ a' b' -> equal [a', b']) a b
    in fmap allTrue lispBools


equal :: [LispVal] -> LispValOrError
equal []                = throwError $ NumArgs 2 []
equal singleArg@[_]     = throwError $ NumArgs 2 singleArg
equal [a, b] = case (a, b) of
  (LispList a', LispList b') -> equalLists a' b'

  (LispDottedList aBleep aBloop, LispDottedList bBleep bBloop) -> do
    bleepsEqual <- equalLists aBleep bBleep
    bloopsEqual <- equal [aBloop, bBloop]
    return $ LispBool $ isTrue bleepsEqual && isTrue bloopsEqual

  (_, _) -> eqv [a, b]

equal args              = throwError $ NumArgs 2 args
