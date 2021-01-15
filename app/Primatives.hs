module Primatives ( primatives ) where
import Control.Monad.Except ( throwError )
import Types
  ( LispNumber (..)
  , LispVal (..)
  , LispError (..)
  , LispValOrError
  )

import Data.Complex (Complex((:+)), realPart, imagPart)

primatives :: [(String, [LispVal] -> LispValOrError)]
primatives =
  [ ("+",         numericBinOp add)
  , ("-",         numericBinOp subtract_)
  , ("*",         numericBinOp multiply)
  , ("/",         numericBinOp divide)
  -- , ("mod",       numericBinOp mod)
  -- , ("quotient",  numericBinOp quot)
  -- , ("remainder", numericBinOp rem)

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
  ]

------------------------------------------
-- helpers (not actual scheme primatives)
------------------------------------------
numericBinOp
  :: (LispNumber -> LispNumber -> LispNumber)
  -> ([LispVal] -> LispValOrError )
numericBinOp _ []              = throwError $ NumArgs 2 []
numericBinOp _ singleVal@[_]   = throwError $ NumArgs 2 singleVal
numericBinOp op args           = foldl1M wrappedOp args
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
