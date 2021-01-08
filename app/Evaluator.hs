{-# LANGUAGE LambdaCase #-}
module Evaluator where
import Control.Monad.Except ( throwError )
import Data.Complex (Complex((:+)), realPart, imagPart)
import Types ( LispVal (..), LispError (..), LispValOrError )


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
  where
    numericBinOp
      :: (LispVal -> LispVal -> LispVal) -- op
      -> [LispVal]                       -- args
      -> LispValOrError                  -- result
    numericBinOp _ []             = throwError $ NumArgs 2 []
    numericBinOp _ singleVal@[_]  = throwError $ NumArgs 2 singleVal
    numericBinOp op (arg:args)    = foldlEither wrappedOp arg args
      where
        foldlEither
          :: (LispVal -> LispVal -> LispValOrError) -- wrappedOp
          -> LispVal                                -- acc/initial
          -> [LispVal]                              -- args/rest
          -> LispValOrError                         -- result
        foldlEither _ acc [] = return acc
        foldlEither f acc (x:xs) = case f acc x of
          Left err -> throwError err
          Right val -> foldlEither f val xs

        wrappedOp :: LispVal -> LispVal -> LispValOrError
        wrappedOp a b
          | not $ isNumber_ a = throwError $ TypeMismatch "number" a
          | not $ isNumber_ b = throwError $ TypeMismatch "number" b
          | otherwise         = return $ op a b

    -----------------------------------
    add :: LispVal -> LispVal -> LispVal
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
    add a@(LispRational _ _)         b@(LispRational _ _)         = addRationals a b
    add a@(LispRational _ _)         (LispComplex bReal bImag)    = LispComplex (add a bReal) bImag

    add a@(LispComplex _ _)          b@(LispInteger _)            = add b a
    add a@(LispComplex _ _)          b@(LispReal _)               = add b a
    add a@(LispComplex _ _)          b@(LispRational _ _)         = add b a
    add (LispComplex aReal aImag)    (LispComplex bReal bImag)    = LispComplex (add aReal bReal) (add aImag bImag)

    -- TODO: design types so this isn't necessary
    add _ _ = error "internal error: bad args to add"

    addRationals (LispRational aNumer aDenom) (LispRational bNumer bDenom) =
      let numer = (aNumer * bDenom) + (bNumer * aDenom)
          denom = aDenom * bDenom
      in LispRational numer denom
    addRationals _ _ = error "internal error in addRationals"

    -----------------------------------------
    multiply :: LispVal -> LispVal -> LispVal
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
    multiply a@(LispComplex _ _)          b@(LispComplex _ _)          = multiplyComplexes a b

    multiply _ _ = error "internal error: bad args to multiply"

    multiplyComplexes (LispComplex aReal aImag) (LispComplex bReal bImag) =
      let real = add (multiply aReal bImag) (multiply bImag aReal)
          imag = subtract_ (multiply aReal bReal) (multiply aImag bImag)
      in LispComplex real imag
    multiplyComplexes _ _ = error "internal error in multiplyComplexes"

    -----------------------------------------
    subtract_ :: LispVal -> LispVal -> LispVal
    -----------------------------------------

    subtract_ a b = let negativeB = multiply (LispInteger (-1)) b
                    in add a negativeB

    -----------------------------------------
    divide :: LispVal -> LispVal -> LispVal
    -----------------------------------------

    divide (LispInteger a)       (LispInteger b)              = LispRational a b
    divide (LispInteger a)       (LispReal b)                 = LispReal (fromInteger a / b)
    divide a@(LispInteger _)     (LispRational bNumer bDenom) = multiply a (LispRational bDenom bNumer)
    divide (LispInteger a)       (LispComplex bReal bImag)    = divideByComplex a bReal bImag
    divide a b = multiply a (divide (LispInteger 1) b)

    divideByComplex :: Integer -> LispVal -> LispVal -> LispVal
    divideByComplex a bReal bImag =
      let aRealFloat = fromInteger a
          aImagFloat = 0.0
          bRealFloat = lispValToFloat bReal
          bImagFloat = lispValToFloat bImag
          res = (aRealFloat :+ aImagFloat) / (bRealFloat :+ bImagFloat)
      in LispComplex (LispReal $ realPart res) (LispReal $ imagPart res)
      where
        lispValToFloat :: LispVal -> Float
        lispValToFloat (LispInteger val)             = fromInteger val
        lispValToFloat (LispReal val)                = val
        lispValToFloat (LispRational numer denom)    = fromInteger numer / fromInteger denom
        lispValToFloat _                             = error "internal error in divide"

    -----------------------------------------
    -- type testing
    -----------------------------------------

    -- TODO: how to make it DRY? type tags? https://stackoverflow.com/a/6039229/8204023
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

    isNumber_ :: LispVal -> Bool
    isNumber_ = \case
      LispInteger _    -> True
      LispRational _ _ -> True
      LispReal _       -> True
      LispComplex _ _  -> True
      _                -> False

    isNumber :: [LispVal] -> LispValOrError
    isNumber [arg]   = return $ LispBool $ isNumber_ arg
    isNumber args    = throwError $ NumArgs 1 args

    isComplex = isNumber

    isReal :: [LispVal] ->    LispValOrError
    isReal [LispInteger _]    = return $ LispBool True
    isReal [LispRational _ _] = return $ LispBool True
    isReal [LispReal _]       = return $ LispBool True
    isReal [_]                = return $ LispBool False
    isReal args               = throwError $ NumArgs 1 args

    isRational :: [LispVal] -> LispValOrError
    isRational [LispInteger _]    = return $ LispBool True
    isRational [LispRational _ _] = return $ LispBool True
    isRational [_]                = return $ LispBool False
    isRational args               = throwError $ NumArgs 1 args

    isInteger :: [LispVal] -> LispValOrError
    isInteger [LispInteger _]    = return $ LispBool True
    isInteger [_]                = return $ LispBool False
    isInteger args               = throwError $ NumArgs 1 args

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

eval :: LispVal -> LispValOrError
eval val@(LispBool _)        = return val
eval val@(LispCharacter _)   = return val
eval val@(LispString _)      = return val
eval val@(LispInteger _)     = return val
eval val@(LispRational _ _)  = return val
eval val@(LispReal _)        = return val
eval val@(LispComplex _ _)   = return val
-- eval val@(LispVector)
-- eval val@(LispDottedList)
eval (LispList [LispSymbol "quote", val]) = return val
eval (LispList (LispSymbol funcName : args)) = mapM eval args >>= func
  where
    func :: [LispVal] -> LispValOrError
    func args' = case lookup funcName primatives of
      Nothing     -> throwError $ NotAFunction "Unknown primitive function" funcName
      Just func'  -> func' args'

eval form = throwError $ BadForm "Unrecognized form: " form

