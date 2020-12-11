{-# LANGUAGE LambdaCase #-}
module Evaluator where
import Types ( LispVal (..) )

primatives :: [(String, [LispVal] -> LispVal)]
primatives =
  [ ("+",         numericBinOp (+))
  , ("-",         numericBinOp (-))
  , ("*",         numericBinOp (*))
  , ("/",         numericBinOp div)
  , ("mod",       numericBinOp mod)
  , ("quotient",  numericBinOp quot)
  , ("remainder", numericBinOp rem)

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
      :: (Integer -> Integer -> Integer)  -- op
      -> [LispVal] -- params
      -> LispVal   -- result
    numericBinOp op params = LispInteger $ foldl1 op $ map unpackInteger params
      where
        unpackInteger :: LispVal -> Integer
        unpackInteger (LispInteger n) = n
        unpackInteger _ = error "unimplemented or invalid numericBinOp"

    -- TODO: how to make it DRY? type tags? https://stackoverflow.com/a/6039229/8204023
    arityErrMsg typeName = "wrong number of arguments to `" ++ typeName ++ "`"
    argsTypeErrMsg typeName = "wrong type passed to `" ++ typeName ++ "`"

    isSymbol :: [LispVal] -> LispVal
    isSymbol [LispSymbol _]        = LispBool True
    isSymbol [_]                   = LispBool False
    isSymbol _                     = error $ arityErrMsg "symbol?"

    isBoolean :: [LispVal] -> LispVal
    isBoolean [LispBool _]         = LispBool True
    isBoolean [_]                  = LispBool False
    isBoolean _                    = error $ arityErrMsg "boolean?"

    isCharacter :: [LispVal] -> LispVal
    isCharacter [LispCharacter _] = LispBool True
    isCharacter [_]               = LispBool False
    isCharacter _                 = error $ arityErrMsg "character?"

    isString :: [LispVal] -> LispVal
    isString [LispString _]       = LispBool True
    isString [_]                  = LispBool False
    isString _                    = error $ arityErrMsg "string?"

    isList :: [LispVal] -> LispVal
    isList [LispList _]           = LispBool True
    isList [_]                    = LispBool False
    isList _                      = error $ arityErrMsg "list?"

    isVector :: [LispVal] -> LispVal
    isVector [LispVector _]       = LispBool True
    isVector [_]                  = LispBool False
    isVector _                    = error $ arityErrMsg "vector?"

    isNumber :: [LispVal] -> LispVal
    isNumber = LispBool . \case
      [LispInteger _]    -> True
      [LispRational _ _] -> True
      [LispReal _]       -> True
      [LispComplex _ _]  -> True
      [_]                -> False
      _                  -> error $ arityErrMsg "number?"

    isComplex :: [LispVal] -> LispVal
    isComplex = isNumber

    isReal :: [LispVal] -> LispVal
    isReal = LispBool . \case
      [LispInteger _]    -> True
      [LispRational _ _] -> True
      [LispReal _]       -> True
      [_]                -> False
      _                  -> error $ arityErrMsg "number?"

    isRational :: [LispVal] -> LispVal
    isRational = LispBool . \case
      [LispInteger _]    -> True
      [LispRational _ _] -> True
      [_]                -> False
      _                  -> error $ arityErrMsg "number?"

    isInteger :: [LispVal] -> LispVal
    isInteger = LispBool . \case
      [LispInteger _]    -> True
      [_]                -> False
      _                  -> error $ arityErrMsg "number?"

    symbolToString :: [LispVal] -> LispVal
    symbolToString [LispSymbol val] = LispString val
    symbolToString [_]              = error $ argsTypeErrMsg "symbol->string"
    symbolToString _                = error $ arityErrMsg "symbol->string"

    stringToSymbol :: [LispVal] -> LispVal
    stringToSymbol [LispString val] = LispSymbol val
    stringToSymbol [_]              = error $ argsTypeErrMsg "string->symbol"
    stringToSymbol _                = error $ arityErrMsg "string->symbol"


eval :: LispVal -> LispVal
eval val@(LispBool _)        = val
eval val@(LispCharacter _)   = val
eval val@(LispString _)      = val
eval val@(LispInteger _)     = val
eval val@(LispRational _ _)  = val
eval val@(LispReal _)        = val
eval val@(LispComplex _ _)   = val
-- eval val@(LispVector)
-- eval val@(LispDottedList)
eval (LispList [LispSymbol "quote", val]) = val
eval (LispList (LispSymbol funcName : args)) = func $ map eval args
  where
    func :: [LispVal] -> LispVal
    func args' = case lookup funcName primatives of
      Nothing     -> LispBool False
      Just func'  -> func' args'
eval val@_ = error $ "uh oh: " ++ show val


