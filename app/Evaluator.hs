module Evaluator where
import Types ( LispVal (..) )

primatives :: [(String, [LispVal] -> LispVal)]
primatives = [ ("+",         numericBinOp (+))
             , ("-",         numericBinOp (-))
             , ("*",         numericBinOp (*))
             , ("/",         numericBinOp div)
             , ("mod",       numericBinOp mod)
             , ("quotient",  numericBinOp quot)
             , ("remainder", numericBinOp rem)
             ]
  where
    numericBinOp
      :: (Integer -> Integer -> Integer)  -- op
      -> [LispVal] -- params
      -> LispVal   -- result
    numericBinOp op params = LispInteger $ foldl1 op $ map unpackInteger params

    unpackInteger :: LispVal -> Integer
    unpackInteger (LispInteger n) = n
    unpackInteger _ = error "unimplemented or invalid numericBinOp"

eval :: LispVal -> LispVal
eval val@(LispBool _)        = val
eval val@(LispCharacter _)   = val
eval val@(LispString _)      = val
eval val@(LispInteger _)     = val
eval val@(LispRational _ _)  = val
eval val@(LispReal _)        = val
eval val@(LispComplex _ _)   = val
-- eval val@(LispList)
-- eval val@(LispVector)
-- eval val@(LispDottedList)
eval (LispList [LispSymbol "quote", val]) = val
eval (LispList (LispSymbol funcName : args)) = func $ map eval args
  where
    func :: [LispVal] -> LispVal
    func args' = case lookup funcName primatives of
      Nothing    -> LispBool False
      Just func'  -> func' args'
eval _ = error "uh oh"


