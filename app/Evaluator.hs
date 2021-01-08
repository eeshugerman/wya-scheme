module Evaluator where
import Control.Monad.Except ( throwError )
import Types ( LispVal (..), LispError (..), LispValOrError )
import Primatives ( primatives )


          Left err -> throwError err

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

