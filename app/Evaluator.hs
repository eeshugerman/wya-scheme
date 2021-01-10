module Evaluator where
import Control.Monad.Except ( throwError )
import Types ( LispVal (..), LispError (..), LispValOrError )
import Primatives ( primatives )


evalQuasiquoted :: [LispVal] -> LispValOrError
evalQuasiquoted =
  let iter :: [LispVal] -> [LispVal] -> LispValOrError
      iter acc [] = return $ LispList acc
      iter acc (LispList [LispSymbol "unquote", unquoted]:xs) =
        case eval unquoted of
          Left err -> throwError err
          Right val -> iter (acc ++ [val]) xs
      iter acc (x:xs) = iter (acc ++ [x]) xs
  in iter []

eval :: LispVal -> LispValOrError
eval val@(LispBool _)        = return val
eval val@(LispCharacter _)   = return val
eval val@(LispString _)      = return val
eval val@(LispNumber _)      = return val
-- eval val@(LispVector)
-- eval val@(LispDottedList)

eval (LispList [LispSymbol "unquote", val]) = eval val
eval (LispList [LispSymbol "quote", val]) = return val
eval (LispList [LispSymbol "quasiquote", LispList list]) =
  case evalQuasiquoted list of
    Left err -> throwError err
    Right val -> return val

eval (LispList (LispSymbol funcName : args)) = mapM eval args >>= func
  where
    func :: [LispVal] -> LispValOrError
    func args' = case lookup funcName primatives of
      Nothing     -> throwError $ NotAFunction "Unknown primitive function" funcName
      Just func'  -> func' args'


eval form = throwError $ BadForm "Unrecognized form: " form

