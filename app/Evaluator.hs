module Evaluator where
import Control.Monad.Except ( throwError )
import Types ( LispVal (..), LispError (..), LispValOrError )
import Primatives ( primatives )

evalQuasiquoted :: LispVal -> LispValOrError
evalQuasiquoted (LispList list') =
  let unquote val = LispList [LispSymbol "unquote", val]
      quasiquote val = LispList [LispSymbol "quasiquote", val]

      iter
        :: Integer    -- quasiquote level
        -> [LispVal]  -- accumulator
        -> [LispVal]  -- remaining
        -> LispValOrError
      iter _ acc [] = return $ LispList $ reverse acc

      iter qqDepth _ [LispSymbol "unquote", val] =
        if qqDepth == 1
        then case eval val of
          Left err -> throwError err
          Right val' -> return val'
        else case val of
          LispList list ->
            case iter (qqDepth - 1) [] list of
              Left err -> throwError err
              Right val' -> return $ unquote val'
          nonList -> return $ unquote nonList

      iter qqDepth _ [LispSymbol "quasiquote", val] =
        case val of
          LispList list ->
            case iter (qqDepth + 1) [] list of
              Left err -> throwError err
              Right val' -> return $ quasiquote val'
          nonList -> return $ quasiquote nonList

      iter qqDepth acc (LispList list:xs) =
        case iter qqDepth [] list of
          Left err -> throwError err
          Right val -> iter qqDepth (val:acc) xs

      iter qqDepth acc (x:xs) = iter qqDepth (x:acc) xs

  in iter 1 [] list'

evalQuasiquoted val = return val


eval :: LispVal -> LispValOrError
eval val@(LispBool _)        = return val
eval val@(LispCharacter _)   = return val
eval val@(LispString _)      = return val
eval val@(LispNumber _)      = return val
-- eval val@(LispVector)
-- eval val@(LispDottedList)

eval (LispList [LispSymbol "quote", val]) = return val
eval (LispList [LispSymbol "quasiquote", val]) = evalQuasiquoted val

eval (LispList (LispSymbol funcName : args)) = mapM eval args >>= func
  where
    func :: [LispVal] -> LispValOrError
    func args' = case lookup funcName primatives of
      Nothing     -> throwError $ NotAFunction "Unknown primitive function" funcName
      Just func'  -> func' args'

eval form = throwError $ BadForm "Unrecognized form: " form

