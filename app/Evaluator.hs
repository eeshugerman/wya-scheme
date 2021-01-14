module Evaluator where
import Control.Monad.Except ( throwError )
import Types ( LispVal (..), LispError (..), LispValOrError )
import Primatives ( primatives )

evalQuasiquoted :: LispVal -> LispValOrError
evalQuasiquoted (LispList fullList) =
  let unquote val = LispList [LispSymbol "unquote", val]
      unquoteSplicing val = LispList [LispSymbol "unquote-splicing", val]
      quasiquote val = LispList [LispSymbol "quasiquote", val]

      iter
        :: Integer    -- quasiquote level
        -> [LispVal]  -- accumulator
        -> [LispVal]  -- remaining
        -> LispValOrError
      iter _ acc [] = return $ LispList $ reverse acc
      iter qqDepth acc (LispList [LispSymbol "unquote", val]:xs) =
        if qqDepth == 1
        then case eval val of
          Left err -> throwError err
          Right x -> iter qqDepth (x:acc) xs
        else case val of
          LispList list ->
            case iter (qqDepth - 1) [] list of
              Left err -> throwError err
              Right x -> iter qqDepth (unquote x:acc) xs
          nonList -> iter qqDepth (unquote nonList:acc) xs

      iter qqDepth acc (LispList [LispSymbol "unquote-splicing", val]:xs) =
        if qqDepth == 1
        then case eval val of
          Left err -> throwError err
          Right x -> case x of
            LispList list -> iter qqDepth (reverse list ++ acc) xs
            _ -> throwError $ Default "unquote-splicing applied to non-list"
        else case val of
          LispList list ->
            case iter (qqDepth - 1) [] list of
              Left err -> throwError err
              Right x -> iter qqDepth (unquoteSplicing x:acc) xs
          nonList -> iter qqDepth (unquoteSplicing nonList:acc) xs

      iter qqDepth acc (LispList [LispSymbol "quasiquote", val]:xs) =
        case val of
          LispList list ->
            case iter (qqDepth + 1) [] list of
              Left err -> throwError err
              Right x -> iter qqDepth (quasiquote x:acc) xs
          nonList -> iter qqDepth (quasiquote nonList:acc) xs

      iter qqDepth acc (unevaled:xs) = iter qqDepth (unevaled:acc) xs
  in iter 1 [] fullList

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

