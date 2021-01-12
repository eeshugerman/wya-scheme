module Evaluator where
import Control.Monad.Except ( throwError )
import Types ( LispVal (..), LispError (..), LispValOrError )
import Primatives ( primatives )

sugarify :: String -> LispVal -> LispVal
sugarify symbol val = LispList [LispSymbol symbol, val]

unquote :: LispVal -> LispVal
unquote val = LispList [LispSymbol "unquote", val]

quasiquote :: LispVal -> LispVal
quasiquote val = LispList [LispSymbol "quasiquote", val]

evalQuasiquoted :: [LispVal] -> LispValOrError
evalQuasiquoted =
  let iter
        :: Integer    -- quasiquote level
        -> [LispVal]  -- accumulator
        -> [LispVal]  -- remaining
        -> LispValOrError
      iter _ acc [] = return $ LispList $ reverse acc
      iter qqDepth acc (LispList [LispSymbol "unquote", unquoted]:xs) =
        if qqDepth == 1 then doEval else doSemiEval
        where
          doEval = case eval unquoted of
            Left err -> throwError err
            Right x -> iter qqDepth (x:acc) xs

          doSemiEval = case unquoted of
            LispList list ->
              case iter (qqDepth - 1) [] list of
                Left err -> throwError err
                Right x -> iter qqDepth (unquote x:acc) xs
            val -> return val

      iter qqDepth acc (LispList [LispSymbol "quasiquote", quasiquoted]:xs) =
        case quasiquoted of
          LispList list ->
            case iter (qqDepth + 1) [] list of
              Left err -> throwError err
              Right x -> iter qqDepth (quasiquote x:acc) xs
          val -> return val

      iter qqDepth acc (unevaled:xs) = iter qqDepth (unevaled:acc) xs
  in iter 1 []

eval :: LispVal -> LispValOrError
eval val@(LispBool _)        = return val
eval val@(LispCharacter _)   = return val
eval val@(LispString _)      = return val
eval val@(LispNumber _)      = return val
-- eval val@(LispVector)
-- eval val@(LispDottedList)

eval (LispList [LispSymbol "quote", val]) = return val
eval (LispList [LispSymbol "quasiquote", LispList list]) = evalQuasiquoted list

eval (LispList (LispSymbol funcName : args)) = mapM eval args >>= func
  where
    func :: [LispVal] -> LispValOrError
    func args' = case lookup funcName primatives of
      Nothing     -> throwError $ NotAFunction "Unknown primitive function" funcName
      Just func'  -> func' args'

eval form = throwError $ BadForm "Unrecognized form: " form

