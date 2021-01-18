{-# LANGUAGE LambdaCase #-}
module Eval where
import Control.Monad.Except (MonadIO(liftIO),  throwError )
import Types ( LispVal (..), LispError (..) )
import Primatives ( primatives )
import Env(IOLispValOrError, Env, setVar, defineVar, liftThrows)

evalQuasiquoted :: Env -> LispVal -> IOLispValOrError 
evalQuasiquoted env (LispList list') =
  let unquote val = LispList [LispSymbol "unquote", val]
      quasiquote val = LispList [LispSymbol "quasiquote", val]

      iter
        :: Integer    -- quasiquote level
        -> [LispVal]  -- accumulator
        -> [LispVal]  -- remaining
        -> IOLispValOrError 
      iter _ acc [] = return $ LispList $ reverse acc

      iter qqDepth _ [LispSymbol "unquote", val] =
        if qqDepth == 1
        then eval env val
        else case val of
          LispList list -> unquote <$> iter (qqDepth - 1) [] list
          nonList -> return $ unquote nonList

      -- TODO: unquote-splicing

      iter qqDepth _ [LispSymbol "quasiquote", val] =
        case val of
          LispList list -> quasiquote <$> iter (qqDepth + 1) [] list
          nonList -> return $ quasiquote nonList

      iter qqDepth acc (LispList list:xs) = do
        scannedList <- iter qqDepth [] list
        iter qqDepth (scannedList:acc) xs

      iter qqDepth acc (x:xs) = iter qqDepth (x:acc) xs

  in iter 1 [] list'

evalQuasiquoted _ val = return val

eval :: Env -> LispVal -> IOLispValOrError
eval _ val@(LispBool _)        = return val
eval _ val@(LispCharacter _)   = return val
eval _ val@(LispString _)      = return val
eval _ val@(LispNumber _)      = return val

eval _ val@(LispDottedList _ _) = throwError $ BadForm "Can't eval dotted list" val
-- eval val@(LispVector)

eval _ (LispList [LispSymbol "quote", val]) = return val
eval env (LispList [LispSymbol "quasiquote", val]) = evalQuasiquoted env val

eval env (LispList [LispSymbol "if", predicate, consq, alt]) =
  eval env predicate >>= \case
    LispBool False -> eval env alt
    _  -> eval env consq

eval env (LispList [LispSymbol "set!", LispSymbol varName, form]) =
  do val <- eval env form
     setVar env varName val
     return $ LispList []

eval env (LispList [LispSymbol "define", LispSymbol varName, form]) =
  do val <- eval env form
     liftIO $ defineVar env varName val
     return $ LispList []

eval env (LispList (LispSymbol procName : args)) = do
  evaledArgs <- mapM (eval env) args
  case lookup procName primatives of
    Nothing -> throwError $ UnboundVar procName
    Just func -> liftThrows $ func evaledArgs


eval _ form = throwError $ BadForm "Unrecognized form: " form

