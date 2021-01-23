{-# LANGUAGE LambdaCase #-}
module Eval where
import Control.Monad.Except (ExceptT, MonadIO(liftIO),  throwError )
import Types ( LispVal (..), LispError (..), Env )
import Primatives ( primatives )
import Data.IORef (writeIORef, readIORef, newIORef)

nullEnv :: IO Env
nullEnv = newIORef []

-- IO LispVal with LispError exceptions
type IOLispValOrError = ExceptT LispError IO LispVal
type IONilOrError = ExceptT LispError IO ()

getVar :: Env -> String -> IOLispValOrError
getVar envRef varName = do
  envMap <- liftIO $ readIORef envRef
  case lookup varName envMap of
    Nothing -> throwError $ UnboundVar varName
    Just varRef -> liftIO $ readIORef varRef


setVar :: Env -> String -> LispVal -> IONilOrError
setVar envRef varName val = do
  envMap <- liftIO $ readIORef envRef
  case lookup varName envMap of
    Nothing -> throwError $ UnboundVar varName
    Just varRef -> liftIO $ writeIORef varRef val


defineVar :: Env -> String -> LispVal -> IO ()
defineVar envRef varName val = do
  envMap <- readIORef envRef
  case lookup varName envMap of
    Nothing -> do
      varRef <- newIORef val
      liftIO $ writeIORef envRef ((varName, varRef):envMap)
    Just varRef -> writeIORef varRef val


-- TODO: untested, might not work
extendFrom :: Env -> [(String, LispVal)] -> IO Env
extendFrom baseEnvRef bindings = do
  baseEnvMap <- readIORef baseEnvRef
  envRef <- newIORef baseEnvMap
  mapM_ (uncurry $ defineVar envRef) bindings
  return envRef

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

eval env (LispSymbol varName)  = getVar env varName

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

eval env (LispList (LispSymbol procName : args)) =
  do evaledArgs <- mapM (eval env) args
     case lookup procName primatives of
       Nothing -> throwError $ UnboundVar procName
       Just primProc -> case primProc evaledArgs of
         Left err -> throwError err
         Right val -> return val



eval _ form = throwError $ BadForm "Unrecognized form" form

