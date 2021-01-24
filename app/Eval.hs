{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Eval where
import Control.Monad.Except (ExceptT, MonadIO(liftIO),  throwError )
import Types (LispValOrError,  LispVal (..), LispError (..), Env )
import Data.IORef (writeIORef, readIORef, newIORef)
import Data.Maybe (isNothing)

-- IO LispVal with LispError exceptions
type IOLispValOrError = ExceptT LispError IO LispVal
type IONilOrError = ExceptT LispError IO ()

liftThrows :: LispValOrError -> IOLispValOrError
liftThrows = \case
  Left err -> throwError err
  Right val -> return val

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


-- TODO: move this somewhere that makes more sense
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

data ProcSpec = ProcSpec { psEnv      :: Env
                         , psName     :: String
                         , psParams   :: [LispVal]
                         , psVarParam :: Maybe LispVal
                         , psBody     :: [LispVal]
                         }

makeProc :: ProcSpec -> LispValOrError
makeProc ProcSpec{..} =
  do params <- mapM symbolToString psParams
     varParam <- mapM symbolToString psVarParam
     return $ LispProc { procParams   = params
                       , procVarParam = varParam
                       , procBody     = psBody
                       , procClosure  = psEnv
                       }
  where
    symbolToString :: LispVal -> Either LispError String
    symbolToString = \case
      LispSymbol val -> return val
      _ -> throwError $ BadForm "Invalid procedure definition"
                                (LispList (LispSymbol psName : psParams))

eval :: Env -> LispVal -> IOLispValOrError
eval _   val@(LispDottedList _ _) = throwError $ BadForm "Can't eval dotted list" val
eval _   val@(LispVector _)       = throwError $ BadForm "Can't eval vector" val

eval _   val@(LispBool _)         = return val
eval _   val@(LispCharacter _)    = return val
eval _   val@(LispString _)       = return val
eval _   val@(LispNumber _)       = return val

eval env (LispSymbol varName)     = getVar env varName

eval _   (LispList [LispSymbol "quote", val])      = return val
eval env (LispList [LispSymbol "quasiquote", val]) = evalQuasiquoted env val

eval env (LispList (LispSymbol "lambda" : LispList params : body)) =
  liftThrows $ makeProc ProcSpec
    { psEnv      = env
    , psName     = "<lambda>"
    , psParams   = params
    , psVarParam = Nothing
    , psBody     = body
    }

eval env (LispList (LispSymbol "lambda" : LispDottedList params varParam : body)) =
  liftThrows $ makeProc ProcSpec
    { psEnv      = env
    , psName     = "<lambda>"
    , psParams   = params
    , psVarParam = Just varParam
    , psBody     = body
    }

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

eval env (LispList (LispSymbol "define" : LispList (LispSymbol name : params) : body)) =
  do proc' <- liftThrows $ makeProc ProcSpec
       { psEnv      = env
       , psName     = name
       , psParams   = params
       , psVarParam = Nothing
       , psBody     = body
       }
     liftIO $ defineVar env name proc'
     return $ LispList []

eval env (LispList ( LispSymbol "define"
                   : LispDottedList (LispSymbol name : params) varParams
                   : body)) =
  do proc' <- liftThrows $ makeProc ProcSpec
       { psEnv      = env
       , psName     = name
       , psParams   = params
       , psVarParam = Just varParams
       , psBody     = body
       }
     liftIO $ defineVar env name proc'
     return $ LispList []


eval env (LispList (procExpr:args)) =
  do proc' <- eval env procExpr
     evaledArgs <- mapM (eval env) args
     case proc' of
       -- TODO: factor out into `apply` primative ?
       LispPrimitiveProc primProc -> liftThrows $ primProc evaledArgs
       LispProc {..} ->
         let numParams' = length procParams
             numParams  = toInteger numParams'
             numArgs    = toInteger $ length args
         in if (numParams > numArgs) || (numParams < numArgs && isNothing procVarParam)
            then
              throwError $ NumArgs numParams args
            else
              let remainingArgs = drop numParams' args
                  paramsArgsMap = zip procParams args ++ case procVarParam of
                    Just varParamName -> [(varParamName, LispList remainingArgs)]
                    Nothing           -> []
              in do
                procEnv <- liftIO $ extendFrom procClosure paramsArgsMap
                last $ map (eval procEnv) procBody

       nonProc           -> throwError $ TypeMismatch "procedure" nonProc



eval _ form = throwError $ BadForm "Unrecognized form" form

