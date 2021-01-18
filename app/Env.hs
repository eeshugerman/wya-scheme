{-# LANGUAGE LambdaCase #-}
module Env where

import Data.Maybe (isJust)
import Data.IORef
import Types (LispError(..),  LispVal)

import Control.Monad.Except
  ( MonadError(throwError)
  , MonadIO(liftIO)
  , ExceptT
  )


type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []


-- add LispError exceptions to IO LispVal
type IOLispValOrError = ExceptT LispError IO LispVal
type IONilOrError = ExceptT LispError IO ()

liftThrows :: Either LispError a -> ExceptT LispError IO a
liftThrows = \case
  Left err -> throwError err
  Right val -> return val


isBound :: Env -> String -> IO Bool
isBound envRef varName = isJust . lookup varName <$> readIORef envRef


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

