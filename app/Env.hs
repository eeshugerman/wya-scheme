module Env
  ( nullEnv
  , primitiveEnv
  , getVar
  , setVar
  , defineVar
  , extendFrom
  ) where

import Data.IORef (writeIORef, readIORef, newIORef)
import Primitives (primitives)
import Control.Monad.Except (throwError, liftIO)

import Types
  ( LispError(..)
  , IONilOrError
  , IOLispValOrError
  , Env
  , LispVal
  )

nullEnv :: IO Env
nullEnv = newIORef []

primitiveEnv :: IO Env
primitiveEnv = nullEnv >>= flip extendFrom primitives


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


extendFrom :: Env -> [(String, LispVal)] -> IO Env
extendFrom baseEnvRef bindings = do
  baseEnvMap <- readIORef baseEnvRef
  envRef <- newIORef baseEnvMap
  mapM_ (uncurry $ defineVar envRef) bindings
  return envRef

