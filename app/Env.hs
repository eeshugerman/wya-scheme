module Env
  ( nullEnv
  -- , primitiveEnv
  , getVar
  , setVar
  , defineVar
  , extendWith
  ) where

import Data.IORef (writeIORef, readIORef, newIORef)
import Control.Monad.Except (throwError, liftIO)

import Types
  ( SchemeError(..)
  , IONilOrError
  , IOSchemeValOrError
  , Env
  , SchemeVal
  )
import qualified Data.Map.Strict as Map

nullEnv :: IO Env
nullEnv = newIORef Map.empty

getVar :: Env -> String -> IOSchemeValOrError
getVar envRef varName = do
  envMap <- liftIO $ readIORef envRef
  case Map.lookup varName envMap of
    Nothing -> throwError $ UnboundVar varName
    Just varRef -> liftIO $ readIORef varRef

setVar :: Env -> String -> SchemeVal -> IONilOrError
setVar envRef varName val = do
  envMap <- liftIO $ readIORef envRef
  case Map.lookup varName envMap of
    Nothing -> throwError $ UnboundVar varName
    Just varRef -> liftIO $ writeIORef varRef val

defineVar :: Env -> String -> SchemeVal -> IO ()
defineVar envRef varName val = do
  envMap <- readIORef envRef
  case Map.lookup varName envMap of
    Nothing -> do
      varRef <- newIORef val
      let newEnvMap = Map.insert varName varRef envMap
      liftIO $ writeIORef envRef newEnvMap
    Just varRef -> writeIORef varRef val

extendWith :: [(String, SchemeVal)] -> Env -> IO Env
extendWith bindings baseEnvRef = do
  baseEnvMap <- readIORef baseEnvRef
  envRef <- newIORef baseEnvMap
  mapM_ (uncurry $ defineVar envRef) bindings
  return envRef
