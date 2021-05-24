module Env
  ( nullEnv
  , getVar
  , setVar
  , defineVar
  , extendWith
  ) where

import Data.IORef (IORef, writeIORef, readIORef, newIORef)
import Control.Monad.Except (throwError, liftIO)
import qualified Data.Unique as U

import Types
  ( SchemeError(..)
  , IONilOrError
  , IOSchemeValOrError
  , Env
  , SchemeVal(..)
  )
import qualified Data.Map.Strict as Map

fillTag :: SchemeVal -> IO SchemeVal
fillTag (SchemeVal Nothing val) = do
  tag <- U.newUnique
  return $ SchemeVal (Just tag) val
fillTag val@(SchemeVal (Just _) _) = return val

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
  taggedVal <- liftIO $ fillTag val
  case Map.lookup varName envMap of
    Nothing -> throwError $ UnboundVar varName
    Just varRef -> liftIO $ writeIORef varRef taggedVal

defineVar :: Env -> String -> SchemeVal -> IO ()
defineVar envRef varName val = do
  envMap <- readIORef envRef
  taggedVal <- fillTag val
  case Map.lookup varName envMap of
    Nothing -> do
      varRef <- newIORef taggedVal
      let newEnvMap = Map.insert varName varRef envMap
      liftIO $ writeIORef envRef newEnvMap
    Just varRef -> writeIORef varRef taggedVal

extendWith :: [(String, SchemeVal)] -> Env -> IO Env
extendWith bindings baseEnvRef = do
  envRef <- deepCopy baseEnvRef
  mapM_ (uncurry $ defineVar envRef) bindings
  return envRef
  where
    deepCopy :: Env -> IO Env
    deepCopy oldEnv = do
      oldMap <- readIORef oldEnv
      newMap <- mapM copy oldMap
      newIORef newMap

    copy :: IORef SchemeVal -> IO (IORef SchemeVal)
    copy = \x -> readIORef x >>= newIORef

