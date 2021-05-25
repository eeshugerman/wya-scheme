{-# LANGUAGE LambdaCase #-}
module Main where
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Control.Monad.Except (runExceptT)
import Data.Functor ((<&>))

import Parser (readExpr, readExprs)
import Primitives (primitives)
import Env (extendWith, nullEnv)
import Eval (eval)
import Types
  ( Env
  , SchemeVal (SList, SSymbol, SString)
  , SchemeError
  , SchemeValOrError
  )


primitiveEnv :: IO Env
primitiveEnv = nullEnv
  >>= extendWith primitives


loadPreludeForm :: SchemeVal
loadPreludeForm = SList [SSymbol "load", SString "app/lib.scm"]


evalPrelude :: Env -> IO (Maybe SchemeError)
evalPrelude env =
  runExceptT (eval env loadPreludeForm) >>= \case
    Left err -> return $ Just err
    Right _ -> return Nothing


runRepl :: IO ()
runRepl = do
  env <- primitiveEnv
  evalPrelude env >>= \case
    Just err -> print err
    Nothing -> loop readFromPrompt (evalAndPrint env)
  where
    loop :: Monad m => m a -> (a -> m ()) -> m ()
    loop getNext action = do
      val <- getNext
      action val
      loop getNext action

    readFromPrompt :: IO SchemeValOrError
    readFromPrompt = do
      putStr ">>> " >> hFlush stdout
      getLine <&> readExpr "REPL"

    evalAndPrint :: Env -> SchemeValOrError -> IO ()
    evalAndPrint env expr = case expr of
      Left err -> print err
      Right exprs ->
        runExceptT (eval env exprs) >>= \case
          Left err -> print err
          Right val -> print val


evalFile :: FilePath -> IO ()
evalFile filename = do
  contents <- readFile filename
  env <- primitiveEnv
  evalPrelude env >>= \case
    Just err -> print err
    Nothing ->
      case readExprs filename contents of
        Left err -> print err
        Right exprs ->
          runExceptT (mapM_ (eval env) exprs) >>= \case
            Left err -> print err
            Right _ -> return ()


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    [filename] -> evalFile filename
    _ -> putStrLn "Bad CLI args"
