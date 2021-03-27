{-# LANGUAGE LambdaCase #-}
module Main where
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Control.Monad.Except (runExceptT)
import Data.Functor ((<&>))

import Parser (readExpr, readExprs)
import Primitives (primitives, ioPrimitives)
import Env (extendWith, nullEnv)
import Eval (eval)
import Types
  ( Env
  , SchemeVal
  , SchemeValOrError
  )


primitiveEnv :: IO Env
primitiveEnv = nullEnv
  >>= extendWith primitives
  >>= extendWith ioPrimitives

runRepl :: IO ()
runRepl = do
  env <- primitiveEnv
  loop readFromPrompt (evalAndPrint env)
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
      Left parserError -> print parserError
      Right valid ->
        runExceptT (eval env valid) >>= \case
          Left err -> print err
          Right val -> print val


evalFile :: FilePath -> IO ()
evalFile filename = do
  contents <- readFile filename
  env <- primitiveEnv
  case readExprs filename contents of
    Left err -> print err
    Right exprs -> mapM_ (evalAndPrint env) exprs
  where
    evalAndPrint :: Env -> SchemeVal -> IO ()
    evalAndPrint env expr =
      runExceptT (eval env expr) >>= \case
        Left err -> print err  -- TODO: stop evaluation
        Right _  -> return ()


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    [filename] -> evalFile filename
    _ -> putStrLn "Bad CLI args"
