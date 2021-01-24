{-# LANGUAGE LambdaCase #-}
module Main where
import qualified Text.ParserCombinators.Parsec as P
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Control.Monad.Except (runExceptT, throwError)

import Parser (parseExpr, parseExprs)
import Eval (eval)
import Env (primitiveEnv)
import Types
  ( Env
  , LispVal
  , LispValOrError
  , LispError (ParseError)
  )

readExpr :: String -> String -> LispValOrError
readExpr streamName input =
  case P.parse parseExpr streamName input of
    Left err -> throwError $ ParseError err
    Right value -> return value

readExprs :: String -> String -> Either LispError [LispVal]
readExprs streamName input =
  case P.parse parseExprs streamName input of
    Left err -> throwError $ ParseError err
    Right exprs -> return exprs

loop_
  :: Monad m
  => m a          -- getNext
  -> (a -> m ())  -- action
  -> m ()
loop_ getNext action = getNext >>= action >> loop_ getNext action

runRepl :: IO ()
runRepl = do
  env <- primitiveEnv
  loop_ readFromPrompt (evalAndPrint env)
  where
    readFromPrompt :: IO LispValOrError
    readFromPrompt = putStr ">>> "
                  >> hFlush stdout
                  >> getLine
                 >>= return . readExpr "REPL"

    evalAndPrint :: Env -> LispValOrError -> IO ()
    evalAndPrint env expr = case expr of
      Left err -> print err
      Right valid ->
        runExceptT (eval env valid) >>= \case
          Left err' -> print err'
          Right val' -> print val'


evalFile :: FilePath -> IO ()
evalFile filename = do
  contents <- readFile filename
  env <- primitiveEnv
  case readExprs filename contents of
    Left err -> print err
    Right exprs -> mapM_ (evalAndPrint env) exprs
  where
    evalAndPrint :: Env -> LispVal -> IO ()
    evalAndPrint env expr =
      runExceptT (eval env expr) >>= \case
        Left err' -> print err'
        Right val' -> print val'


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    [filename] -> evalFile filename
    _ -> putStrLn "Bad CLI args"
