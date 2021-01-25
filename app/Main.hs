{-# LANGUAGE LambdaCase #-}
module Main where
import qualified Text.ParserCombinators.Parsec as P
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Control.Monad.Except (runExceptT, throwError)

import Parser (parseExpr, parseExprs)
import Primitives (primitives, ioPrimitives)
import Env (extendWith, nullEnv)
import Eval (eval)
import Types
  ( Env
  , SchemeVal
  , SchemeValOrError
  , SchemeError (ParseError)
  )


primitiveEnv :: IO Env
primitiveEnv = nullEnv
           >>= extendWith primitives
           >>= extendWith ioPrimitives

readOrThrow :: P.Parser a -> String -> String -> Either SchemeError a
readOrThrow parser streamName input =
  case P.parse parser streamName input of
    Left err -> throwError $ ParseError err
    Right val -> return val

readExpr :: String -> String -> SchemeValOrError
readExpr = readOrThrow parseExpr

readExprs :: String -> String -> Either SchemeError [SchemeVal]
readExprs = readOrThrow parseExprs

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
    readFromPrompt :: IO SchemeValOrError
    readFromPrompt = putStr ">>> "
                  >> hFlush stdout
                  >> getLine
                 >>= return . readExpr "REPL"

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
        Left err -> print err
        Right val -> print val


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    [filename] -> evalFile filename
    _ -> putStrLn "Bad CLI args"
