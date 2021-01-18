{-# LANGUAGE LambdaCase #-}
module Main where
import qualified Text.ParserCombinators.Parsec as P
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import Text.Pretty.Simple (pShow)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Monad.Except (runExceptT, throwError)

import Parser (parseExpr)
import Types
  ( LispVal
  , LispValOrError
  , LispError (ParseError)
  )
import Eval (eval)
import Env (nullEnv, Env)


readExpr :: String -> String -> LispValOrError
readExpr filename input = case P.parse parseExpr filename input of
  Left err -> throwError $ ParseError err
  Right value -> return value

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env str =
  case readExpr "REPL" str of
    Left err -> print err
    Right parsed ->
      runExceptT (eval env parsed) >>= \case
        Left err' -> print err'
        Right val' -> print val'

until_ :: Monad m
  => (a -> Bool) -- predicate
  -> m a         -- prompt
  -> (a -> m())  -- action
  -> m ()
until_ predicate prompt action = do
  result <- prompt
  if predicate result
    then return ()
    else action result >> until_ predicate prompt action

runRepl :: IO ()
runRepl = do
  env <- nullEnv
  until_ (== ",quit") (readPrompt ">>> ") (evalAndPrint env)

evalFile :: FilePath -> IO ()
evalFile filename = do
  contents <- readFile filename
  env <- nullEnv
  evalAndPrint env contents

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    [filename] -> evalFile filename
    _ -> putStrLn "Bad CLI args"


-- testing / debug helpers --------------------------------------------------
strPShow :: LispVal -> String
strPShow = T.unpack . TL.toStrict . pShow

debugReadExpr :: String -> String
debugReadExpr input = case P.parse parseExpr "[source]" input of
  Left err -> "No match: " ++ show err
  Right value -> strPShow value

applyParser :: P.Parser LispVal -> String -> IO ()
applyParser parser input = putStrLn $ case P.parse parser "[test]" input of
  Left err -> "No match: " ++ show err
  Right value -> "Found value: " ++ strPShow value

printReadExpr :: String -> IO ()
printReadExpr input = putStrLn $ debugReadExpr input
