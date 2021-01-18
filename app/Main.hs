{-# LANGUAGE LambdaCase #-}
module Main where
import qualified Text.ParserCombinators.Parsec as P
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import Text.Pretty.Simple (pShow)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Monad.Except (runExceptT, throwError)

import Parser (parseExpr, parseExprs)
import Types
  ( LispVal
  , LispValOrError
  , LispError (ParseError)
  )
import Eval (eval)
import Env (nullEnv, Env)


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

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout


loop_ :: Monad m
  => m a          -- getNext
  -> (a -> m ())  -- action
  -> m ()
loop_ getNext action = getNext >>= action >> loop_ getNext action


runRepl :: IO ()
runRepl = do
  env <- nullEnv
  loop_ readFromPrompt (evalAndPrint env)
  where
    readFromPrompt :: IO LispValOrError
    readFromPrompt = flushStr ">>> " >> getLine >>= return . readExpr "REPL"

    evalAndPrint :: Env -> LispValOrError -> IO ()
    evalAndPrint env expr =
      case expr of
        Left err -> print err
        Right valid ->
          runExceptT (eval env valid) >>= \case
            Left err' -> print err'
            Right val' -> print val'


evalFile :: FilePath -> IO ()
evalFile filename = do
  contents <- readFile filename
  env <- nullEnv
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
