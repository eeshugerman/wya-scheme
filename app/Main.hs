module Main where
import qualified Text.ParserCombinators.Parsec as P
import System.Environment (getArgs)

import Text.Pretty.Simple (pShow)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Monad.Except (throwError)

import Parser (parseExpr)
import Types
  ( LispVal
  , LispValOrError
  , LispError (ParseError)
  )
import Evaluator (eval)


strPShow :: LispVal -> String
strPShow = T.unpack . TL.toStrict . pShow

readExpr :: String -> LispValOrError
readExpr input = case P.parse parseExpr "[source]" input of
  Left err -> throwError $ ParseError err
  Right value -> return value

main :: IO ()
-- main = getArgs >>= readFile . head >>= print . eval . readExpr

main = do
  (filename:_) <- getArgs
  sourceCode <- readFile filename
  let parsed = readExpr sourceCode
      evaled = parsed >>= eval
  print $ case evaled of
    Left err -> "Error: " ++ show err
    Right value -> show value


-- testing / debug helpers
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

