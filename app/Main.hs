{-# LANGUAGE LambdaCase #-}

module Main where
import qualified Text.ParserCombinators.Parsec as P
import System.Environment ( getArgs )
import Text.Pretty.Simple (pShow)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Parser ( parseExpr )
import Types ( LispVal )


strPShow :: LispVal -> String
strPShow = T.unpack . TL.toStrict . pShow

readExpr :: String -> String
readExpr input = case P.parse parseExpr "[source]" input of
  Left err -> "No match: " ++ show err
  Right value -> "Found value: " ++ strPShow value


main :: IO ()
main = do
  (filepath:_) <- getArgs
  lispCode <- readFile filepath
  putStrLn (readExpr lispCode)

-- testing / debug helpers
applyParser :: P.Parser LispVal -> String -> IO ()
applyParser parser input = putStrLn $ case P.parse parser "[test]" input of
  Left err -> "No match: " ++ show err
  Right value -> "Found value: " ++ strPShow value

printReadExpr :: String -> IO ()
printReadExpr input = putStrLn (readExpr input)

