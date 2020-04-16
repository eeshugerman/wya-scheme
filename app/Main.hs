module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving Show

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapeChar :: Parser Char
escapeChar = do
  char '\\'
  x <- oneOf ['"', 'n', 'r', 't', '\\']
  return $ case x of
    '"'  -> '"'
    'n'  -> '\n'
    'r'  -> '\r'
    't'  -> '\t'
    '\\' -> '\\'

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escapeChar <|> noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= (return . Number . read)

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right value -> "Found value: " ++ show value

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
