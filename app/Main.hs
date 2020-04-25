module Main where
import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import Data.Char
import Numeric

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Character Char
             | Integer Integer
             | Float Float
             | String String
             | Bool Bool
             deriving Show


parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escapeChar <|> noneOf "\"")
  char '"'
  return $ String x
  where
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

parseBool :: Parser LispVal
parseBool = do
  _ <- char '#'
  val <- char 't' <|> char 'f'
  return $ case val of
    't' -> Bool True
    'f' -> Bool False

parseAtom :: Parser LispVal
parseAtom = try parseBool <|> do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol <|> char '#')
  return $ Atom $ first:rest
  where
    symbol :: Parser Char
    symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseCharacter :: Parser LispVal  -- TODO: "named" characters, eg #\newline, #\space
parseCharacter = string "#\\" >> anyChar >>= (return . Character)

parseInteger :: Parser LispVal
parseInteger = do
  radix <- parseRadix
  many1 (digit <|> oneOf "abcdef") >>= (  -- TODO: get more specific here
    return . Integer . case radix of
        'b' -> readBinary
        'o' -> fst . head . readOct
        'd' -> read
        'x' -> fst . head. readHex
    )
  where
    parseRadix :: Parser Char
    parseRadix = do { _ <- char '#'
                    ; base <- oneOf "bBoOdDxX"
                    ; return $ toLower base
                    } <|> return 'd'

    readBinary :: String -> Integer
    readBinary = sum . applyBase2 0 . reverse . stringToInts
      where
        applyBase2 :: Integer -> [Integer] -> [Integer]
        applyBase2 _ [] = []
        applyBase2 idx (x:xs) = x * (2 ^ idx) : applyBase2 (idx + 1) xs

        stringToInts :: String -> [Integer]
        stringToInts s = [read [c] | c <- s]


-- TODO: This will accept `.`. Is that a problem?
-- TODO: Seems like there should be a better way to do this... `sequence`?
-- TODO: #e / #i
parseFloat :: Parser LispVal
parseFloat = do
  whole <- option "0" (many1 digit)
  _ <- string "."
  fractional <- option "" (many1 digit)
  return . Float . fst . head $ readFloat (whole ++ "." ++ fractional)

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseCharacter
        <|> try parseFloat
        <|> parseInteger

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right value -> "Found value: " ++ show value

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)

