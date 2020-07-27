{-# LANGUAGE LambdaCase #-}

module Main where
import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import Data.Char
import Numeric
import Text.Pretty.Simple (pShow)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL


data Sign = Plus | Minus

data Radix = Binary | Octal | Decimal | Hex

data LispVal = LispIdentifier String
             | LispBool Bool
             | LispCharacter Char
             | LispString String
             | LispInteger Integer
             | LispRational (Integer, Integer)
             | LispReal Float
             | LispComplex (LispVal, LispVal)
             | LispList [LispVal]
             | LispDottedList [LispVal] LispVal
             deriving Show


-- based on https://people.csail.mit.edu/jaffer/r5rs_9.html
parseIdentifier :: Parser LispVal
parseIdentifier = peculiarIdentifier <|> do
  first <- initial
  rest <- many (letter <|> digit <|> symbol <|> char '#')
  return $ LispIdentifier $ first:rest
  where
    initial :: Parser Char
    initial = letter <|> oneOf "!$%&*/:<=>?^_~"

    subsequent :: Parser Char
    subsequent = initial <|> digit <|> oneOf "+-.@"

    symbol :: Parser Char
    symbol = oneOf "!$%&|*/:<=>?@~_^"

    peculiarIdentifier :: Parser LispVal
    peculiarIdentifier = LispIdentifier <$>
      (string "+" <|> string "-" <|> try (string "..."))


parseBool :: Parser LispVal
parseBool = LispBool <$>
  (char '#' >> ((char 't' >> return True) <|>
                (char 'f' >> return False)))


parseCharacter :: Parser LispVal
parseCharacter = LispCharacter <$>
  (string "#\\"  >> (try (string "space" >> return ' ') <|>
                     try (string "newline" >> return '\n') <|>
                     anyChar))


parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escapeChar <|> noneOf "\"")
  char '"'
  return $ LispString x
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


parseSign :: Parser Sign
parseSign = \case {'+' -> Plus; '-' -> Minus} <$> option '+' (oneOf "+-")


applySign :: (Num a) => Sign -> a -> a
applySign sign mag = case sign of
    Plus ->  mag
    Minus -> -mag

parseInteger :: Parser LispVal
parseInteger = do
  radix <- option Decimal parseRadix
  sign <- parseSign
  LispInteger . applySign sign . readInt radix <$>
    many1 (digit <|> oneOf "abcdef")
  where
    readInt :: Radix -> String -> Integer
    readInt = \case
      Binary ->  readBinary
      Octal ->   fst . head . readOct
      Decimal -> read
      Hex ->     fst . head. readHex

    parseRadix :: Parser Radix
    parseRadix = do
      _ <- char '#'
      base <- oneOf "bBoOdDxX"
      return $ case toLower base of
        'b' -> Binary
        'o' -> Octal
        'd' -> Decimal
        'x' -> Hex

    readBinary :: String -> Integer
    readBinary = sum . applyBase2 0 . reverse . stringToInts
      where
        applyBase2 :: Integer -> [Integer] -> [Integer]
        applyBase2 _ [] = []
        applyBase2 idx (x:xs) = x * (2 ^ idx) : applyBase2 (idx + 1) xs

        stringToInts :: String -> [Integer]
        stringToInts s = [read [c] | c <- s]


parseRational :: Parser LispVal
parseRational = do
  sign <- parseSign
  numerator <- many1 digit
  char '/'
  denominator <- many1 digit
  return $ LispRational (applySign sign $ read numerator, read denominator)


-- TODO: This will accept `.`. Is that a problem?
-- TODO: #e / #i
parseReal :: Parser LispVal
parseReal = do
  sign <- parseSign
  whole <- option "0" (many1 digit)
  string "."
  fractional <- option "" (many1 digit)
  return . LispReal $ readFloat_ sign (whole ++ "." ++ fractional)
  where
    readFloat_ :: Sign -> String -> Float
    readFloat_ sign = applySign sign . fst . head . readFloat


parseComplex :: Parser LispVal
parseComplex = do
  real <- try parseReal <|> try parseRational <|> parseInteger
  imag <- try parseReal <|> try parseRational <|> parseInteger
  char 'i'
  return $ LispComplex (real, imag)


parseList :: Parser LispVal
parseList = LispList <$> sepBy parseExpr spaces


{-# ANN parseDottedList "HLint: ignore Use <$>" #-}
parseDottedList :: Parser LispVal
parseDottedList = do
  head <- sepBy parseExpr spaces
  spaces >> char '.' >> spaces
  tail <- parseExpr
  return $ LispDottedList head tail


parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  expr <- parseExpr
  return $ LispList [LispIdentifier "quote", expr]

parseUnquoted :: Parser LispVal
parseUnquoted = do
  char ','
  expr <- parseExpr
  return $ LispList [LispIdentifier "unquote", expr]

parseQuasiquoted :: Parser LispVal
parseQuasiquoted = do
  char '`'
  expr <- parseExpr
  return $ LispList [LispIdentifier "quasiquote", expr]

-- TODO: how to not abuse `try`?
parseExpr :: Parser LispVal
parseExpr = parseIdentifier
        <|> try parseBool
        <|> try parseCharacter
        <|> try parseComplex
        <|> try parseReal
        <|> parseInteger
        <|> parseQuoted
        <|> parseUnquoted
        <|> parseQuasiquoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

readExpr :: String -> String
readExpr input = case parse parseExpr "[source]" input of
  Left err -> "No match: " ++ show err
  Right value -> "Found value: " ++ (T.unpack . TL.toStrict $ pShow value)


main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)


-- testing / debug helpers
applyParser :: Parser String -> String -> String
applyParser parser input = case parse parser "[test]" input of
  Left err -> "No match: " ++ show err
  Right value -> "Found value: " ++ (T.unpack . TL.toStrict $ pShow value)

printReadExpr :: String -> IO ()
printReadExpr input = putStrLn (readExpr input)

