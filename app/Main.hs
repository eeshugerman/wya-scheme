{-# LANGUAGE LambdaCase #-}

module Main where
import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import Data.Char
import Numeric


data Sign = Plus | Minus

data Radix = Binary | Octal | Decimal | Hex

data LispVal = LispAtom String
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


-- an "identifier" according to https://www.scheme.com/tspl3/grammar.html
-- TODO: accept `-`, `+`, `...`
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol <|> char '#')
  return $ LispAtom $ first:rest
  where
    symbol :: Parser Char
    symbol = oneOf "!$%&|*/:<=>?@^_~"


parseBool :: Parser LispVal
parseBool = LispBool . \case {'t' -> True; 'f' -> False} <$>
  (char '#' >> (char 't' <|> char 'f'))


parseCharacter :: Parser LispVal  -- TODO: "named" characters, eg #\newline, #\space
parseCharacter = fmap LispCharacter $ string "#\\" >> anyChar


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


-- TODO: how to not abuse `try`?
parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> try parseBool
        <|> try parseCharacter
        <|> try parseComplex
        <|> try parseReal
        <|> parseInteger


readExpr :: String -> String
readExpr input = case parse parseExpr "[source]" input of
  Left err -> "No match: " ++ show err
  Right value -> "Found value: " ++ show value


main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)

