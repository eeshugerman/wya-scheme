{-# LANGUAGE LambdaCase #-}

module Main where
import Text.ParserCombinators.Parsec
import System.Environment
import Data.Char
import Numeric hiding (readInt)
import Data.Array hiding (elems)
import Text.Pretty.Simple (pShow)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL


data Sign = Plus | Minus

data Radix = Binary | Octal | Decimal | Hex

data LispVal = LispSymbol String
             | LispBool Bool
             | LispCharacter Char
             | LispString String
             | LispInteger Integer
             | LispRational (Integer, Integer)
             | LispReal Float
             | LispComplex (LispVal, LispVal)
             | LispList [LispVal]
             | LispVector (Array Int LispVal)
             | LispDottedList [LispVal] LispVal
             deriving Show


parseSymbol :: Parser LispVal
parseSymbol = LispSymbol <$> (peculiarSymbol <|> regularSymbol)
  where
    peculiarSymbol :: Parser String
    peculiarSymbol = string "+" <|> string "-" <|> try (string "...")

    regularSymbol :: Parser String
    regularSymbol = do
      first <- initial
      rest <- many subsequent
      return $ first:rest
      where
        initial :: Parser Char
        initial = letter <|> oneOf "!$%&*/:<=>?^_~"

        subsequent :: Parser Char
        subsequent = initial <|> digit <|> oneOf "+-.@"


parseBool :: Parser LispVal
parseBool = LispBool <$>
  try (char '#' >> ((char 't' >> return True) <|>
                    (char 'f' >> return False)))

parseCharacter :: Parser LispVal
parseCharacter = let
  spaceName = string "space" >> return ' '
  newlineName = string "newline" >> return '\n'
  prefix = try $ string "#\\"
  in LispCharacter <$> (prefix >> (spaceName <|> newlineName <|> anyChar))

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (try escapeChar <|> noneOf "\"")
  char '"'
  return $ LispString x
  where
    escapeChar :: Parser Char
    escapeChar = do
      char '\\'
      x <- anyChar
      case x of
        '"'  -> return '"'
        'n'  -> return '\n'
        'r'  -> return '\r'
        't'  -> return '\t'
        '\\' -> return '\\'
        _    -> pzero


parseSign :: Parser Sign
parseSign = do
  sign <- option '+' (oneOf "+-")
  return $ case sign of
    '+' -> Plus
    '-' -> Minus
    val -> error $ "expected '+' or '-', got '" ++ [val] ++ "'"


applySign :: (Num a) => Sign -> a -> a
applySign sign mag = case sign of
    Plus ->  mag
    Minus -> -mag

parseInteger :: Parser LispVal
parseInteger = do
  radix <- option Decimal parseRadix
  sign <- parseSign
  digits <- many1 (digit <|> oneOf "abcdef")
  return . LispInteger . applySign sign $ readInt radix digits

  where
    readInt :: Radix -> String -> Integer
    readInt = \case
      Binary ->  readBinary
      Octal ->   fst . head . readOct
      Decimal -> read
      Hex ->     fst . head. readHex

    parseRadix :: Parser Radix
    parseRadix = do
      char '#'
      base <- oneOf "bBoOdDxX"
      return $ case toLower base of
        'b' -> Binary
        'o' -> Octal
        'd' -> Decimal
        'x' -> Hex
        val -> error $ "expected oneOf \"bodx\", got '" ++ [val] ++ "'"

    readBinary :: String -> Integer
    readBinary chars = let
      digits = reverse [read [c] | c <- chars]
      terms = zipWith (\idx d -> d * (2 ^ (idx::Integer))) [0..] digits
      in sum terms


parseRational :: Parser LispVal
parseRational = do
  sign <- parseSign
  numerator <- many1 digit
  char '/'
  denominator <- many1 digit
  return $ LispRational (applySign sign $ read numerator, read denominator)


-- TODO: #e / #i
parseReal :: Parser LispVal
parseReal = do
  sign <- parseSign
  whole <- many digit
  string "."
  fractional <- many digit
  if whole ++ fractional == ""
    then pzero
    else let chars = (if whole == "" then "0" else whole) ++ "." ++ fractional
             mag = fst $ head $ readFloat chars
         in return $ LispReal $ applySign sign mag


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
  rest <- sepBy parseExpr spaces
  spaces >> char '.' >> spaces
  end <- parseExpr
  return $ LispDottedList rest end

parseQuoted :: Parser LispVal
parseQuoted = do
  expr <- char '\'' >> parseExpr
  return $ LispList [LispSymbol "quote", expr]

parseUnquoted :: Parser LispVal
parseUnquoted = do
  expr <- char ',' >> parseExpr
  return $ LispList [LispSymbol "unquote", expr]

parseQuasiquoted :: Parser LispVal
parseQuasiquoted = do
  expr <- char '`' >> parseExpr
  return $ LispList [LispSymbol "quasiquote", expr]

parseVector :: Parser LispVal
parseVector = do
  string "#("
  LispList elems <- parseList
  char ')'
  return $ LispVector $ listArray (0, length elems - 1) elems


parseExpr :: Parser LispVal
parseExpr = parseSymbol
        <|> parseCharacter
        <|> parseBool
        <|> try parseComplex
        <|> try parseReal
        <|> try parseInteger
        <|> parseVector
        <|> parseString
        <|> parseQuoted
        <|> parseUnquoted
        <|> parseQuasiquoted
        <|> do char '('
               elems <- try parseList <|> parseDottedList
               char ')'
               return elems

readExpr :: String -> String
readExpr input = case parse parseExpr "[source]" input of
  Left err -> "No match: " ++ show err
  Right value -> "Found value: " ++ (T.unpack . TL.toStrict $ pShow value)


main :: IO ()
main = do
  (filepath:_) <- getArgs
  lispCode <- readFile filepath
  putStrLn (readExpr lispCode)


-- testing / debug helpers
applyParser :: Parser LispVal -> String -> IO ()
applyParser parser input = putStrLn $ case parse parser "[test]" input of
  Left err -> "No match: " ++ show err
  Right value -> "Found value: " ++ (T.unpack . TL.toStrict $ pShow value)

printReadExpr :: String -> IO ()
printReadExpr input = putStrLn (readExpr input)

