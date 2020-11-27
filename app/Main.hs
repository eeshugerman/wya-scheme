{-# LANGUAGE LambdaCase #-}

module Main where
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))
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


parseSymbol :: P.Parser LispVal
parseSymbol = LispSymbol <$> (peculiarSymbol <|> regularSymbol)
  where
    peculiarSymbol :: P.Parser String
    peculiarSymbol = P.string "+" <|> P.string "-" <|> P.try (P.string "...")

    regularSymbol :: P.Parser String
    regularSymbol = do
      first <- initial
      rest <- P.many subsequent
      return $ first:rest
      where
        initial :: P.Parser Char
        initial = P.letter <|> P.oneOf "!$%&*/:<=>?^_~"

        subsequent :: P.Parser Char
        subsequent = initial <|> P.digit <|> P.oneOf "+-.@"


parseBool :: P.Parser LispVal
parseBool = LispBool <$> P.try (
  P.char '#' >> ((P.char 't' >> return True) <|>
                 (P.char 'f' >> return False)))

parseCharacter :: P.Parser LispVal
parseCharacter = let
  spaceName = P.string "space" >> return ' '
  newlineName = P.string "newline" >> return '\n'
  prefix = P.try $ P.string "#\\"
  in LispCharacter <$> (prefix >> (spaceName <|> newlineName <|> P.anyChar))

parseString :: P.Parser LispVal
parseString = do
  P.char '"'
  x <- P.many (P.try escapeChar <|> P.noneOf "\"")
  P.char '"'
  return $ LispString x
  where
    escapeChar :: P.Parser Char
    escapeChar = do
      P.char '\\'
      x <- P.anyChar
      case x of
        '"'  -> return '"'
        'n'  -> return '\n'
        'r'  -> return '\r'
        't'  -> return '\t'
        '\\' -> return '\\'
        _    -> P.pzero


parseSign :: P.Parser Sign
parseSign = do
  sign <- P.option '+' (P.oneOf "+-")
  return $ case sign of
    '+' -> Plus
    '-' -> Minus
    val -> error $ "expected '+' or '-', got '" ++ [val] ++ "'"


applySign :: (Num a) => Sign -> a -> a
applySign sign mag = case sign of
    Plus ->  mag
    Minus -> -mag

parseInteger :: P.Parser LispVal
parseInteger = P.try $ do
  radix <- P.option Decimal parseRadix
  sign <- parseSign
  digits <- P.many1 (P.digit <|> P.oneOf "abcdef")
  return . LispInteger . applySign sign $ readInt radix digits

  where
    readInt :: Radix -> String -> Integer
    readInt = \case
      Binary ->  readBinary
      Octal ->   fst . head . readOct
      Decimal -> read
      Hex ->     fst . head. readHex

    parseRadix :: P.Parser Radix
    parseRadix = do
      P.char '#'
      base <- P.oneOf "bBoOdDxX"
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


parseRational :: P.Parser LispVal
parseRational = P.try $ do
  sign <- parseSign
  numerator <- P.many1 P.digit
  P.char '/'
  denominator <- P.many1 P.digit
  return $ LispRational (applySign sign $ read numerator, read denominator)


-- TODO: #e / #i
parseReal :: P.Parser LispVal
parseReal = P.try $ do
  sign <- parseSign
  whole <- P.many P.digit
  P.string "."
  fractional <- P.many P.digit
  if whole ++ fractional == ""
    then P.pzero
    else let chars = (if whole == "" then "0" else whole) ++ "." ++ fractional
             mag = fst $ head $ readFloat chars
         in return $ LispReal $ applySign sign mag


parseComplex :: P.Parser LispVal
parseComplex = P.try $ do
  real <- P.try parseReal <|> P.try parseRational <|> parseInteger
  imag <- P.try parseReal <|> P.try parseRational <|> parseInteger
  P.char 'i'
  return $ LispComplex (real, imag)


parseList :: P.Parser LispVal
parseList = LispList <$> P.sepBy parseExpr P.spaces


{-# ANN parseDottedList "HLint: ignore Use <$>" #-}
parseDottedList :: P.Parser LispVal
parseDottedList = do
  rest <- P.sepBy parseExpr P.spaces
  P.spaces >> P.char '.' >> P.spaces
  end <- parseExpr
  return $ LispDottedList rest end

parseQuoted :: P.Parser LispVal
parseQuoted = do
  expr <- P.char '\'' >> parseExpr
  return $ LispList [LispSymbol "quote", expr]

parseUnquoted :: P.Parser LispVal
parseUnquoted = do
  expr <- P.char ',' >> parseExpr
  return $ LispList [LispSymbol "unquote", expr]

parseQuasiquoted :: P.Parser LispVal
parseQuasiquoted = do
  expr <- P.char '`' >> parseExpr
  return $ LispList [LispSymbol "quasiquote", expr]

parseVector :: P.Parser LispVal
parseVector = do
  P.string "#("
  LispList elems <- parseList
  P.char ')'
  return $ LispVector $ listArray (0, length elems - 1) elems


parseExpr :: P.Parser LispVal
parseExpr = parseSymbol
        <|> parseCharacter
        <|> parseBool
        <|> parseReal
        <|> parseRational
        <|> parseComplex
        <|> parseInteger
        <|> parseVector
        <|> parseString
        <|> parseQuoted
        <|> parseUnquoted
        <|> parseQuasiquoted
        <|> do P.char '('
               elems <- P.try parseList <|> parseDottedList
               P.char ')'
               return elems

readExpr :: String -> String
readExpr input = case P.parse parseExpr "[source]" input of
  Left err -> "No match: " ++ show err
  Right value -> "Found value: " ++ (T.unpack . TL.toStrict $ pShow value)


main :: IO ()
main = do
  (filepath:_) <- getArgs
  lispCode <- readFile filepath
  putStrLn (readExpr lispCode)


-- testing / debug helpers
applyParser :: P.Parser LispVal -> String -> IO ()
applyParser parser input = putStrLn $ case P.parse parser "[test]" input of
  Left err -> "No match: " ++ show err
  Right value -> "Found value: " ++ (T.unpack . TL.toStrict $ pShow value)

printReadExpr :: String -> IO ()
printReadExpr input = putStrLn (readExpr input)

