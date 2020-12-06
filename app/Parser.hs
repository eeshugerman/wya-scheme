{-# LANGUAGE LambdaCase #-}

module Parser where
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))
import Data.Char ( toLower )
import Data.Array (listArray)
import Numeric ( readFloat, readHex, readOct )

import Types
  ( Sign(Plus, Minus)
  , Radix(..)
  , LispVal(..)
  )

-- TODO: sort out naming convention -- what gets parse/read// prefix?

-- based on https://www.scheme.com/tspl4/grammar.html#Symbols
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
parseBool = LispBool <$>
  P.try (P.char '#' >> ((P.char 't' >> return True) <|>
                        (P.char 'f' >> return False)))

parseCharacter :: P.Parser LispVal
parseCharacter = let
  prefix = P.try $ P.string "#\\"
  namedChar name val =  P.try $ P.string name >> return val
  charName =  namedChar "space"   ' '
          <|> namedChar "newline" '\n'
          <|> namedChar "tab"     '\t'
          <|> P.anyChar
  in LispCharacter <$> (prefix >> charName)


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
applySign sign mag = case sign of {Plus ->  mag; Minus -> -mag}

parseInteger :: P.Parser LispVal
parseInteger = P.try $ do
  radix <- P.option Decimal parseRadix
  sign <- parseSign
  digits <- P.many1 $ allowedDigits radix
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

    allowedDigits :: Radix -> P.Parser Char
    allowedDigits = \case
      Binary ->  P.oneOf "01"
      Octal ->   P.oneOf "01234567"
      Decimal -> P.oneOf "0123456789"
      Hex ->     P.oneOf "0123456789abcdefABCDEF"


parseRational :: P.Parser LispVal
parseRational = P.try $ do
  sign <- parseSign
  numerator <- P.many1 P.digit
  P.char '/'
  denominator <- P.many1 P.digit
  return $ LispRational (applySign sign $ read numerator) (read denominator)


-- TODO: #e / #i
parseReal :: P.Parser LispVal
parseReal = P.try $ do
  sign <- parseSign
  whole <- P.many P.digit
  P.string "."
  fractional <- P.many P.digit
  if whole ++ fractional == ""
    then P.pzero   -- do a fail
    else let chars = (if whole == "" then "0" else whole) ++ "." ++ fractional
             mag = fst $ head $ readFloat chars
         in return $ LispReal $ applySign sign mag


parseComplex :: P.Parser LispVal
parseComplex = P.try $ do
  real <- P.try parseReal <|> P.try parseRational <|> parseInteger
  imag <- P.try parseReal <|> P.try parseRational <|> parseInteger
  P.char 'i'
  return $ LispComplex real imag

parseLispVals :: P.Parser [LispVal]
-- endBy is like sepBy except if there's seperator at the end it will be consumed
parseLispVals = parseExpr `P.endBy` P.spaces

parseMaybeDottedListEnd :: P.Parser (Maybe LispVal)
parseMaybeDottedListEnd =
  P.optionMaybe (P.char '.' >> P.skipMany1 P.space >> parseExpr)

parseListOrDottedList :: P.Parser LispVal
parseListOrDottedList = do
  P.char '('
  beginning <- parseLispVals
  maybeEnd <- parseMaybeDottedListEnd
  P.char ')'
  return $ case maybeEnd of
    Nothing -> LispList beginning
    Just end -> LispDottedList beginning end

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
  elems <- parseLispVals
  P.char ')'
  return $ LispVector $ listArray (0, length elems - 1) elems


parseExpr :: P.Parser LispVal
parseExpr = parseCharacter
        <|> parseBool
        <|> parseReal
        <|> parseRational
        <|> parseComplex
        <|> parseInteger
        <|> parseSymbol
        <|> parseVector
        <|> parseString
        <|> parseQuoted
        <|> parseUnquoted
        <|> parseQuasiquoted
        <|> parseListOrDottedList

