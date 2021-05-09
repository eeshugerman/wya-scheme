{-# LANGUAGE LambdaCase #-}
module Primitives ( primitives ) where

import qualified Data.Bifunctor
import qualified System.IO as IO
import qualified GHC.IO.Handle
import Data.Functor ((<&>))
import Control.Monad.Except ( throwError, liftIO )
import qualified Text.Parsec.Pos as Parsec

import Types
  ( IOSchemeValOrError
  , SchemeNumber (..)
  , SchemeVal (..)
  , SchemeError (..)
  , SchemeValOrError, SchemeReal
  )
import Eval (apply, liftThrows)
import Parser (readExprWithPos)

primitives :: [(String, SchemeVal)]
primitives = map (Data.Bifunctor.second SPrimativeProc)
  [ ("+",         numericFoldableOp (+))
  , ("-",         numericFoldableOp (-))
  , ("*",         numericFoldableOp (*))
  , ("/",         numericFoldableOp (/))

  , ("=",           numEqBoolBinOp (==))
  , ("/=",          numEqBoolBinOp (/=))
  , ("<",           numOrdBoolBinOp (<))
  , (">",           numOrdBoolBinOp (>))
  , (">=",          numOrdBoolBinOp (>=))
  , ("<=",          numOrdBoolBinOp (<=))

  , ("symbol?",      isTypeOp isSymbol)
  , ("boolean?",     isTypeOp isBool)
  , ("character?",   isTypeOp isChar)
  , ("string?",      isTypeOp isString)
  , ("number?",      isNumTypeOp isNumber)
  , ("complex?",     isNumTypeOp isComplex)
  , ("real?",        isNumTypeOp isReal)
  , ("rational?",    isNumTypeOp isRational)
  , ("integer?",     isNumTypeOp isInteger)
  , ("list?",        isTypeOp isList)
  , ("vector?",      isTypeOp isVector)
  , ("pair?",        isTypeOp isPair)
  , ("port?",        isTypeOp isPort)
  , ("procedure?",   isTypeOp isProcedure)

  , ("symbol->string", symbolToString)
  , ("string->symbol", stringToSymbol)

  , ("and",         boolBoolBinOp (&&))
  , ("or",          boolBoolBinOp (||))
  , ("string=?",    strBoolBinOp (==))
  , ("string<?",    strBoolBinOp (<))
  , ("string>?",    strBoolBinOp (>))
  , ("string<=?",   strBoolBinOp (<=))
  , ("string>=?",   strBoolBinOp (>=))

  , ("car",         car)
  , ("cdr",         cdr)
  , ("cons",        cons)

  , ("eqv?",        eqv)
  , ("eq?",         eq)
  , ("equal?",      equal)
  ]
  ++
  map (Data.Bifunctor.second SPrimativeProc)
  [ ("current-input-port",  getPortProc currentInputHdl)
  , ("current-output-port", getPortProc currentOutputHdl)
  , ("current-error-port",  getPortProc currentErrorHdl)
  ]
  ++
  map (Data.Bifunctor.second SIOProc)
  [ ("apply",              wrappedApply)

  , ("open-input-file",    makePort IO.ReadMode)
  , ("open-output-file",   makePort IO.WriteMode)
  , ("close-input-file",   closePort)
  , ("close-output-file",  closePort)

  , ("read-char",          readChar)
  , ("peek-char",          peekChar)
  , ("read",               read_)

  , ("write",              write)
  , ("write-string",       writeString)
  ]

------------------------------------------
-- helpers (not actual scheme primitives)
------------------------------------------
numericFoldableOp
  :: (SchemeNumber -> SchemeNumber -> SchemeNumber)
  -> ([SchemeVal] -> SchemeValOrError )
numericFoldableOp _ []              = throwError $ NumArgs 2 []
numericFoldableOp _ singleArg@[_]   = throwError $ NumArgs 2 singleArg
numericFoldableOp op (first:rest)   = foldlM wrappedOp first rest
  where
    foldlM _ acc [] = return acc
    foldlM f acc (x:xs) = do
      acc' <- f acc x
      foldlM f acc' xs

    wrappedOp :: SchemeVal -> SchemeVal -> SchemeValOrError
    wrappedOp (SchemeNumber a) (SchemeNumber b) = return $ SchemeNumber $ op a b
    wrappedOp a                (SchemeNumber _) = throwError $ TypeMismatch "number" a
    wrappedOp (SchemeNumber _) b                = throwError $ TypeMismatch "number" b
    wrappedOp a                _                = throwError $ TypeMismatch "number" a

boolBinOp
  :: (SchemeVal -> Either SchemeError a)  -- unpacker
  -> (a -> a -> Bool)                     -- op
  -> [SchemeVal]                          -- args
  -> SchemeValOrError                     -- result
boolBinOp unpacker op args =
  if length args /= 2
  then throwError $ NumArgs 2 args
  else do
    left <- unpacker $ head args
    right <- unpacker $ args !! 1
    return $ SBool $ left `op` right

type BoolBinOpBuilder a =
  (a -> a -> Bool) -> ([SchemeVal] -> SchemeValOrError)

numEqBoolBinOp :: BoolBinOpBuilder SchemeNumber
numEqBoolBinOp = boolBinOp unpacker
  where
    unpacker :: SchemeVal -> Either SchemeError SchemeNumber
    unpacker (SchemeNumber val) = return val
    unpacker nonNum = throwError $ TypeMismatch "number" nonNum

numOrdBoolBinOp :: BoolBinOpBuilder SchemeReal
numOrdBoolBinOp  = boolBinOp unpacker
  where
    unpacker :: SchemeVal -> Either SchemeError SchemeReal
    unpacker (SchemeNumber (SchemeReal val))  = return val
    unpacker val                              = throwError $ TypeMismatch "real" val

boolBoolBinOp :: BoolBinOpBuilder Bool
boolBoolBinOp = boolBinOp unpacker
  where
    unpacker :: SchemeVal -> Either SchemeError Bool
    unpacker (SBool val) = return val
    unpacker nonBool = throwError $ TypeMismatch "boolean" nonBool

strBoolBinOp :: BoolBinOpBuilder String
strBoolBinOp = boolBinOp unpacker
  where
    unpacker :: SchemeVal -> Either SchemeError String
    unpacker (SString val) = return val
    unpacker nonString = throwError $ TypeMismatch "string" nonString

isTypeOp
  :: (SchemeVal -> Bool)
  -> ([SchemeVal] -> SchemeValOrError)
isTypeOp test = \case
  [arg] -> return $ SBool $ test arg
  args  -> throwError $ NumArgs 1 args

-- can this be implemented as a wrapper around isTypeOp?
isNumTypeOp
  :: (SchemeNumber -> Bool)
  -> ([SchemeVal] -> SchemeValOrError)
isNumTypeOp test = \case
  [SchemeNumber num] -> return $ SBool $ test num
  [_]                -> return $ SBool False
  args               -> throwError $ NumArgs 1 args


-----------------------------------------
-- type testing
-----------------------------------------

isSymbol :: SchemeVal -> Bool
isSymbol (SSymbol _) = True
isSymbol _           = False

isBool :: SchemeVal -> Bool
isBool (SBool _) = True
isBool _         = False

isChar :: SchemeVal -> Bool
isChar (SChar _) = True
isChar _         = False

isString :: SchemeVal -> Bool
isString (SString _) = True
isString _           = False

-- numbers
isNumber :: SchemeNumber -> Bool
isNumber = const True

isComplex :: SchemeNumber -> Bool
isComplex = const True

isReal :: SchemeNumber -> Bool
isReal (SComplex _)   = False
isReal val            = isComplex val

isRational :: SchemeNumber -> Bool
isRational (SReal _ ) = False
isRational val        = isReal val

isInteger :: SchemeNumber -> Bool
isInteger (SRational _) = False
isInteger val           = isRational val

-- end numbers

isList :: SchemeVal -> Bool
isList (SList _) = True
isList _         = False

isVector :: SchemeVal -> Bool
isVector (SVector _) = True
isVector _           = False

isPair :: SchemeVal -> Bool
isPair (SList _)         = True
isPair (SDottedList _ _) = True
isPair _                 = False

isPort :: SchemeVal -> Bool
isPort (SPort _) = True
isPort _         = False

isProcedure :: SchemeVal -> Bool
isProcedure (SPrimativeProc _) = True
isProcedure (SIOProc _)        = True
isProcedure (SProc _)          = True
isProcedure _                  = False

-----------------------------------------
-- type conversion
-----------------------------------------

symbolToString :: [SchemeVal] -> SchemeValOrError
symbolToString [SSymbol arg] = return $ SString arg
symbolToString [arg]         = throwError $ TypeMismatch "symbol" arg
symbolToString args          = throwError $ NumArgs 1 args

stringToSymbol :: [SchemeVal] -> SchemeValOrError
stringToSymbol [SString arg] = return $ SSymbol arg
stringToSymbol [arg]         = throwError $ TypeMismatch "string" arg
stringToSymbol args          = throwError $ NumArgs 1 args


-----------------------------------------
-- list ops
-----------------------------------------

car :: [SchemeVal] -> SchemeValOrError
car [arg] = case arg of
  SList (x:_)             -> return x
  empty@(SList [])        -> throwError $ TypeMismatch "pair" empty
  SDottedList (x:_) _     -> return x
  wtf@(SDottedList [] _)  -> throwError $ TypeMismatch "pair" wtf
  val                     -> throwError $ TypeMismatch "pair" val
car args                   = throwError $ NumArgs 1 args

cdr :: [SchemeVal] -> SchemeValOrError
cdr [arg] = case arg of
  SList (_:xs)              -> return $ SList xs
  empty@(SList [])          -> throwError $ TypeMismatch "pair" empty
  SDottedList [_] bloop     -> return bloop
  SDottedList (_:xs) bloop  -> return $ SDottedList xs bloop
  SDottedList _ bloop       -> return bloop
  val                       -> throwError  $ TypeMismatch "pair" val
cdr args                     = throwError $ NumArgs 1 args

cons :: [SchemeVal] -> SchemeValOrError
cons [x, SList xs]                = return $ SList (x:xs)
cons [x, SDottedList bleep bloop] = return $ SDottedList (x:bleep) bloop
cons [a, b]                       = return $ SDottedList [a] b
cons args                         = throwError $ NumArgs 2 args

-----------------------------------------
-- equality
-----------------------------------------

-- ... a most unpermissive eq?
_eq :: SchemeVal -> SchemeVal -> Bool
_eq (SchemeVal (Just tagA) _) (SchemeVal (Just tagB) _) = tagA == tagB
_eq _ _ = False

eq :: [SchemeVal] -> SchemeValOrError
eq [a, b] = return $ SBool $ _eq a b
eq args   = throwError $ NumArgs 2 args

_eqv :: SchemeVal -> SchemeVal -> Bool
_eqv a b = case (a, b) of
  (SSymbol a',      SSymbol b') -> a' == b'
  (SChar a',        SChar b')   -> a' == b'
  (SString a',      SString b') -> a' == b'
  (SBool a',        SBool b')   -> a' == b'
  (SchemeNumber a', SchemeNumber b') ->
    case (a', b') of
      (SInteger a'',  SInteger b'')  -> a'' == b''
      (SRational a'', SRational b'') -> a'' == b''
      (SReal a'',     SReal b'')     -> a'' == b''
      (SComplex a'',  SComplex b'')  -> a'' == b''
      (_, _)                         -> False

  (a', b') -> _eq a' b'


eqv :: [SchemeVal] -> SchemeValOrError
eqv [a, b] = return $ SBool $ _eqv a b
eqv args   = throwError $ NumArgs 2 args

_equal :: SchemeVal -> SchemeVal -> Bool
_equal a b = case (a, b) of
  (SList a', SList b') -> allEqual a' b'
  (SDottedList aBleep aBloop, SDottedList bBleep bBloop) ->
    allEqual aBleep bBleep && _equal aBloop bBloop
  (_, _) -> _eqv a b
  where
    allEqual x y = let
      sameLength = length x == length y
      sameElems = all (==True) (zipWith _equal x y)
      in sameLength && sameElems

equal :: [SchemeVal] -> SchemeValOrError
equal [a, b]            = return $ SBool $ _equal a b
equal args              = throwError $ NumArgs 2 args

-----------------------------------------
-- io
-----------------------------------------
-- TODO: these should be user-overridable (preferably as r7rs parameters)
currentInputHdl :: IO.Handle
currentInputHdl = IO.stdin

currentOutputHdl :: IO.Handle
currentOutputHdl = IO.stdout

currentErrorHdl :: IO.Handle
currentErrorHdl = IO.stderr

getPortProc :: IO.Handle -> ([SchemeVal] -> SchemeValOrError)
getPortProc port = \case
  []   -> return $ SPort port
  args -> throwError $ NumArgs 0 args

wrappedApply :: [SchemeVal] -> IOSchemeValOrError
wrappedApply [proc', SList args] = apply proc' args
wrappedApply [_,     nonList]    = throwError $ TypeMismatch "list" nonList
wrappedApply badArgs             = throwError $ NumArgs 2 badArgs

makePort :: IO.IOMode -> [SchemeVal] -> IOSchemeValOrError
makePort mode [SString filename] = fmap SPort $ liftIO $ IO.openFile filename mode
makePort _    [nonString]        = throwError $ TypeMismatch "string" nonString
makePort _    badArgs            = throwError $ NumArgs 1 badArgs

closePort :: [SchemeVal] -> IOSchemeValOrError
closePort [SPort handle] = liftIO $ IO.hClose handle >> return (SList [])
closePort [nonPort]      = throwError $ TypeMismatch "port" nonPort
closePort badArgs        = throwError $ NumArgs 1 badArgs

_inputPortProc
  :: (IO.Handle -> IO SchemeVal)
  -> ([SchemeVal] -> IOSchemeValOrError)
_inputPortProc f = \case
  []             -> liftIO $ f currentInputHdl
  [SPort handle] -> liftIO $ f handle
  [nonPort]      -> throwError $ TypeMismatch "port" nonPort
  badArgs        -> throwError $ NumArgs 2 badArgs

_outputPortProc
  :: (IO.Handle -> SchemeVal -> IOSchemeValOrError)
  -> ([SchemeVal] -> IOSchemeValOrError)
_outputPortProc f = \case
  [val]               -> f currentOutputHdl val >> return (SList [])
  [val, SPort handle] -> f handle val >> return (SList [])
  [_, nonPort]        -> throwError $ TypeMismatch "port" nonPort
  badArgs             -> throwError $ NumArgs 2 badArgs

readChar :: [SchemeVal] -> IOSchemeValOrError
readChar = _inputPortProc $ \handle -> IO.hGetChar handle <&> SChar

peekChar :: [SchemeVal] -> IOSchemeValOrError
peekChar = _inputPortProc $ \handle -> IO.hLookAhead handle <&> SChar

-- TODO: doesn't work with stdin :(
read_ :: [SchemeVal] -> IOSchemeValOrError
read_ = \case
  []             -> _read currentInputHdl
  [SPort handle] -> _read handle
  [nonPort]      -> throwError $ TypeMismatch "port" nonPort
  badArgs        -> throwError $ NumArgs 1 badArgs
  where
    -- this is a bit gruesome, but i can't find a better way to do it with
    -- parsec. attoparsec, on the other hand, has built-in support for
    -- incremental parsing
    _read :: IO.Handle  -> IOSchemeValOrError
    _read handle = do
      -- unfortunately there's no way around hGetContents closing the handle,
      -- so we work with a duplicate
      tempHandle <- liftIO $ GHC.IO.Handle.hDuplicate handle
      string <- liftIO $ IO.hGetContents tempHandle
      (pos, parsed) <- liftThrows $ readExprWithPos (show handle) string
      liftIO $ IO.hSeek handle IO.AbsoluteSeek (posToBytes pos string)
      liftIO $ IO.hClose tempHandle
      return parsed

    posToBytes :: Parsec.SourcePos -> String -> Integer
    posToBytes pos source = let
      precedingLines = take (Parsec.sourceLine pos - 1) (lines source)
      in fromIntegral $ length (unlines precedingLines) + Parsec.sourceColumn pos

writeString :: [SchemeVal] -> IOSchemeValOrError
writeString = _outputPortProc $ \handle val -> case val of
  SString string ->
    liftIO $ IO.hPutStr handle string >>
    return (SList [])
  nonString -> throwError $ TypeMismatch "string" nonString

write :: [SchemeVal] -> IOSchemeValOrError
write = _outputPortProc $ \handle val ->
    liftIO $ IO.hPutStr handle (show val) >>
    return (SList [])

