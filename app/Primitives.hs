{-# LANGUAGE LambdaCase #-}
module Primitives ( primitives ,ioPrimitives) where


import Control.Monad (zipWithM)
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

ioPrimitives :: [(String, SchemeVal)]
ioPrimitives = map (Data.Bifunctor.second SIOProc)
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
  -- TODO: default to current-input-port
  [SPort handle] -> liftIO $ f handle
  [nonPort]      -> throwError $ TypeMismatch "port" nonPort
  badArgs        -> throwError $ NumArgs 2 badArgs

_outputPortProc
  :: (IO.Handle -> SchemeVal -> IOSchemeValOrError)
  -> ([SchemeVal] -> IOSchemeValOrError)
_outputPortProc f = \case
  -- TODO: default to current-input-port
  [SPort handle, val] -> f handle val >> return (SList [])
  [nonPort, _]        -> throwError $ TypeMismatch "port" nonPort
  badArgs             -> throwError $ NumArgs 2 badArgs

readChar :: [SchemeVal] -> IOSchemeValOrError
readChar = _inputPortProc $ \handle -> IO.hGetChar handle <&> SChar

peekChar :: [SchemeVal] -> IOSchemeValOrError
peekChar = _inputPortProc $ \handle -> IO.hLookAhead handle <&> SChar

-- this is a bit gruesome, but i can't find a better way to do it with
-- parsec. attoparsec, on the other hand, has built-in support for
-- incremental parsing
read_ :: [SchemeVal] -> IOSchemeValOrError
read_ [SPort handle] = do
  -- unfortunately there's no way around hGetContents closing the handle,
  -- so we work with a duplicate
  tempHandle <- liftIO $ GHC.IO.Handle.hDuplicate handle
  string <- liftIO $ IO.hGetContents tempHandle
  (pos, parsed) <- liftThrows $ readExprWithPos (show handle) string
  liftIO $ IO.hSeek handle IO.AbsoluteSeek (posToBytes pos string)
  liftIO $ IO.hClose tempHandle
  return parsed
  where
    posToBytes :: Parsec.SourcePos -> String -> Integer
    posToBytes pos source = let
      precedingLines = take (Parsec.sourceLine pos - 1) (lines source)
      in fromIntegral $ length (unlines precedingLines) + Parsec.sourceColumn pos

read_ [nonPort] = throwError $ TypeMismatch "port" nonPort
read_ badArgs   = throwError $ NumArgs 1 badArgs

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


primitives :: [(String, SchemeVal)]
primitives = map (Data.Bifunctor.second SPrimativeProc)
  [ ("+",         numericFoldableOp (+))
  , ("-",         numericFoldableOp (-))
  , ("*",         numericFoldableOp (*))
  , ("/",         numericFoldableOp (/))

  -- , ("mod",       numericFoldableOp mod)
  -- , ("quotient",  numericFoldableOp quot)
  -- , ("remainder", numericFoldableOp rem)

  , ("=",           numEqBoolBinOp (==))
  , ("/=",          numEqBoolBinOp (/=))
  , ("<",           numOrdBoolBinOp (<))
  , (">",           numOrdBoolBinOp (>))
  , (">=",          numOrdBoolBinOp (>=))
  , ("<=",          numOrdBoolBinOp (<=))

  , ("symbol?",      isTypeOp isSymbol)
  , ("boolean?",     isTypeOp isBoolean)
  , ("character?",   isTypeOp isCharacter)
  , ("string?",      isTypeOp isString)
  , ("list?",        isTypeOp isList)
  , ("vector?",      isTypeOp isVector)
  , ("number?",      isNumTypeOp isNumber)
  , ("complex?",     isNumTypeOp isComplex)
  , ("real?",        isNumTypeOp isReal)
  , ("rational?",    isNumTypeOp isRational)
  , ("integer?",     isNumTypeOp isInteger)

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
  , ("equal?",      equal)
  ]

------------------------------------------
-- helpers (not actual scheme primitives)
------------------------------------------
numericFoldableOp
  :: (SchemeNumber -> SchemeNumber -> SchemeNumber)
  -> ([SchemeVal] -> SchemeValOrError )
numericFoldableOp _ []              = throwError $ NumArgs 2 []
numericFoldableOp _ singleArg@[_]   = throwError $ NumArgs 2 singleArg
numericFoldableOp op args           = foldl1M wrappedOp args
  where
    foldl1M f (x:xs) = foldlM f x xs
    foldl1M _ [] = error "internal error in foldl1M"

    foldlM _ acc [] = return acc
    foldlM f acc (x:xs) = f acc x >>= \ acc' -> foldlM f acc' xs

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

isBoolean :: SchemeVal -> Bool
isBoolean (SBool _) = True
isBoolean _         = False

isCharacter :: SchemeVal -> Bool
isCharacter (SChar _) = True
isCharacter _         = False

isString :: SchemeVal -> Bool
isString (SString _) = True
isString _           = False

isList :: SchemeVal -> Bool
isList (SList _) = True
isList _         = False

isVector :: SchemeVal -> Bool
isVector (SVector _) = True
isVector _           = False

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
car []                     = throwError $ NumArgs 1 []
car [arg] = case arg of
  SList (x:_)             -> return x
  empty@(SList [])        -> throwError $ TypeMismatch "pair" empty
  SDottedList (x:_) _     -> return x
  wtf@(SDottedList [] _)  -> throwError $ TypeMismatch "pair" wtf
  val                     -> throwError $ TypeMismatch "pair" val
car args                   = throwError $ NumArgs 1 args

cdr :: [SchemeVal] -> SchemeValOrError
cdr empty@[]                 = throwError $ NumArgs 1 empty
cdr [arg] = case arg of
  SList (_:xs)              -> return $ SList xs
  empty@(SList [])          -> throwError $ TypeMismatch "pair" empty
  SDottedList [_] bloop     -> return bloop
  SDottedList (_:xs) bloop  -> return $ SDottedList xs bloop
  SDottedList _ bloop       -> return bloop
  val                       -> throwError  $ TypeMismatch "pair" val
cdr args                     = throwError $ NumArgs 1 args

cons :: [SchemeVal] -> SchemeValOrError
cons []                           = throwError $ NumArgs 2 []
cons singleArg@[_]                = throwError $ NumArgs 2 singleArg
cons [x, SList xs]                = return $ SList (x:xs)
cons [x, SDottedList bleep bloop] = return $ SDottedList (x:bleep) bloop
cons [a, b]                       = return $ SDottedList [a] b
cons args                         = throwError $ NumArgs 2 args

-----------------------------------------
-- equality
-----------------------------------------

-- TODO
-- eq :: [SchemeVal] -> SchemeValOrError

eqv :: [SchemeVal] -> SchemeValOrError
eqv []                = throwError $ NumArgs 2 []
eqv singleArg@[_]     = throwError $ NumArgs 2 singleArg
eqv [a, b] = return $ SBool $ case (a, b) of
 (SSymbol a',    SSymbol b')    -> a' == b'
 (SChar a', SChar b')           -> a' == b'
 (SString a',    SString b')    -> a' == b'
 (SBool a',      SBool b')      -> a' == b'
 (SchemeNumber a',    SchemeNumber b')    ->
   case (a', b') of
     (SInteger a'',  SInteger b'')  -> a'' == b''
     (SRational a'', SRational b'') -> a'' == b''
     (SReal a'',     SReal b'')     -> a'' == b''
     (SComplex a'',  SComplex b'')  -> a'' == b''
     (_, _)                         -> False
 (_, _)                         -> error "not implemented" -- TODO: iterables
eqv args              = throwError $ NumArgs 2 args


-- TODO: this is gross
_isTrue :: SchemeVal -> Bool
_isTrue (SBool val) = val
_isTrue _ = False


equalLists :: [SchemeVal] -> [SchemeVal] -> SchemeValOrError
equalLists a b =
    let allTrue :: [SchemeVal] -> SchemeVal
        allTrue = SBool . all _isTrue
        lispBools = zipWithM (\ a' b' -> equal [a', b']) a b
    in fmap allTrue lispBools


equal :: [SchemeVal] -> SchemeValOrError
equal []                = throwError $ NumArgs 2 []
equal singleArg@[_]     = throwError $ NumArgs 2 singleArg
equal [a, b] = case (a, b) of
  (SList a', SList b') -> equalLists a' b'

  (SDottedList aBleep aBloop, SDottedList bBleep bBloop) -> do
    bleepsEqual <- equalLists aBleep bBleep
    bloopsEqual <- equal [aBloop, bBloop]
    return $ SBool $ _isTrue bleepsEqual && _isTrue bloopsEqual

  (_, _) -> eqv [a, b]

equal args              = throwError $ NumArgs 2 args
