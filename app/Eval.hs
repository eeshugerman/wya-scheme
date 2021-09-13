{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Eval where

import Data.Maybe (isNothing)
import Control.Monad.Except (throwError, liftIO, ExceptT)

import Types
  ( IOSchemeValOrError
  , SchemeVal (..)
  , SchemeError (..)
  , Env
  , CallableSpec (..)
  )
import Env (getVar , defineVar , setVar, extendWith)
import Parser (readExprs)


---------------------------------------------------------------------------------
-- misc helpers
---------------------------------------------------------------------------------
nil :: SchemeVal
nil = SList []

liftThrows :: Either SchemeError a -> ExceptT SchemeError IO a
liftThrows = \case
  Left err -> throwError err
  Right val -> return val

---------------------------------------------------------------------------------
-- quasiquote stuff
---------------------------------------------------------------------------------
negativeQqDepth :: SchemeVal -> SchemeError
negativeQqDepth = BadForm "negative quasiquote depth"

data EitherSpliced a = Typical a | Spliced a

instance Functor EitherSpliced where
  fmap f = \case
    Typical val -> Typical $ f val
    Spliced val -> Spliced $ f val

-- TODO: BUG: this doesn't eval correctly: ``(,,@(list 1))
evalQuasiquoted :: Env -> SchemeVal -> IOSchemeValOrError
evalQuasiquoted env (SList list) = iter 1 [] list >>= \case
  Typical val -> return val
  Spliced val -> return $ UnquoteSplicing val
  where
    iter
      :: Integer      -- quasiquote depth
      -> [SchemeVal]  -- accumulator
      -> [SchemeVal]  -- remaining
      -> ExceptT SchemeError IO (EitherSpliced SchemeVal)

    iter _ acc [] = return $ Typical $ SList $ reverse acc

    iter depth _ [SSymbol "unquote", val] = case depth of
      0 -> throwError $ negativeQqDepth val
      1 -> Typical <$> eval env val
      _ -> case val of
        SList list' -> fmap Unquote <$> iter (depth - 1) [] list'
        nonList -> return $ Unquote <$> Typical nonList

    iter depth _ [SSymbol "unquote-splicing", val] = case depth of
      0 -> throwError $ negativeQqDepth val
      1 -> Spliced <$> eval env val
      _ -> case val of
        SList list' -> fmap UnquoteSplicing <$> iter (depth - 1) [] list'
        nonList -> return $ UnquoteSplicing <$> Typical nonList

    iter depth _ [SSymbol "quasiquote", val] = case val of
      SList list' -> fmap Quasiquote <$> iter (depth + 1) [] list'
      nonList -> return $ Quasiquote <$> Typical nonList

    iter depth acc (SList list':xs) = iter depth [] list' >>= \case
      Typical val -> iter depth (val:acc) xs
      Spliced (SList splicedList) -> iter depth (reverse splicedList ++ acc) xs
      Spliced splicedNonList -> throwError $ TypeMismatch "list" splicedNonList

    iter depth acc (x:xs) = iter depth (x:acc) xs

evalQuasiquoted _ val = return val

---------------------------------------------------------------------------------
-- proc/macro stuff
---------------------------------------------------------------------------------

callableSpec
  :: String          -- name
  -> Env             -- closure
  -> [SchemeVal]     -- params
  -> Maybe SchemeVal -- varParam
  -> [SchemeVal]     -- body
  -> Either SchemeError CallableSpec
callableSpec name closure params varParam body =
  do params' <- mapM symbolToString params
     varParam' <- mapM symbolToString varParam
     return $ CallableSpec closure params' varParam' body
     where
        symbolToString :: SchemeVal -> Either SchemeError String
        symbolToString = \case
          SSymbol val -> return val
          _ -> throwError $ BadForm "Invalid procedure definition"
                                    (SList $ SSymbol name : params)

apply :: SchemeVal -> [SchemeVal] -> IOSchemeValOrError

apply (SPrimativeProc proc') args = liftThrows $ proc' args
apply (SIOProc proc') args = proc' args

apply (SProc CallableSpec {..}) args = let
  numParams' = length cParams
  numParams  = toInteger numParams'
  numArgs    = toInteger $ length args
  in if (numParams > numArgs) ||
        (numParams < numArgs && isNothing cVarParam)
     then throwError $ NumArgs numParams args
     else let
       remainingArgs = drop numParams' args
       paramsArgsMap = zip cParams args ++ case cVarParam of
         Just varParamName -> [(varParamName, SList remainingArgs)]
         Nothing -> []
       in do
         procEnv <- liftIO $ extendWith paramsArgsMap cClosure
         last <$> mapM (eval procEnv) cBody


apply nonProc _ = throwError $ TypeMismatch "procedure" nonProc


pattern Lambda :: [SchemeVal] -> [SchemeVal] -> SchemeVal
pattern Lambda params body <- SList
  (SSymbol "lambda" : SList params : body)

pattern VariadicLambda
  :: [SchemeVal] -> SchemeVal -> [SchemeVal] -> SchemeVal
pattern VariadicLambda params varParam body <- SList
  ( SSymbol "lambda"
    : SDottedList params varParam
    : body
  )

pattern VariadicOnlyLambda
  ::  SchemeVal -> [SchemeVal] -> SchemeVal
pattern VariadicOnlyLambda varParam body <- SList
  ( SSymbol "lambda"
    : varParam
    : body
  )

pattern MacroDef
  :: String -> [SchemeVal] -> [SchemeVal] -> SchemeVal
pattern MacroDef name params body <- SList
  ( SSymbol "define-macro"
    : SList (SSymbol name : params)
    : body
  )

pattern VariadicMacroDef
  :: String -> [SchemeVal] -> SchemeVal -> [SchemeVal] -> SchemeVal
pattern VariadicMacroDef name params varParam body <- SList
  ( SSymbol "define-macro"
    : SDottedList (SSymbol name : params) varParam
    : body
  )

---------------------------------------------------------------------------------
-- eval
---------------------------------------------------------------------------------

eval :: Env -> SchemeVal -> IOSchemeValOrError
eval _   val@(SDottedList _ _) = throwError $ BadForm "Can't eval dotted list" val
eval _   val@(SVector _)       = throwError $ BadForm "Can't eval vector" val

eval _   val@(SBool _)         = return val
eval _   val@(SChar _)         = return val
eval _   val@(SString _)       = return val
eval _   val@(SchemeNumber _)  = return val

eval env (SSymbol varName)     = getVar env varName

eval _   (Quote val)           = return val
eval env (Quasiquote val)      = evalQuasiquoted env val
eval _   (Unquote val)         = throwError $ negativeQqDepth val
eval _   (UnquoteSplicing val) = throwError $ negativeQqDepth val

eval env (Lambda params body) = do
  liftThrows $ SProc <$> callableSpec
    "<lambda>" env params Nothing body

eval env (VariadicLambda params varParam body) = do
  liftThrows $ SProc <$> callableSpec
    "<lambda>" env params (Just varParam) body

eval env (VariadicOnlyLambda varParam body) = do
  liftThrows $ SProc <$> callableSpec
    "<lambda>" env [] (Just varParam) body

eval env (MacroDef name params body) =
  do macro <- liftThrows $ SMacro <$> callableSpec
       name env params Nothing body
     liftIO $ defineVar env name macro
     return nil

eval env (VariadicMacroDef name params varParam body) =
  do macro <- liftThrows $ SMacro <$> callableSpec
       name env params (Just varParam) body
     liftIO $ defineVar env name macro
     return nil

eval env (SList [SSymbol "if", predicate, consq, alt]) =
  eval env predicate >>= \case
    SBool False -> eval env alt
    _  -> eval env consq

eval env (SList [SSymbol "set!", SSymbol varName, form]) =
  do val <- eval env form
     setVar env varName val
     return nil

eval env (SList [SSymbol "define", SSymbol varName, form]) =
  do val <- eval env form
     liftIO $ defineVar env varName val
     return nil

eval env (SList (SSymbol "begin" : forms)) =
  last <$> mapM (eval env) forms

-- TODO: this is supposed to take an `env' arg
eval env (SList [SSymbol "eval",  val]) = eval env val >>= eval env

-- TODO: improve `read' so that this can be a regular scheme proc
eval env (SList [SSymbol "load",  val]) = case val of
  SString filename ->
    liftIO (readFile filename) >>=
    liftThrows . readExprs filename >>=
    mapM_ (eval env) >>
    return nil
  badArg -> throwError $ TypeMismatch "string" badArg

eval env (SList (procExpr:args)) =
  eval env procExpr >>= \case
    SMacro transformer -> do
      form <- apply (SProc transformer) args
      eval env form
    proc' -> do
      evaledArgs <- mapM (eval env) args
      apply proc' evaledArgs

eval _ form = throwError $ BadForm "Invalid form:" form
