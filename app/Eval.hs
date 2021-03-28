{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Eval where

import Data.Maybe (isNothing)
import Control.Monad.Except (throwError, liftIO, ExceptT)

import Types
  ( SchemeValOrError
  , IOSchemeValOrError
  , SchemeVal (..)
  , SchemeError (..)
  , Env
  )
import Env (getVar , defineVar , setVar, extendWith)
import Parser (readExprs)


---------------------------------------------------------------------------------
-- misc helpes
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

data MaybeSpliced a = Typical a | Spliced a

instance Functor MaybeSpliced where
  fmap f = \case
    Typical val -> Typical $ f val
    Spliced val -> Spliced $ f val

-- TODO: BUG: this doesn't eval correctly: `(1 ```,,@,,@(list (+ 1 2)) 4)
evalQuasiquoted :: Env -> SchemeVal -> IOSchemeValOrError
evalQuasiquoted env (SList list) = iter 1 [] list >>= \case
  Typical val -> return val
  Spliced val -> throwError $ BadForm "splicing weirdness" val
  where
    iter
      :: Integer      -- quasiquote depth
      -> [SchemeVal]  -- accumulator
      -> [SchemeVal]  -- remaining
      -> ExceptT SchemeError IO (MaybeSpliced SchemeVal)

    iter _ acc [] = return $ Typical $ SList $ reverse acc

    iter depth _ [SSymbol "unquote", val]
      | depth == 0 = throwError $ negativeQqDepth val
      | depth == 1 = Typical <$> eval env val
      | otherwise  = case val of
          SList list' -> fmap Unquote <$> iter (depth - 1) [] list'
          nonList -> return $ Unquote <$> Typical nonList

    iter depth _ [SSymbol "unquote-splicing", val]
      | depth == 0 = throwError $ negativeQqDepth val
      | depth == 1 = Spliced <$> eval env val
      | otherwise  = case val of
          SList list' -> fmap UnquoteSplicing <$> iter (depth - 1) [] list'
          nonList -> return $ UnquoteSplicing <$> Spliced nonList

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
-- proc stuff
---------------------------------------------------------------------------------

data ProcSpec = ProcSpec
  { psEnv      :: Env
  , psName     :: String
  , psParams   :: [SchemeVal]
  , psVarParam :: Maybe SchemeVal
  , psBody     :: [SchemeVal]
  }


makeProc :: ProcSpec -> SchemeValOrError
makeProc ProcSpec {..} =
  do params <- mapM symbolToString psParams
     varParam <- mapM symbolToString psVarParam
     return $ SProc
       { procParams   = params
       , procVarParam = varParam
       , procBody     = psBody
       , procClosure  = psEnv
       } where
        symbolToString = \case
          SSymbol val -> return val
          _ -> throwError $ BadForm
                "Invalid procedure definition"
                (SList (SSymbol psName : psParams))

apply :: SchemeVal -> [SchemeVal] -> IOSchemeValOrError

apply (SPrimativeProc proc') args = liftThrows $ proc' args
apply (SIOProc proc') args = proc' args

apply SProc {..} args = let
  numParams' = length procParams
  numParams  = toInteger numParams'
  numArgs    = toInteger $ length args
  in if (numParams > numArgs) ||
        (numParams < numArgs && isNothing procVarParam)
     then throwError $ NumArgs numParams args
     else let
       remainingArgs = drop numParams' args
       paramsArgsMap = zip procParams args ++ case procVarParam of
         Just varParamName -> [(varParamName, SList remainingArgs)]
         Nothing -> []
       in do
         procEnv <- liftIO $ extendWith paramsArgsMap procClosure
         last <$> mapM (eval procEnv) procBody


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

pattern ProcDef
  :: String -> [SchemeVal] -> [SchemeVal] -> SchemeVal
pattern ProcDef name params body <- SList
  ( SSymbol "define"
    : SList (SSymbol name : params)
    : body
  )

pattern VariadicProcDef
  :: String -> [SchemeVal] -> SchemeVal -> [SchemeVal] -> SchemeVal
pattern VariadicProcDef name params varParam body <- SList
  ( SSymbol "define"
    : SDottedList (SSymbol name : params) varParam
    : body
  )

---------------------------------------------------------------------------------
-- macros
---------------------------------------------------------------------------------
-- TODO: dedup from proc stuff

pattern MacroDef
  :: String -> [SchemeVal] -> [SchemeVal] -> SchemeVal
pattern MacroDef name params body <- SList
  ( SSymbol "define-macro"
    : SList (SSymbol name : params)
    : body
  )

makeMacro :: ProcSpec -> SchemeValOrError
makeMacro ProcSpec {..} =
  do params <- mapM symbolToString psParams
     varParam <- mapM symbolToString psVarParam
     return $ SMacro
       { macroParams   = params
       , macroVarParam = varParam
       , macroBody     = psBody
       , macroClosure  = psEnv
       } where
        symbolToString = \case
          SSymbol val -> return val
          _ -> throwError $ BadForm
                "Invalid macro definition"
                (SList (SSymbol psName : psParams))


applyMacro env SMacro {..} args = let
  numParams' = length macroParams
  numParams  = toInteger numParams'
  numArgs    = toInteger $ length args
  expandedMacro =
    if (numParams > numArgs) ||
        (numParams < numArgs && isNothing macroVarParam)
    then throwError $ NumArgs numParams args
    else let
      remainingArgs = drop numParams' args
      paramsArgsMap = zip macroParams args ++ case macroVarParam of
        Just varParamName -> [(varParamName, SList remainingArgs)]
        Nothing -> []
      in do
        procEnv <- liftIO $ extendWith paramsArgsMap macroClosure
        last <$> mapM (eval procEnv) macroBody
  in expandedMacro >>= eval env

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

eval env (Lambda params body) =
  liftThrows $ makeProc ProcSpec
    { psEnv      = env
    , psName     = "<lambda>"
    , psParams   = params
    , psVarParam = Nothing
    , psBody     = body
    }

eval env (VariadicLambda params varParam body) =
  liftThrows $ makeProc ProcSpec
    { psEnv      = env
    , psName     = "<lambda>"
    , psParams   = params
    , psVarParam = Just varParam
    , psBody     = body
    }

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

eval env (ProcDef name params body) =
  do proc' <- liftThrows $ makeProc ProcSpec
       { psEnv      = env
       , psName     = name
       , psParams   = params
       , psVarParam = Nothing
       , psBody     = body
       }
     liftIO $ defineVar env name proc'
     return nil

eval env (VariadicProcDef name params varParam body) =
  do proc' <- liftThrows $ makeProc ProcSpec
       { psEnv      = env
       , psName     = name
       , psParams   = params
       , psVarParam = Just varParam
       , psBody     = body
       }
     liftIO $ defineVar env name proc'
     return nil

eval env (MacroDef name params body) =
  do macro <- liftThrows $ makeMacro ProcSpec
       { psEnv      = env
       , psName     = name
       , psParams   = params
       , psVarParam = Nothing
       , psBody     = body
       }
     liftIO $ defineVar env name macro
     return nil

eval env (SList [SSymbol "eval",  val]) = eval env val

eval env (SList [SSymbol "load",  val]) = case val of
  SString filename ->
    liftIO (readFile filename) >>=
    liftThrows . readExprs filename >>=
    mapM_ (eval env) >>
    return nil
  badArg -> throwError $ TypeMismatch "string" badArg

eval env (SList (procExpr:args)) =
  eval env procExpr >>= \case
    macro@SMacro {} -> do
      applyMacro env macro args
    proc' -> do
      evaledArgs <- mapM (eval env) args
      apply proc' evaledArgs

eval _ form = throwError $ BadForm "LEARN 2 CODE!!1!" form
