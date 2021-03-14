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


liftThrows :: Either SchemeError a -> ExceptT SchemeError IO a
liftThrows = \case
  Left err -> throwError err
  Right val -> return val


evalQuasiquoted :: Env -> SchemeVal -> IOSchemeValOrError
evalQuasiquoted env (SList list') = let
  unquote val = SList [SSymbol "unquote", val]
  quasiquote val = SList [SSymbol "quasiquote", val]

  iter
    :: Integer      -- quasiquote level
    -> [SchemeVal]  -- accumulator
    -> [SchemeVal]  -- remaining
    -> IOSchemeValOrError

  iter _ acc [] = return $ SList $ reverse acc

  iter qqDepth _ [SSymbol "unquote", val] =
    if qqDepth == 1
    then eval env val
    else case val of
      SList list -> unquote <$> iter (qqDepth - 1) [] list
      nonList -> return $ unquote nonList

  -- TODO: unquote-splicing

  iter qqDepth _ [SSymbol "quasiquote", val] =
    case val of
      SList list -> quasiquote <$> iter (qqDepth + 1) [] list
      nonList -> return $ quasiquote nonList

  iter qqDepth acc (SList list:xs) = do
    scannedList <- iter qqDepth [] list
    iter qqDepth (scannedList:acc) xs

  iter qqDepth acc (x:xs) = iter qqDepth (x:acc) xs

  in iter 1 [] list'

evalQuasiquoted _ val = return val


data ProcSpec = ProcSpec
  { psEnv      :: Env
  , psName     :: String
  , psParams   :: [SchemeVal]
  , psVarParam :: Maybe SchemeVal
  , psBody     :: [SchemeVal]
  }

makeProc :: ProcSpec -> SchemeValOrError
makeProc ProcSpec{..} =
  do params <- mapM symbolToString psParams
     varParam <- mapM symbolToString psVarParam
     return $ SProc
       { procParams   = params
       , procVarParam = varParam
       , procBody     = psBody
       , procClosure  = psEnv
       }
  where
    symbolToString :: SchemeVal -> Either SchemeError String
    symbolToString = \case
      SSymbol val -> return val
      _ -> throwError $ BadForm "Invalid procedure definition"
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
         Nothing           -> []
       in do
         procEnv <- liftIO $ extendWith paramsArgsMap procClosure
         last <$> mapM (eval procEnv) procBody

apply nonProc _ = throwError $ TypeMismatch "procedure" nonProc


pattern Lambda :: [SchemeVal] -> [SchemeVal] -> SchemeVal
pattern Lambda params body <- SList
  ( SSymbol "lambda" : SList params : body)

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

nil :: SchemeVal
nil = SList []

eval :: Env -> SchemeVal -> IOSchemeValOrError
eval _   val@(SDottedList _ _) = throwError $ BadForm "Can't eval dotted list" val
eval _   val@(SVector _)       = throwError $ BadForm "Can't eval vector" val

eval _   val@(SBool _)         = return val
eval _   val@(SChar _)         = return val
eval _   val@(SString _)       = return val
eval _   val@(SchemeNumber _)       = return val

eval env (SSymbol varName)     = getVar env varName

eval _   (SList [SSymbol "quote", val]) = return val

eval env (SList [SSymbol "quasiquote", val]) = evalQuasiquoted env val

eval env (SList [SSymbol "eval",  val]) = eval env val

eval env (SList [SSymbol "load",  val]) = case val of
  SString filename ->
    liftIO (readFile filename) >>=
    liftThrows . readExprs filename >>=
    mapM_ (eval env) >>
    return nil
  badArg -> throwError $ TypeMismatch "string" badArg

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

eval env (SList (procExpr:args)) =
  do proc' <- eval env procExpr
     evaledArgs <- mapM (eval env) args
     apply proc' evaledArgs

eval _ form = throwError $ BadForm "Unrecognized form" form
