module Semantics.Annotators.Function (
  annotateFunc
) where

import Control.Monad.State.Strict
import qualified Data.Map as Map

import Semantics.ErrorMsgs
import Utilities.Definitions
import Semantics.Annotators.Identifier
import Semantics.Annotators.Statement
import Semantics.Annotators.Util

-- TODO: Refactor to make look nicer, try use Util.inChildScope(AndWrap)
annotateFunc :: Func -> LexicalScoper Func
annotateFunc (Func t ident paramList body) = do
  globalST          <- get
  newIdent          <- annotateNewIdent ident

  -- needs a temporary Env with newIdent, we will only update actual
  -- global ST if newIdent not a duplicate
  -- NB: If function name is a duplicate, we are having to assume that
  --        all uses of the name in the body are recursive calls.
  let newGlobalST   = addToST newIdent globalST

  -- initialise function ST
  let funcST        = ST newGlobalST Map.empty
  put funcST
  newParamList      <- annotateParamList paramList

  -- annotate the function body
  newBody           <- annotateStat body

  -- restore global symbol table; discard function's ST
  if identHasError newIdent
    then put globalST
    else put newGlobalST

  -- return annotated function
  return $ Func t newIdent newParamList newBody

  -- Zubair's code
  -- st@(ST parent env) <- get
  -- let newEnv = Map.insert (nameAndContext ident) info env
  -- let newGlobalST = ST parent newEnv
  -- let funcST = ST st Map.empty
  -- put funcST
  -- pl'          <- mapM addParam ps
  -- stat'        <- annotateStat stat
  -- put st
  -- if lookUpIdent ident st
  --   then return (Func t errIdent pl stat)
  --   else return (Func t nonErrIdent pl' stat')
  -- where
  --   errIdent = setErrType Duplicate ident
  --   nonErrIdent = setErrType NoError ident

annotateParamList :: ParamList -> LexicalScoper ParamList
annotateParamList (ParamList ps)
  = ParamList <$> mapM annotateParam ps

annotateParam :: Param -> LexicalScoper Param
annotateParam (Param tp ident)
  = Param tp <$> annotateNewIdent ident
  --
  -- st@(ST parent env) <- get
  -- let newEnv       = Map.insert (nameAndContext ident) info env
  -- let newST        = ST parentST newEnv
  -- if lookUpIdent (nameAndContext ident) st
  --   then return (setErrType Duplicate ident)
  --   else do {put newST; return (setErrType NoError ident)}
