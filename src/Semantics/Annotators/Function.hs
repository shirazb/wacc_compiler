module Semantics.Annotators.Function (
  annotateFunc
) where

import Semantics.ErrorMsgs
import Utilities.Definitions
import Semantics.Annotators.Identifier
import Semantics.Annotators.Statement

annotateFunc :: Func -> LexicalScoper Func
annotateFunc f@(Func t ident paramList body) = do
  globalST@(ST None globalEnv) <- get
  newIdent          <- annotateNewIdent ident

  -- needs a temporary Env with newIdent, we will only update actual
  -- global ST if newIdent not a duplicate
  let newGlobalEnv  = Map.insert (nameAndContext newIdent) info globalEnv
  let newGlobalST   = ST None newGlobalEnv

  -- initialise function ST
  let funcST        = ST newGlobalST Map.empty
  put funcST
  newParamList <- annotateParamList

  -- annotate the function body
  newBody      <- annotateStat

  -- restore global symbol table; discard function's ST
  if identHasError newIdent
    then put globalST
    else put newGlobalST

  -- return annotated function
  return $ Func t newIdent newParamList newBody

identHasError :: Ident -> Bool
identHasError (Ident _ (Info _ _ _ NoError))
  = False
identHasError _
  = True

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

annotateParamList :: ParamList -> LexicalScope ParamList
annotateParamList (ParamList ps)
  = mapM annotateParam ps

annotateParam :: Param -> LexicalScoper Param
annotateParam (Param tp ident)
  = annotateNewIdent ident
  --
  -- st@(ST parent env) <- get
  -- let newEnv       = Map.insert (nameAndContext ident) info env
  -- let newST        = ST parentST newEnv
  -- if lookUpIdent (nameAndContext ident) st
  --   then return (setErrType Duplicate ident)
  --   else do {put newST; return (setErrType NoError ident)}
