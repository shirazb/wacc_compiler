module Semantics.Annotators.Function (
  annotateFunc
) where

import Semantics.ErrorMsgs
import Utilities.Definitions
import Semantics.Annotators.Identifier
import Semantics.Annotators.Statement
--
annotateFunc :: Func -> LexicalScoper Func
annotateFunc f@(Func t ident (ParamList ps) stat) = do
  st@(ST parent env) <- get
  let newEnv = Map.insert (nameAndContext ident) info env
  let newGlobalST = ST parent newEnv
  if lookUpIdent (nameAndContext ident) st
    then (let funcName = setErrType Duplicate ident) -- no such thing as side effects
    else (let funcName = setErrType NoError ident
  -- now lets initiate a symbol table for functions
  -- add all the parameters, then pass this symbol table in to stat
  let funcST = ST st Map.empty
  put funcST
  newParamList <- mapM addParam ps
  stat'        <- annotateStat stat

  return ident


addParam :: Param -> LexicalScoper Param
addParam (Param tp ident) = do
  st@(ST parent env) <- get
  let newEnv       = Map.insert (nameAndContext ident) info env
  let newST        = ST parentST newEnv
  if lookUpIdent (nameAndContext ident) st
    then return (setErrType Duplicate ident)
    else do {put newST; return (setErrType NoError ident)}
