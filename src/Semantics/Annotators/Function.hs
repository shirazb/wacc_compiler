module Semantics.Annotators.Function (
  annotateFunc
) where

import Semantics.ErrorMsgs
import Utilities.Definitions
import Semantics.Annotators.Identifier
import Semantics.Annotators.Statement
--
annotateFunc :: Func -> LexicalScoper Func
annotateFunc f@(Func t ident pl@(ParamList ps) stat) = do
  st@(ST parent env) <- get
  let newEnv = Map.insert (nameAndContext ident) info env
  let newGlobalST = ST parent newEnv
  let funcST = ST st Map.empty
  put funcST
  pl'          <- mapM addParam ps
  stat'        <- annotateStat stat
  put st
  if lookUpIdent ident st
    then return (Func t errIdent pl stat)
    else return (Func t nonErrIdent pl' stat')
  where
    errIdent = setErrType Duplicate ident
    nonErrIdent = setErrType NoError


addParam :: Param -> LexicalScoper Param
addParam (Param tp ident) = do
  st@(ST parent env) <- get
  let newEnv       = Map.insert (nameAndContext ident) info env
  let newST        = ST parentST newEnv
  if lookUpIdent (nameAndContext ident) st
    then return (setErrType Duplicate ident)
    else do {put newST; return (setErrType NoError ident)}
