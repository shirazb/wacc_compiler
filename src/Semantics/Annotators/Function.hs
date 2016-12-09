{- This module annotates function identifiers and variables within the body of
functions -}

module Semantics.Annotators.Function (
  annotateFunc,
  addFuncDeclToST,
  annotateParamList
) where

import Control.Monad.State.Strict (get, put)
import qualified Data.Map as Map

{- LOCAL IMPORTS -}
import Semantics.Annotators.Identifier
import Semantics.Annotators.Statement
import Semantics.Annotators.Util
import Semantics.Annotators.Type
import Semantics.ErrorMessages
import Utilities.Definitions

-- POST: Adds function name to the global symbol table
addFuncDeclToST :: Func -> ScopeAnalysis Func
addFuncDeclToST (Func t ident paramList body pos) = do
  t'     <- scopeCheckType t
  ident' <- annotateNewIdent ident (Info Static t' Function)
  return $ Func t' ident' paramList body pos

-- PRE:  Function name ident already annotated
-- POST: Annotates all the identifiers within the function body.
annotateFunc :: Func -> ScopeAnalysis Func
annotateFunc (Func t ident paramList body pos) = do
  globalST     <- get

  -- Enter new function scope
  put (ST globalST Map.empty)

  -- Annotate the function body
  newParamList <- annotateParamList paramList
  newBody      <- annotateStat body

  -- Exit function scope
  put globalST
  return $ Func t ident newParamList newBody pos

-- POST: Annotates a list of parameters
annotateParamList :: ParamList -> ScopeAnalysis ParamList
annotateParamList (ParamList ps pos)
  = ParamList <$> mapM annotateParam ps <*> return pos

-- POST: Annotates the input parameter
annotateParam :: Param -> ScopeAnalysis Param
annotateParam (Param t ident pos) = do
  t'     <- scopeCheckType t
  ident' <- annotateNewIdent ident (Info Static t' Variable)
  return $ Param t' ident' pos
