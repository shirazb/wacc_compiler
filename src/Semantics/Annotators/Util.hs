module Semantics.Annotators.Util where

import qualified Data.Map as Map
import Control.Monad.State.Strict

import Utilities.Definitions
import Semantics.ErrorMsgs

-- Checks if an identifier has an error in its info
identHasError :: Ident -> Bool
identHasError (Ident _ (ScopeError _))
  = True
identHasError _
  = False

setInfo :: Info -> Ident -> Ident
setInfo info (Ident name _)
  = Ident name info

addToEnv :: Ident -> Env -> Env
addToEnv ident env
  = Map.insert (nameAndContext ident) (identInfo ident) env

addToST :: Ident -> SymbolTable -> SymbolTable
addToST ident@(Ident _ Info{}) (ST parent env)
  = ST parent (addToEnv ident env)
addToST _ _
  = error "make a proper error message -- addToST"

identInfo :: Ident -> Info
identInfo (Ident _ info)
  = info

nameAndContext :: Ident -> (String, Context)
nameAndContext (Ident name (Info _ context))
  = (name, context)
nameAndContext ident
  = error $ assertNoNameAndContext ident

setErrType :: ScopeErrorType -> Ident -> Ident
setErrType errType (Ident name _)
  = Ident name (ScopeError errType)

lookUpIdentCurrScope :: (String, Context) -> SymbolTable -> Maybe Info
lookUpIdentCurrScope nameAndCtxt (ST _ env)
  = Map.lookup nameAndCtxt env

lookUpIdent :: (String, Context) -> SymbolTable -> Maybe Info
lookUpIdent _ None
  = error "write a proper error msg -- lookUpIdent"
lookUpIdent nameAndCtxt st@(ST None env)
  = Map.lookup nameAndCtxt env
lookUpIdent nameAndCtxt (ST parentST env)
  = case Map.lookup nameAndCtxt env of
    Nothing   -> lookUpIdent nameAndCtxt parentST
    Just info -> Just info

inChildScope :: LexicalScoper a -> LexicalScoper a
inChildScope child = do
  parentST <- get
  put (ST parentST Map.empty)
  annotatedAST <- child
  put parentST
  return annotatedAST

inChildScopeAndWrap :: (a -> b) -> LexicalScoper a -> LexicalScoper b
inChildScopeAndWrap f child
  = f <$> inChildScope child
