module Semantics.Annotators.Util where

import qualified Data.Map as Map
import Control.Monad.State.Strict

import Utilities.Definitions

-- Checks if an identifier has an error in its info
identHasError :: Ident -> Bool
identHasError (Ident _ (Info _ _ _ NoError))
  = False
identHasError _
  = True

addToEnv :: Ident -> Env -> Env
addToEnv ident env
  = Map.insert (nameAndContext ident) (identInfo ident) env

addToST :: Ident -> SymbolTable -> SymbolTable
addToST ident (ST parent env)
  = ST parent (addToEnv ident env)

identInfo :: Ident -> Info
identInfo (Ident _ info)
  = info

-- Put in one of the utilities files?
nameAndContext :: Ident -> (String, Context)
nameAndContext (Ident name (Info _ context _ _))
  = (name, context)

setErrType :: ErrorType -> Ident -> Ident
setErrType errType (Ident name (Info t context expr _))
  = Ident name (Info t context expr errType)

-- TODO: less duplication
lookUpIdent :: Ident -> SymbolTable -> Bool
lookUpIdent ident st@(ST None env)
  = case Map.lookup (nameAndContext ident) env of
    Nothing -> False
    Just _  -> True
lookUpIdent ident st@(ST parentST env)
  = case Map.lookup (nameAndContext ident) env of
    Nothing -> lookUpIdent ident parentST
    Just _  -> True

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
