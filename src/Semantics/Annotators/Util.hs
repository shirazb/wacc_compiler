{- This module defines a number of utility functions which are used whilst
annotating the AST -}

module Semantics.Annotators.Util where

import qualified Data.Map as Map
import Control.Monad.State.Strict (get, put)

{- LOCAL IMPORTS -}
import Semantics.ErrorMessages
import Utilities.Definitions

-- POST: Returns true if the given identifier is marked with an error.
identHasError :: Ident -> Bool
identHasError (Ident _ (ScopeError _))
  = True
identHasError _
  = False

-- POST: Sets the info of a given identifier
setInfo :: Info -> Ident -> Ident
setInfo info (Ident name _)
  = Ident name info

-- POST: Adds given identifier to the given map.
addToEnv :: Ident -> Env -> Env
addToEnv ident
  = Map.insert (nameAndContext ident) (identInfo ident)

-- POST: Adds the identifer to the symbol table.
addToST :: Ident -> SymbolTable -> SymbolTable
addToST ident@(Ident _ Info{}) (ST parent env)
  = ST parent (addToEnv ident env)
addToST _ _
  = error $ assertionFailed "Cannot add non annotated or error" ++
      " identifer to the symbol table."

-- POST: Retrieves the info of the given input identifier
identInfo :: Ident -> Info
identInfo (Ident _ info)
  = info

-- POST: Returns the name and context of an annotated identifier
nameAndContext :: Ident -> (String, Context)
nameAndContext (Ident name (Info _ context))
  = (name, context)
nameAndContext ident
  = error $ assertNoNameAndContext ident

-- POST: Sets the error type of the identifer
setErrType :: ScopeErrorType -> Ident -> Ident
setErrType errType (Ident name _)
  = Ident name (ScopeError errType)

-- POST: Returns information about a given identifier if it is in the
--       the current scope of the symbol table
lookUpIdentCurrScope :: (String, Context) -> SymbolTable -> Maybe Info
lookUpIdentCurrScope nameAndCtxt (ST _ env)
  = Map.lookup nameAndCtxt env

-- POST: Returns information about a given identifier if it is in
--       any scope of the symbol table
lookUpIdent :: (String, Context) -> SymbolTable -> Maybe Info
lookUpIdent _ None
  = error "write a proper error msg -- lookUpIdent"
lookUpIdent nameAndCtxt st@(ST None env)
  = Map.lookup nameAndCtxt env
lookUpIdent nameAndCtxt (ST parentST env)
  = case Map.lookup nameAndCtxt env of
    Nothing   -> lookUpIdent nameAndCtxt parentST
    Just info -> Just info

-- POST: Peforms a given annotator inside of a
--       child scope. Exits to the parent scope
inChildScope :: ScopeAnalysis a -> ScopeAnalysis a
inChildScope child = do
  parentST <- get
  put (ST parentST Map.empty)
  annotatedAST <- child
  put parentST
  return annotatedAST

-- POST: Same as above but applys a function to the result of annotation
inChildScopeAndWrap :: (a -> b) -> ScopeAnalysis a -> ScopeAnalysis b
inChildScopeAndWrap f child
  = f <$> inChildScope child
