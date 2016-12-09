module Semantics.Annotators.Type (
  scopeCheckType
) where

import Control.Monad.State (get)

import Semantics.Annotators.Util
import Utilities.Definitions

-- POST: Returns NotInScope type or the found type
scopeCheckType :: Type -> ScopeAnalysis Type
scopeCheckType ct@(ClassT name) = do
  st <- get
  case lookUpIdent (name, ClassName) st of
    Just _  -> return ct
    Nothing -> return (NotInScopeT name)
scopeCheckType t
  = return t
