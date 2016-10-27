module Semantics.Annotators.Identifier (
  annotateIdent
) where

import qualified Data.Map as Map
import Control.Monad.State.Strict

import Utilities.Definitions

-- Put in one of the utilities files?
nameAndContext :: Ident -> (String, Context)
nameAndContext (Ident name (Info _ context _ _))
  = (name, context)

annotateIdent :: Ident -> LexicalScoper Ident
annotateIdent ident@(Ident name info) = do
  st@(ST parentST env)  <- get
  let newEnv            = Map.insert (nameAndContext ident) info env
  let newST             = ST parentST newEnv
  if lookUpIdent ident st
    then do { put newST; return ident }
    else return $ addErrType Duplicate ident
-- annotateIdent ident@(Ident name info) st@(ST parentST env)
--   = newIdentAndST
--   where
--     newEnv        = Map.insert (nameAndContext ident) info env
--     newST         = ST parentST newEnv
--     newIdentAndST = if lookUpIdent ident st
--                       then (ident, newST)
--                       else (addErrType Duplicate ident, st)

addErrType :: ErrorType -> Ident -> Ident
addErrType errType (Ident name (Info t context expr _))
  = Ident name (Info t context expr errType)

lookUpIdent :: Ident -> SymbolTable -> Bool
lookUpIdent (Ident name _) st@(ST None _)
  = False
lookUpIdent ident st@(ST parentST env)
  = case Map.lookup (nameAndContext ident) env of
      Nothing -> lookUpIdent ident parentST
      Just _  -> True
