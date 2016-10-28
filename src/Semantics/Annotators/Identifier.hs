module Semantics.Annotators.Identifier (
  annotateIdent,
  annotateNewIdent
) where

import qualified Data.Map as Map
import Control.Monad.State.Strict

import Utilities.Definitions

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

annotateNewIdent :: Ident -> LexicalScoper Ident
annotateNewIdent ident@(Ident name info) = do
  st@(ST parentST env)  <- get
  -- Do we want to do this?
  -- it will override the definition of the old variable?
  -- do we actually need NoInfo
  -- im guessing in the parsing stage
  -- we will now encode the information about
  -----------------------------------------------------
  -- It does not override the definition of the old variable, 'put newST' is
  -- not called in the case of the lookup succeeding.
  let newEnv            = Map.insert (nameAndContext ident) info env
  let newST             = ST parentST newEnv
  if lookUpIdent ident st
    then return (setErrType Duplicate ident)
    else do { put newST; return setErrType NoError ident }

annotateIdent :: Ident -> LexicalScoper Ident
annotateIdent ident@(Ident name info) = do
  st <- get
  if lookUpIdent ident st
      -- should already by NoError
    then return $ setErrType NoError ident
    else return $ setErrType NotInScope ident
