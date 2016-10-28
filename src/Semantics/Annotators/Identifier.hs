module Semantics.Annotators.Identifier (
  annotateIdent,
  annotateNewIdent
) where

import qualified Data.Map as Map
import Control.Monad.State.Strict

import Semantics.Annotators.Util
import Utilities.Definitions

annotateNewIdent :: Ident -> LexicalScoper Ident
annotateNewIdent ident@(Ident name info) = do
  st          <- get
  -- Do we want to do this?
  -- it will override the definition of the old variable?
  -- do we actually need NoInfo
  -- im guessing in the parsing stage
  -- we will now encode the information about
  -----------------------------------------------------
  -- It does not override the definition of the old variable, 'put newST' is
  -- not called in the case of the lookup succeeding.
  let newST   = addToST ident st
  if lookUpIdent ident st
    then return (setErrType Duplicate ident)
    else do { put newST; return (setErrType NoError ident) }

annotateIdent :: Ident -> LexicalScoper Ident
annotateIdent ident@(Ident name info) = do
  st <- get
  if lookUpIdent ident st
      -- should already by NoError
    then return $ setErrType NoError ident
    else return $ setErrType NotInScope ident
