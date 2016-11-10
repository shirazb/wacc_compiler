{-
  Annotates identifiers with type information, or marks them as scope errors.
-}

module Semantics.Annotators.Identifier (
  annotateIdent,
  annotateNewIdent
) where

import qualified Data.Map as Map
import Control.Monad.State.Strict

import Semantics.Annotators.Util
import Semantics.ErrorMsgs
import Utilities.Definitions

-- PRE: info is NoInfo
-- POST: If the identifier is not a duplicate, adds it to the current ST.
--       Otherwise, marks it as a duplicate.
annotateNewIdent :: Ident -> Info -> LexicalScoper Ident
annotateNewIdent (Ident name NoInfo) info = do
  st            <- get
  let newIdent  = Ident name info
  let newST     = addToST newIdent st
  case lookUpIdentCurrScope (name, context info) st of
    Just _  -> return (setErrType Duplicate newIdent)
    Nothing -> do { put newST; return newIdent }
annotateNewIdent (Ident name info) info'
  = error $ assertReannotatingNewIdent name info info'

-- PRE:  Info is NoInfo
-- POST: If the identifier is in scope, retrieves its type information.
--       Otherwise, marks it as not in scope.
annotateIdent :: Context -> Ident -> LexicalScoper Ident
annotateIdent ctext ident@(Ident name NoInfo) = do
  st <- get
  return $ case lookUpIdent (name, ctext) st of
    Nothing   -> setErrType NotInScope ident
    Just info -> setInfo info ident
annotateIdent ctext (Ident name info)
  = error $ assertReannotatingIdent ctext name info
