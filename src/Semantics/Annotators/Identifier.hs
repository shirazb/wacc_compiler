module Semantics.Annotators.Identifier (
  annotateIdent,
  annotateNewIdent
) where

import qualified Data.Map as Map
import Control.Monad.State.Strict
import Debug.Trace

import Semantics.Annotators.Util
import Utilities.Def2
import Semantics.ErrorMsgs

-- PRE: info contains NoError
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

annotateIdent :: Context -> Ident -> LexicalScoper Ident
annotateIdent ctext ident@(Ident name NoInfo) = do
  st <- get
  return $ case lookUpIdent (name, ctext) st of
    Nothing   -> setErrType NotInScope ident
    Just info -> setInfo info ident
annotateIdent _ _
  = error "make a proper error message -- annotateIdent"
