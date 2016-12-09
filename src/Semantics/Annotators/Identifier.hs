{- This module annotates identifiers with type information, or marks them as
   scope errors -}

module Semantics.Annotators.Identifier (
  annotateIdent,
  annotateNewIdent
) where

import qualified Data.Map as Map
import Control.Monad.State.Strict

{- LOCAL IMPORTS -}
import Semantics.Annotators.Type (scopeCheckType)
import Semantics.Annotators.Util
import Semantics.ErrorMessages
import Utilities.Definitions
import Debug.Trace

-- PRE:  info parameter is NoInfo
-- POST: If the identifier is not a duplicate, adds it to the current ST.
--       Otherwise, marks it as a duplicate
annotateNewIdent :: Ident -> Info -> ScopeAnalysis Ident
annotateNewIdent i@(Ident name NoInfo) info = do
  st            <- get
  let newIdent  = Ident name info
  let newST     = addToST newIdent st
  case lookUpIdentCurrScope (name, context info) st of
    Just _  -> return (setErrType Duplicate newIdent)
    Nothing -> put newST >> return newIdent

annotateNewIdent (Self info) info'
  = error $ assertReannotatingNewIdent self info info'

annotateNewIdent (Ident name info) info'
  = error $ assertReannotatingNewIdent name info info'

-- PRE:  Info is NoInfo
-- POST: If the identifier is in scope, retrieves its type information.
--       Otherwise, marks it as not in scope.
--       For "self" identifiers, marks it as an error if not in a class, or
--       sets its type according to the class name.
annotateIdent :: Context -> Ident -> ScopeAnalysis Ident
annotateIdent ctext ident@(Ident name NoInfo) = do
  st <- get
  return $ case lookUpIdent (name, ctext) st of
    Nothing   -> setErrType NotInScope ident
    Just info -> setInfo info ident

annotateIdent Variable (Self NoInfo) = do
  className <- lookUpCurrClassName
  case className of
    Nothing   -> return $ Self (ScopeError SelfNotInClass)
    Just name -> do
        let classType = ClassT name
        return $ Self (Info Instance classType Variable)

annotateIdent ctext (Ident name info)
  = error $ assertReannotatingIdent ctext name info

annotateIdent ctext (Self info)
  = error $ assertReannotatingIdent ctext self info

-- POST: Returns the current class name or Nothing
lookUpCurrClassName :: ScopeAnalysis (Maybe String)
lookUpCurrClassName = do
  st <- get
  let foundInfo = lookUpIdent (self, Variable) st
  case foundInfo of
    Just (Info Instance (ClassT name) Variable) -> return $ Just name
    Nothing                     -> return Nothing
    Just _                      -> error  $
        "lookUpCurrClassName shows invalid self entry in env. Info returned: "
        ++ show foundInfo
