module Semantics.Annotators.Expression (
  annotateExpr
) where

import Control.Monad.State.Strict
import qualified Data.Map as Map

import Semantics.Annotators.Identifier
import Semantics.ErrorMsgs
import Utilities.Definitions

annotateExpr :: Expr -> LexicalScoper Expr
annotateExpr (IdentE ident)
  = IdentE <$> annotateIdent ident

annotateExpr (ExprArray (ArrayElem ident exprs)) = do
  newIdent <- annotateIdent ident
  newExprs <- mapM annotateExpr exprs
  return $ ExprArray (ArrayElem newIdent newExprs)

annotateExpr (UnaryApp unOp expr) = do
  newExpr <- annotateExpr expr
  return $ UnaryApp unOp newExpr

annotateExpr (BinaryApp binOp e e') = do
  newE  <- annotateExpr e
  newE' <- annotateExpr e'
  return $ BinaryApp binOp newE newE'

annotateExpr literal
  = return literal
