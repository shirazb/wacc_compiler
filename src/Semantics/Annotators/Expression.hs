module Semantics.Annotators.Expression (
  annotateExpr,
  annotateExprList
) where

import Control.Monad.State.Strict
import qualified Data.Map as Map

import Semantics.Annotators.Identifier
import Semantics.ErrorMsgs
import Utilities.Definitions

annotateExpr :: Expr -> LexicalScoper Expr
annotateExpr (IdentE ident pos) = do
  newIdent <- annotateIdent Variable ident
  return $ IdentE newIdent pos

annotateExpr (ExprArray (ArrayElem ident exprs pos1) pos2) = do
  newIdent <- annotateIdent Variable ident
  newExprs <- mapM annotateExpr exprs
  return $ ExprArray (ArrayElem newIdent newExprs pos1) pos2

annotateExpr (UnaryApp unOp expr pos) = do
  newExpr <- annotateExpr expr
  return $ UnaryApp unOp newExpr pos

annotateExpr (BinaryApp binOp e1 e2 pos) = do
  e1' <- annotateExpr e1
  e2' <- annotateExpr e2
  return $ BinaryApp binOp e1' e2' pos

-- Must be the last case
annotateExpr literal
  = return literal

annotateExprList :: [Expr] -> LexicalScoper [Expr]
annotateExprList
  = mapM annotateExpr
