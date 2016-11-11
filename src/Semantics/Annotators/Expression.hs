{- This module annotates all the identifers found within expressions -}

module Semantics.Annotators.Expression (
  annotateExpr,
  annotateExprList
) where

import qualified Data.Map as Map

{- LOCAL IMPORTS -}
import Semantics.Annotators.Identifier
import Semantics.ErrorMessages
import Utilities.Definitions

-- POST: Annotates expressions
annotateExpr :: Expr -> ScopeAnalysis Expr

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

annotateExpr literal
  = return literal

-- POST: Annotates a list of expressions
annotateExprList :: [Expr] -> ScopeAnalysis [Expr]
annotateExprList
  = mapM annotateExpr
