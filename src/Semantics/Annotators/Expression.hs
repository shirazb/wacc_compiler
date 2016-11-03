module Semantics.Annotators.Expression (
  annotateExpr,
  annotateExprList
) where

import Control.Monad.State.Strict
import qualified Data.Map as Map

import Semantics.Annotators.Identifier
import Semantics.ErrorMsgs
import Utilities.Def2

annotateExpr :: Expr -> LexicalScoper Expr
annotateExpr (IdentE ident)
  = IdentE <$> annotateIdent Variable ident

annotateExpr (ExprArray (ArrayElem ident exprs)) = do
  newIdent <- annotateIdent Variable ident
  newExprs <- mapM annotateExpr exprs
  return $ ExprArray (ArrayElem newIdent newExprs)

annotateExpr (UnaryApp unOp expr)
  = UnaryApp unOp <$> annotateExpr expr

annotateExpr (BinaryApp binOp e e')
  = liftM2 (BinaryApp binOp) (annotateExpr e) (annotateExpr e')

-- Must be the last case
annotateExpr literal
  = return literal

annotateExprList :: [Expr] -> LexicalScoper [Expr]
annotateExprList
  = mapM annotateExpr
