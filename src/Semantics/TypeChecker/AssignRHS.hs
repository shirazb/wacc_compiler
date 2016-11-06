module Semantics.TypeChecker.AssignRHS where

import Control.Monad.Writer.Strict

import Utilities.Definitions
import Semantics.TypeChecker.Expression
import ErrorMessages.Semantic

typeCheckRHS :: AssignRHS -> TypeChecker Type
typeCheckRHS (ExprAssign e)       = typeCheckExpr e
typeCheckRHS (ArrayLitAssign es)  = typeCheckConcat (map typeCheckExpr es)
typeCheckRHS (NewPairAssign e e') = do
  t  <- typeCheckExpr e
  t' <- typeCheckExpr e'
  return (PairT t t')
typeCheckRHS (PairElemAssign (PairElem _ e)) = typeCheckExpr e
typeCheckRHS (FuncCallAssign (Ident _ i) es) = do
  ts          <- mapM typeCheckExpr es
  FuncT t ts' <- return (typeInfo i)
  if ts == ts'
   then return (FuncT t ts)
   else tell ["Error (Mismatch (FuncT t ts') (FuncT t ts))"] >> return NoType

typeCheckConcat :: [TypeChecker Type] -> TypeChecker Type
typeCheckConcat = undefined
