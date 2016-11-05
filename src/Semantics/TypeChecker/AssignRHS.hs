module Semantics.Typechecker.AssignRHS where

import Control.Monad.Writer.Strict
import Utilities.Definitions

type TypeChecker a = Writer [Error] a

data Error = Error TypeError

data TypeError
  = Mismatch Type Type
  | InvalidArgs [Type] [Type]

typeCheckRHS :: AssignRHS -> TypeChecker Type
typeCheckRHS (ExprAssign e)       = typeCheckExpr e
typeCheckRHS (ArrayLitAssign es)  = typeCheckConcat (map typeCheckExpr es)
typeCheckRHS (NewPairAssign e e') = do
  DataT t  <- typeCheckExpr e
  DataT t' <- typeCheckExpr e'
  return (DataT $ PairT t t')
typeCheckRHS (PairElemAssign (PairElem _ e)) = typeCheckExpr e
typeCheckRHS (FuncCallAssign (Ident _ i) es) = do
  ts          <- mapM typeCheckExpr es
  FuncT t ts' <- return (typeInfo i)
  if (ts == ts')
   then return (FuncT t ts)
   else tell [Error (InvalidArgs ts ts')] >> return UnitType

typeCheckExpr :: Expr -> TypeChecker Type
typeCheckExpr = undefined

typeCheckConcat :: [TypeChecker Type] -> TypeChecker Type
typeCheckConcat = undefined
