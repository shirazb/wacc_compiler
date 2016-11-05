module Semantics.Typechecker.AssignRHS where

import Control.Monad.Writer.Strict
import Utilities.Definitions

type TypeChecker a = Writer [Error] a

data Error = Error TypeError

data TypeError
  = Mismatch Type Type
  | InvalidArgs Type Type

typeCheckRHS :: AssignRHS -> TypeChecker Type
typeCheckRHS (ExprAssign e)       = typeCheckExpr e
typeCheckRHS (ArrayLitAssign es)  = typeCheckConcat (map typeCheckExpr es)
typeCheckRHS (NewPairAssign e e') = do
  t  <- typeCheckExpr e
  t' <- typeCheckExpr e'
  if (checkIfFuncT t || checkIfFuncT t')
    then tell [] -- What type do I return?
    else return (PairT t t')
typeCheckRHS (PairElemAssign (PairElem _ e)) = typeCheckExpr e
typeCheckRHS (FuncCallAssign (Ident _ i) es) = do
  ts          <- mapM typeCheckExpr es
  FuncT t ts' <- return (typeInfo i)
  if (ts == ts')
   then return (FuncT t ts)
   else tell [Error (Mismatch (FuncT t ts') (FuncT t ts))] >> return NoType

checkIfFuncT :: Type -> Bool
checkIfFuncT (FuncT _ _) = True
checkIfFuncT _           = False

typeCheckExpr :: Expr -> TypeChecker Type
ypeCheckExpr = undefined

typeCheckConcat :: [TypeChecker Type] -> TypeChecker Type
typeCheckConcat = undefined
