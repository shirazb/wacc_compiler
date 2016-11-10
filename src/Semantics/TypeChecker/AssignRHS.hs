{-# LANGUAGE MultiWayIf #-}

module Semantics.TypeChecker.AssignRHS where

import Control.Monad.Writer.Strict

import Semantics.TypeChecker.Expression
import Semantics.ErrorMessages
import Utilities.Definitions

typeCheckRHS :: AssignRHS -> TypeChecker Type
typeCheckRHS (ExprAssign e _)
  = typeCheckExpr e

typeCheckRHS (ArrayLitAssign es _)  = do
  ts <- mapM typeCheckExpr es
  typeCheckConcat ts

typeCheckRHS (NewPairAssign e e' _) = do
  t  <- typeCheckExpr e
  t' <- typeCheckExpr e'
  case (t, t') of
    (NoType, _) -> return NoType
    (_, NoType) -> return NoType
    _           -> return (PairT t t')

typeCheckRHS (PairElemAssign p _)
  = typeCheckPairElem p

typeCheckRHS (FuncCallAssign (Ident funcName i) es pos) = do
  ts <- mapM typeCheckExpr es
  let FuncT t ts' = typeInfo i
  if | length ts /= length ts' -> tell [typeMismatchList ts' ts pos funcName]
                                    >> return NoType
     | ts == ts'               -> return t
     | otherwise               -> tell [typeMismatchList ts' ts pos funcName]
                                    >> return NoType

typeCheckConcat :: [Type] -> TypeChecker Type
typeCheckConcat ts
  | checkNoType ts                   = return NoType
  | and (zipWith (==) ts (tail ts))  = return (head ts)
  | otherwise                        = return NoType

-- This function has to be recursive as you can only check for NoType in case
-- statements
checkNoType :: [Type] -> Bool
checkNoType []            = True
checkNoType (NoType : ts) = False
checkNoType (_ : ts)      = checkNoType ts
