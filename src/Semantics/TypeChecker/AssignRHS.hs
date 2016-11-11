{-# LANGUAGE MultiWayIf #-}

{-
  Type checks the different kinds of assignRHS.
-}
module Semantics.TypeChecker.AssignRHS where

import Control.Monad.Writer.Strict

import Semantics.TypeChecker.Expression
import Semantics.ErrorMessages
import Utilities.Definitions

typeCheckRHS :: AssignRHS -> TypeChecker Type

-- Type checks the expression
typeCheckRHS (ExprAssign e _)
  = typeCheckExpr e

-- Type checks each element, and checks they are the same type.
typeCheckRHS (ArrayLitAssign es _)  = do
  ts <- mapM typeCheckExpr es
  typeCheckConcat ts

-- Type checks each element, returning their type
typeCheckRHS (NewPairAssign e e' _) = do
  t  <- typeCheckExpr e
  t' <- typeCheckExpr e'
  case (t, t') of
    (NoType, _) -> return NoType
    (_, NoType) -> return NoType
    _           -> return (PairT t t')

typeCheckRHS (PairElemAssign p _)
  = typeCheckPairElem p

-- Type checks function calls: checks the ident is a function and that the
-- parameter list is of the correct type.
typeCheckRHS expr@(FuncCallAssign (Ident funcName i) es pos) = do
  ts <- mapM typeCheckExpr es
  let FuncT t ts' = typeInfo i
  if | length ts /= length ts' -> tell [typeMismatchList ts' ts pos expr]
                                    >> return NoType
     | ts == ts'               -> return t
     | otherwise               -> tell [typeMismatchList ts' ts pos expr]
                                  >> return NoType

-- Given a list of types, checks each are the same.
typeCheckConcat :: [Type] -> TypeChecker Type
typeCheckConcat ts
  | checkNoType ts                   = return NoType
  | and (zipWith (==) ts (tail ts))  = return (head ts)
  | otherwise                        = return NoType

-- This function has to be recursive as you can only check for NoType in case
-- statements
checkNoType :: [Type] -> Bool
checkNoType []
  = True
checkNoType (NoType : ts)
  = False
checkNoType (_ : ts)
  = checkNoType ts
