{-# LANGUAGE MultiWayIf #-}

module Semantics.TypeChecker.AssignRHS where

import Control.Monad.Writer.Strict

import Semantics.TypeChecker.Expression
import ErrorMessages.Semantic
import Utilities.Definitions

typeCheckRHS :: AssignRHS -> TypeChecker Type
typeCheckRHS (ExprAssign e)
  = typeCheckExpr e

typeCheckRHS (ArrayLitAssign es)  = do
  ts <- mapM typeCheckExpr es
  typeCheckConcat ts


typeCheckRHS (NewPairAssign e e') = do
  t  <- typeCheckExpr e
  t' <- typeCheckExpr e'
  case (t, t') of
    (NoType, _) -> return NoType
    (_, NoType) -> return NoType
    _           -> return (PairT t t')

typeCheckRHS (PairElemAssign p)
  = typeCheckPairElem p

typeCheckRHS (FuncCallAssign (Ident _ i) es) = do
  ts <- mapM typeCheckExpr es
  let FuncT t ts' = typeInfo i
  if | length ts /= length ts' -> tell ["Incorrect number of args func"] >> return NoType
     | ts == ts'               -> return t
     | otherwise               -> tell ["Expected: _ Actual: _"] >> return NoType

typeCheckConcat :: [Type] -> TypeChecker Type
typeCheckConcat ts
  | checkNoType ts                   = return NoType
  | and (zipWith (==) ts (tail ts))  = return (head ts)
  | otherwise                        = return NoType

-- This function has to be recursive as you can only check for NoType in case statements
checkNoType :: [Type] -> Bool
checkNoType []             = True
checkNoType (NoType : ts)  = False
checkNoType (_ : ts)       = checkNoType ts

-- -- totally better version :)
-- typeCheckConcat :: [Type] -> TypeChecker Type
-- typeCheckConcat (NoType : ts)
--   = return NoType
-- typeCheckConcat [t]
--   return t
-- typeCheckConcat (t : t' : ts)
--   | t == t'   = typeCheckConcat (t' : ts)
--   | otherwise = return NoType
