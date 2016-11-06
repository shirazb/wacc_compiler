{-# LANGUAGE MultiWayIf #-}

module Semantics.Typechecker.AssignRHS where

import Control.Monad.Writer.Strict

import Utilities.Definitions
--import ErrorMessages.Semantic

type ErrorMsg      = String
type TypeChecker a = Writer [ErrorMsg] a

typeCheckRHS :: AssignRHS -> TypeChecker Type
typeCheckRHS (ExprAssign e)
  = typeCheckExpr e

typeCheckRHS (ArrayLitAssign es)  = do
  ts <- (mapM typeCheckExpr es)
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

typeCheckExpr :: Expr -> TypeChecker Type
typeCheckExpr = undefined

typeCheckPairElem :: PairElem -> TypeChecker Type
typeCheckPairElem = undefined

typeCheckConcat :: [Type] -> TypeChecker Type
typeCheckConcat ts = do
  if | checkNoType ts                    -> return NoType
     | (and $ zipWith (==) ts (tail ts)) -> return (head ts)
     | otherwise                         -> return NoType

-- This function has to be recursive as you can only check for NoType in case statements
checkNoType :: [Type] -> Bool
checkNoType []          = True
checkNoType (NoType:ts) = False
checkNoType (_ : ts)    = checkNoType ts

  
