{- This module type checks right hand side assignments -}

{-# LANGUAGE MultiWayIf #-}

module Semantics.TypeChecker.AssignRHS where

import Control.Monad.Writer.Strict

{- LOCAL IMPORTS -}
import Semantics.TypeChecker.Expression
import Semantics.ErrorMessages
import Utilities.Definitions

-- POST: Type checks the right hand side of an expression
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

typeCheckRHS expr@(FuncCallAssign (Ident funcName i) es pos) = do
  ts <- mapM typeCheckExpr es
  let FuncT t ts' = typeInfo i
  if | length ts /= length ts' -> tell [typeMismatchList ts' ts pos expr]
                                  >> return NoType
     | ts == ts'               -> return t
     | otherwise               -> tell [typeMismatchList ts' ts pos expr]
                                  >> return NoType

{- HELPER FUNCTIONS -}

-- POST: Checks if every element in a list of types has the same type
typeCheckConcat :: [Type] -> TypeChecker Type
typeCheckConcat ts
  | checkNoType ts                   = return NoType
  | and (zipWith (==) ts (tail ts))  = return (head ts)
  | otherwise                        = return NoType


-- POST: Checks if a list of types contains a NoType. Returns false if it does,
--       otherwise it returns true
-- NOTE: checkNoType must be recursive as you can only check for NoType in
--       case statements (since equality on NoType in 'if' conditions will
--       always evaluate to true)
checkNoType :: [Type] -> Bool
checkNoType []
  = True
checkNoType (NoType : ts)
  = False
checkNoType (_ : ts)
  = checkNoType ts

-- POST: Gets the position of an AssignRHS
getPosRHS :: AssignRHS -> Position
getPosRHS rhs
  = case rhs of
      ExprAssign _ p        -> p
      ArrayLitAssign _ p    -> p
      NewPairAssign _ _ p   -> p
      PairElemAssign _ p    -> p
      FuncCallAssign _ _ p  -> p
