{-# LANGUAGE MultiWayIf #-}

module Semantics.TypeChecker.AssignLHS (
  typeCheckLHS
) where

import Control.Monad.Writer.Strict

import Semantics.ErrorMessages
import Semantics.TypeChecker.Expression
import Utilities.Definitions

typeCheckLHS :: AssignLHS -> TypeChecker Type
typeCheckLHS var@(Var ident pos) = do
  identT <- typeCheckIdent ident
  if | identT == PolyFunc -> tell [typeMismatch DataType PolyFunc pos var] >> return NoType
     | otherwise -> return identT
typeCheckLHS (ArrayDeref arrayElem _)
  = typeCheckArrayElem arrayElem
typeCheckLHS (PairDeref pairElem _)
  = typeCheckPairElem pairElem
