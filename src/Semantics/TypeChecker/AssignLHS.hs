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
  if identT == PolyFunc
    then tell [typeMismatch DataType PolyFunc pos var] >> return NoType
    else return identT
typeCheckLHS (ArrayDeref arrayElem _)
  = typeCheckArrayElem arrayElem
typeCheckLHS (PairDeref pairElem _)
  = typeCheckPairElem pairElem

runTest
  = runWriter . typeCheckLHS
