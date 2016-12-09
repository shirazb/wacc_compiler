{- This module type checks left hand side assignments -}

{-# LANGUAGE MultiWayIf #-}

module Semantics.TypeChecker.AssignLHS (typeCheckLHS) where

import Control.Monad.Writer.Strict (tell)

{- LOCAL IMPORTS -}
import Semantics.ErrorMessages
import Semantics.TypeChecker.Expression
import Utilities.Definitions

-- POST: Type checks the left hand side of a statement
typeCheckLHS :: AssignLHS -> TypeChecker Type

typeCheckLHS var@(Var ident pos) = do
  identT <- typeCheckIdent ident
  if | identT == PolyFunc -> tell [typeMismatch DataType PolyFunc pos var]
       >> return NoType
     | otherwise -> return identT

typeCheckLHS (ArrayDeref arrayElem _)
  = typeCheckArrayElem arrayElem

typeCheckLHS (PairDeref pairElem _)
  = typeCheckPairElem pairElem

typeCheckLHS (MemberDeref ma _)
  = typeCheckMemberAccess ma
