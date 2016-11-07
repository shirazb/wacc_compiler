{-# LANGUAGE MultiWayIf #-}

module Semantics.TypeChecker.AssignLHS (
  typeCheckLHS
) where

import Debug.Trace

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

-- Common test resources
x = Ident "x" (Info (PairT (BaseT BaseInt) Pair) Variable)
f = Ident "f" (Info (FuncT (BaseT BaseChar) [ArrayT 5 (BaseT BaseString)]) Function)
a = Ident "a" (Info (ArrayT 3 (PairT Pair (ArrayT 2 (BaseT BaseInt)))) Variable)
p = Ident "p" (Info (PairT (ArrayT 2 (BaseT BaseInt)) (BaseT BaseString)) Variable)

-- Var tests
-- varX = Var x
-- funcVarFails = Var f
--
-- -- ArrayDeref tests
-- validArrayDeref = ArrayDeref (ArrayElem a [BinaryApp (Arith Mul) (IntLit 1) (IntLit 2), UnaryApp Neg (IntLit (-3))])
-- derefFuncFails = ArrayDeref (ArrayElem f [IntLit 3])
-- tooManyDerefsFails = ArrayDeref (ArrayElem a [IntLit 2, IntLit 5, IntLit 2, IntLit 4])
-- derefToInnerTypeReturnsInnerType = ArrayDeref (ArrayElem a [IntLit 10, IntLit 2, IntLit 11])
-- propagatesTypeErrInIndex = ArrayDeref (ArrayElem a [UnaryApp Neg (BoolLit True)])
--
-- -- PairDeref tests
-- validPairDeref = PairDeref (PairElem Fst (IdentE p))
-- nullPairDeref  = PairDeref (PairElem Snd PairLiteral)
-- malformedExprInPairDerefPropagates = PairDeref (PairElem Snd (UnaryApp Not (IntLit 3)))
-- nonPairExprFails = PairDeref (PairElem Snd (IdentE a))

runTest
  = runWriter . typeCheckLHS
