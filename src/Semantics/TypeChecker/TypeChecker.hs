module Semantics.TypeChecker.TypeChecker where

import Control.Monad.Writer.Strict
import Utilities.Definitions

type TypeErrMsg = String

typeCheckStat :: Stat -> Writer [TypeErrMsg] ()
typeCheckStat (Declaration t ident rhs) = do
  typeRHS <- typeCheckRHS rhs
  when (typeRHS /= t) (tell ["Type mismatch in declaration"])
  return ()

typeCheckRHS :: AssignRHS -> Writer [TypeErrMsg] Type
typeCheckRHS = undefined

typeCheckExpr :: Expr -> Writer [TypeErrMsg] Type
typeCheckExpr (IntLit _)
  = return (BaseT BaseInt)
typeCheckExpr (ExprArray arrayElem@(ArrayElem ident es)) = do
  arrayDerefType      <- typeCheckArrayDeref es
  let dimension       = countDimension (identGetType ident)
  let numArrayDerefs  = countDimension arrayDerefType
  if numArrayDerefs > dimension then do {
    tell ["Cannot dereference; not an array"];
    return TypeErr;
  } else
    return arrayDerefType

identGetType :: Ident -> Type
identGetType (Ident _ (Info t _))
  = t

-- checks type of each expression is an int, by delegating to helper that writes error msgs
-- check type of ident derefence
typeCheckArrayDeref :: [Expr] -> Writer [TypeErrMsg] Type
typeCheckArrayDeref (e : es) = do
  eType <- typeCheckExpr e
  when (eType /= BaseT BaseInt)
      (tell ["type of array index must be int, actually: " ++ show eType])
  rest <- typeCheckArrayDeref es
  return $ ArrayT rest

countDimension :: Type -> Int
countDimension (ArrayT t)
  = 1 + countDimension t
countDimension _
  = 0
