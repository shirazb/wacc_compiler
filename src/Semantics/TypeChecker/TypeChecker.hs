module Semantics.TypeChecker.TypeChecker where

import Control.Monad.Writer.Strict
import Utilities.Definitions

type TypeErrMsg = String

typeCheckStat :: Stat -> Writer [TypeErrMsg] ()
typeCheckStat (Declaration t ident rhs) = do
  typeRHS <- typeCheckRHS rhs
  when (typeRHS /= t) (tell ["Type mismatch in declaration"])
  return ()

typeCheckStat (Assignment lhs rhs) = do
  typeLHS <- typeCheckLHS lhs
  typeRHS <- typeCheckRHS rhs
  when (typeLHS /= typeRHS) (tell ["Type mismatch in declaration"])
  return ()

typeCheckStat (Read lhs)
  = void $ typeCheckLHS lhs

typeCheckStat (Free expr@(IdentE _)) = do
  typeCheckExpr expr
  return ()

typeCheckStat (Free _) = do
  tell ["Free called with invalid args"]
  return ()

typeCheckStat (Exit (IntLit _))
  = return ()

typeCheckStat (Exit _)
  = void $ tell ["Exit passed non integer arg"]

typeCheckStat (If cond s1 s2) = do
  expr <- typeCheckExpr cond
  when (expr /= BaseT BaseBool) (tell ["If condition not valid"])
  return ()

typeCheckStat (While cond stat) = do
  expr <- typeCheckExpr cond
  when (expr /= BaseT BaseBool) (tell ["While condition not valid"])
  return ()
typeCheckStat (Return expr)
  = void $ typeCheckExpr expr

typeCheckStat (Print expr)
  = void $ typeCheckExpr expr

typeCheckStat (Println expr)
  = void $ typeCheckExpr expr

typeCheckStat (Block s)
  = typeCheckStat s

typeCheckStat (Seq s s') = do
  typeCheckStat s
  typeCheckStat s'
  return ()

typeCheckLHS :: AssignLHS -> Writer [TypeErrMsg] Type
typeCheckLHS (Var ident)
  = return (identGetType ident)

-- we should jus call the one we have defined already
-- make it a bit more general
-- so we can reuse the same logic
-- its the same thing
typeCheckLHS (ArrayDeref a)
  = undefined

typeCheckLHS (PairDeref pe@(PairElem _ (IdentE i)))
  = return (identGetType i)

typeCheckLHS (PairDeref pe@(PairElem sel _)) = do
  tell ["Invalid arg to " ++ show sel]
  return TypeErr

typeCheckRHS :: AssignRHS -> Writer [TypeErrMsg] Type
typeCheckRHS (ExprAssign e)
  = typeCheckExpr e

-- what type do we return?
-- because the rhs of an array
-- what do we know about arrays in wacc
-- empty array is any type right, so we need something
-- that represents that
typeCheckRHS (ArrayLitAssign [])
  = return AllType
typeCheckRHS (ArrayLitAssign es) = do
  types <- mapM typeCheckExpr es
  if not $ and (zipWith (==) types (tail types))
    then do {
      tell ["ArrayLiteral Error -- not all elements same type"];
      return TypeErr;
    } else
      return (constructAType (countDimension (head types)) (head types))
typeCheckRHS (NewPairAssign e e') = undefined



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

constructAType :: Int -> Type -> ArrayType
constructAType 0 t = t
constructAType n t = ArrayT (constructAType (n - 1) t)


countDimension :: Type -> Int
countDimension (ArrayT t)
  = 1 + countDimension t
countDimension _
  = 0
