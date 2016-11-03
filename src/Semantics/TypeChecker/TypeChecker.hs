module Semantics.TypeChecker.TypeChecker where

import Control.Monad.Writer.Strict
import Utilities.Definitions

type TypeErrMsg = String

typeCheckStat :: Stat -> Writer [TypeErrMsg] ()
typeCheckStat (Declaration t ident rhs) = do
  typeRHS <- typeCheckRHS rhs
  when (typeRHS /= t) (tell ["Type mismatch in declaration"])

typeCheckStat (Assignment lhs rhs) = do
  typeLHS <- typeCheckLHS lhs
  typeRHS <- typeCheckRHS rhs
  when (typeLHS /= typeRHS) (tell ["Type mismatch in declaration"])

typeCheckStat (Read lhs)
  = void $ typeCheckLHS lhs

typeCheckStat (Free expr@(IdentE _))
  = return ()

typeCheckStat (Free _)
  = tell ["Free called with invalid args"]

typeCheckStat (Exit (IntLit _))
  = return ()

typeCheckStat (Exit _)
  = tell ["Exit passed non integer arg"]

typeCheckStat (If cond s1 s2) = do
  expr <- typeCheckExpr cond
  when (expr /= BaseT BaseBool) (tell ["If condition not valid"])

typeCheckStat (While cond stat) = do
  expr <- typeCheckExpr cond
  when (expr /= BaseT BaseBool) (tell ["While condition not valid"])

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
      return (constructArrayType (countDimension (head types)) (head types))

typeCheckRHS (NewPairAssign e e') = do
  e1 <- typeCheckExpr e
  e2 <- typeCheckExpr e'
  -- are we sure we want typeErr to be equal to everything
  -- because what do we return in this case then?
  -- if any of the sub exprs are TypeErr, how do we check for it??
  return undefined

typeCheckExpr :: Expr -> Writer [TypeErrMsg] Type
typeCheckExpr (IntLit _)
  = return (BaseT BaseInt)
typeCheckExpr (ExprArray arrayElem@(ArrayElem ident es))
  = typeCheckArrayDeref ident es

-- Returns the type of an identifier
identGetType :: Ident -> Type
identGetType (Ident _ (Info t _))
  = t

-- checks type of each expression is an int, by delegating to helper that writes error msgs
-- check type of ident derefence
typeCheckArrayDeref :: Ident -> [Expr] -> Writer [TypeErrMsg] Type
typeCheckArrayDeref (Ident _ (Info (ArrayT a@(Array n t)) Variable)) es = do
  mapM_ (checkExprHasType (BaseT BaseInt)) es
  if n < length es
    then tell ["Cannot derefence; not an array."] >> return TypeErr
    else return $ ArrayT a
typeCheckArrayDeref _ _
  = tell ["Cannot derefence; not an array."] >> return TypeErr

-- Gets the type of the innermost elements of an array. Returns TypeErr if ident

-- Checks an expression has Type Int
checkExprHasType :: Type -> Expr -> Writer [TypeErrMsg] ()
checkExprHasType t e = do
  eType <- typeCheckExpr e
  when (eType /= t)
      (tell ["type of array index must be int, actually: " ++ show t])

-- -- 'constructArrayType n t' Constructs an n dimensional array with elemet type t
-- constructArrayType :: Int -> Type -> ArrayType
-- constructArrayType _ TypeErr
--   = error ("Assertion failed in TypeChecker.hs: constructing array type from" ++
--            "TypeErr")
-- constructArrayType 0 t
--   = t
-- constructArrayType n t
--   = ArrayT (constructArrayType (n - 1) t)
--
-- countDimension :: Type -> Int
-- countDimension (ArrayT t)
--   = 1 + countDimension t
-- countDimension _
--   = 0
