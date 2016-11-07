{-# LANGUAGE LambdaCase #-}

module Semantics.TypeChecker.Statement (
  typeCheckStat
) where

import Control.Monad.Writer.Strict

import Semantics.TypeChecker.AssignLHS
import Semantics.TypeChecker.AssignRHS
import Semantics.TypeChecker.Expression
import Utilities.Definitions

typeCheckStat :: Stat -> TypeChecker ()
typeCheckStat (Skip _)
  = return ()

typeCheckStat (Declaration t ident rhs pos) = do
  typeRHS <- typeCheckRHS rhs
  when (typeRHS /= t) (tell ["mismatch"])

typeCheckStat (Assignment lhs rhs pos) = do
  typeLHS <- typeCheckLHS lhs
  typeRHS <- typeCheckRHS rhs
  when (typeLHS /= typeRHS) (tell ["Type mismatch"])

typeCheckStat (Read lhs pos)
  = void $ typeCheckLHS lhs

typeCheckStat (Free expr pos)
  = typeCheckExpr expr >>= \case
      ArrayT _ _ -> return ()
      PairT _ _  -> return ()
      NoType     -> return ()
      _          -> tell ["freeing thing not on the heap"]

typeCheckStat (Exit expr pos) = do
  t <- typeCheckExpr expr
  when (t /= BaseT BaseInt) (tell ["exit code is fucked mate"])



typeCheckStat (If cond s1 s2 pos) = do
  expr <- typeCheckExpr cond
  when (expr /= BaseT BaseBool) (tell ["If condition not valid"])
  typeCheckStat s1
  typeCheckStat s2

typeCheckStat (While cond stat pos) = do
  exprT <- typeCheckExpr cond
  when (exprT /= BaseT BaseBool) (tell ["While condition not valid"])
  typeCheckStat stat

typeCheckStat (Return expr pos)
  = void $ typeCheckExpr expr

typeCheckStat (Print expr pos)
  = void $ typeCheckExpr expr

typeCheckStat (Println expr pos)
  = void $ typeCheckExpr expr

typeCheckStat (Block s pos)
  = typeCheckStat s

typeCheckStat (Seq s s' pos) = do
  typeCheckStat s
  typeCheckStat s'
