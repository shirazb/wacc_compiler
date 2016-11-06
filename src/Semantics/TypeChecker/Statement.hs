{-# LANGUAGE LambdaCase #-}

module Semantics.TypeChecker.Statement (typeCheckStat) where

import Control.Monad.Writer.Strict

import Semantics.TypeChecker.AssignLHS
import Semantics.TypeChecker.AssignRHS
import Semantics.TypeChecker.Expression
import Utilities.Definitions

typeCheckStat :: Stat -> TypeChecker ()
typeCheckStat (Declaration t ident rhs) = do
  typeRHS <- typeCheckRHS rhs
  when (typeRHS /= t) (tell ["mismatch"])

typeCheckStat (Assignment lhs rhs) = do
  typeLHS <- typeCheckLHS lhs
  typeRHS <- typeCheckRHS rhs
  when (typeLHS /= typeRHS) (tell ["Type mismatch"])

typeCheckStat (Read lhs)
  = void $ typeCheckLHS lhs

typeCheckStat (Free expr)
  = typeCheckExpr expr >>= \case
      ArrayT _ _ -> return ()
      PairT _ _  -> return ()
      NoType     -> return ()
      _          -> tell ["freeing thing not on the heap"]

typeCheckStat (Exit (IntLit _))
  = return ()

typeCheckStat (Exit _)
  = tell ["Exit passed non integer arg"]

typeCheckStat (If cond s1 s2) = do
  expr <- typeCheckExpr cond
  when (expr /= BaseT BaseBool) (tell ["If condition not valid"])
  typeCheckStat s1
  typeCheckStat s2

typeCheckStat (While cond stat) = do
  expr <- typeCheckExpr cond
  when (expr /= BaseT BaseBool) (tell ["While condition not valid"])
  typeCheckStat stat

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
