{-# LANGUAGE LambdaCase #-}

module Semantics.TypeChecker.Statement (
  typeCheckStat
) where

import Control.Monad.Writer.Strict

import Semantics.TypeChecker.AssignLHS
import Semantics.TypeChecker.AssignRHS
import Semantics.TypeChecker.Expression
import Utilities.Definitions
import Semantics.ErrorMessages

typeCheckStat :: Stat -> TypeChecker ()
typeCheckStat (Skip _)
  = return ()

typeCheckStat stat@(Declaration t ident rhs pos) = do
  typeRHS <- typeCheckRHS rhs
  when (typeRHS /= t) (tell [typeMismatch t typeRHS pos stat])

typeCheckStat stat@(Assignment lhs rhs pos) = do
  typeLHS <- typeCheckLHS lhs
  typeRHS <- typeCheckRHS rhs
  when (typeLHS /= typeRHS) (tell [typeMismatch typeLHS typeRHS pos stat])

typeCheckStat (Read lhs pos)
  = void $ typeCheckLHS lhs

typeCheckStat (Free expr pos)
  = typeCheckExpr expr >>= \case
      ArrayT _ _ -> return ()
      PairT _ _  -> return ()
      NoType     -> return ()
      _          -> tell ["freeing thing not on the heap"]

typeCheckStat exitStat@(Exit expr pos) = do
  t <- typeCheckExpr expr
  when (t /= BaseT BaseInt) (tell [typeMismatch (BaseT BaseInt) t pos exitStat])



typeCheckStat (If cond s1 s2 pos) = do
  exprT <- typeCheckExpr cond
  when (exprT /= BaseT BaseBool) (tell [typeMismatch (BaseT BaseBool) exprT pos cond])
  typeCheckStat s1
  typeCheckStat s2

typeCheckStat (While cond stat pos) = do
  exprT <- typeCheckExpr cond
  when (exprT /= BaseT BaseBool) (tell [typeMismatch (BaseT BaseBool) exprT pos cond])
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