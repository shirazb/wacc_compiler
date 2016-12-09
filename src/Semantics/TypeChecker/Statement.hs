{- This module type checks statements -}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}

module Semantics.TypeChecker.Statement (typeCheckStat) where

import Control.Monad.Writer.Strict

{- LOCAL IMPORTS -}
import Semantics.Annotators.Util
import Semantics.TypeChecker.AssignLHS
import Semantics.TypeChecker.AssignRHS
import Semantics.TypeChecker.Expression
import Semantics.ErrorMessages
import Utilities.Definitions

import Semantics.Annotators.Util (typeInfo, memberIdent)

-- POST: Type checks statements
typeCheckStat :: Stat -> TypeChecker ()

typeCheckStat stat@(Declaration t ident rhs pos) = do
  typeRHS <- typeCheckRHS rhs
  when (typeRHS /= t) (tell [typeMismatch t typeRHS (getPosRHS rhs) stat])

-- If we see Self, we know from the success of the scope checking that we must
-- be in a class
typeCheckStat stat@(Assignment lhs rhs pos) =
  if |  Self _  <- lhsIdent lhs -> tell [selfAssign lhs stat pos]
     |  otherwise -> do
        typeLHS <- typeCheckLHS lhs
        typeRHS <- typeCheckRHS rhs
        when (typeLHS /= typeRHS) (tell [typeMismatch typeLHS typeRHS (getPosRHS rhs) stat])

typeCheckStat (Read lhs pos) = do
  t <- typeCheckLHS lhs
  unless (checkCharOrInt t) (tell [typeMismatch RelationalT t pos lhs])
  return ()

typeCheckStat (Free expr pos)
  = typeCheckExpr expr >>= \case
      ArrayT _ _ -> return ()
      PairT _ _  -> return ()
      NoType     -> return ()
      _          -> tell [freeNonHeapObject pos]

typeCheckStat exitStat@(Exit expr pos) = do
  t <- typeCheckExpr expr
  when (t /= BaseT BaseInt) (tell [typeMismatch (BaseT BaseInt) t pos exitStat])

typeCheckStat (If cond s1 s2 pos) = do
  exprT <- typeCheckExpr cond
  when (exprT /= BaseT BaseBool)
    (tell [typeMismatch (BaseT BaseBool) exprT (getPosExpr cond) cond])
  typeCheckStat s1
  typeCheckStat s2

typeCheckStat (While cond stat pos) = do
  exprT <- typeCheckExpr cond
  when (exprT /= BaseT BaseBool)
    (tell [typeMismatch (BaseT BaseBool) exprT (getPosExpr cond) cond])
  typeCheckStat stat

typeCheckStat (For decl cond assign loopBody pos) = do
  typeCheckStat decl
  exprT <- typeCheckExpr cond
  typeCheckStat assign
  when (exprT /= BaseT BaseBool)
    (tell [typeMismatch (BaseT BaseBool) exprT (getPosExpr cond) cond])
  typeCheckStat loopBody

typeCheckStat (Return expr pos)
  = void $ typeCheckExpr expr

typeCheckStat (Print expr pos)
  = void $ typeCheckExpr expr

typeCheckStat (Println expr pos)
  = void $ typeCheckExpr expr

typeCheckStat (Block s pos)
  = typeCheckStat s

typeCheckStat (CallFunc fc pos)
  = void $ typeCheckFuncCall fc pos

typeCheckStat (CallMethod ma _)
  = void $ typeCheckMemberAccess ma

typeCheckStat (Seq s s' pos) = do
  typeCheckStat s
  typeCheckStat s'

--Catches Skip, Break, Continue
typeCheckStat _
  = return ()
