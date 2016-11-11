{- This module annotates statements -}

module Semantics.Annotators.Statement (annotateStat) where

import Control.Monad (liftM2, mapM)
import Control.Monad.State.Strict
import qualified Data.Map as Map

{- LOCAL IMPORTS -}
import Semantics.ErrorMessages
import Semantics.Annotators.Expression ( annotateExpr, annotateExprList )
import Semantics.Annotators.Identifier ( annotateIdent, annotateNewIdent )
import Semantics.Annotators.Util ( inChildScope )
import Utilities.Definitions

-- POST: Annotates statements
annotateStat :: Stat -> ScopeAnalysis Stat

annotateStat s@Skip{}
  = return s

annotateStat (Declaration t ident rhs pos) = do
  newRHS   <- annotateRHS rhs
  newIdent <- annotateNewIdent ident (Info t Variable)
  return $ Declaration t newIdent newRHS pos

annotateStat (Assignment lhs rhs pos) = do
  newLHS <- annotateLHS lhs
  newRHS <- annotateRHS rhs
  return $ Assignment newLHS newRHS pos

annotateStat (Read lhs pos) = do
  newLHS <- annotateLHS lhs
  return $ Read newLHS pos

annotateStat (Free expr pos) = do
  newExpr <- annotateExpr expr
  return $ Free newExpr pos

annotateStat (Return expr pos) = do
  newExpr <- annotateExpr expr
  return $ Return newExpr pos

annotateStat (Exit expr pos) = do
  newExpr <- annotateExpr expr
  return $ Exit newExpr pos

annotateStat (Print expr pos) = do
  newExpr <- annotateExpr expr
  return $ Print newExpr pos

annotateStat (Println expr pos) = do
  newExpr <- annotateExpr expr
  return $ Println newExpr pos

annotateStat (If cond thenStat elseStat pos) = do
  newCond     <- annotateExpr cond
  newThenStat <- inChildScope $ annotateStat thenStat
  newElseStat <- inChildScope $ annotateStat elseStat
  return $ If newCond newThenStat newElseStat pos

annotateStat (While cond body pos) = do
  newCond     <- annotateExpr cond
  newBody <- inChildScope $ annotateStat body
  return $ While newCond newBody pos

annotateStat (Block s pos) = do
  newStat <- inChildScope (annotateStat s)
  return $ Block newStat pos

annotateStat (Seq s1 s2 pos) = do
  newStat1 <- annotateStat s1
  newStat2 <- annotateStat s2
  return $ Seq newStat1 newStat2 pos

-- POST: Annotates an AssignLHS
annotateLHS :: AssignLHS -> ScopeAnalysis AssignLHS

annotateLHS (Var ident pos) = do
  newIdent <- annotateIdent Variable ident
  return $ Var newIdent pos

annotateLHS (ArrayDeref (ArrayElem ident exprs pos1) pos2) = do
  newIdent <- annotateIdent Variable ident
  newExprs <- annotateExprList exprs
  return $ ArrayDeref (ArrayElem newIdent newExprs pos1) pos2

annotateLHS (PairDeref (PairElem elemSelector expr pos1) pos2) = do
  newExpr <- annotateExpr expr
  return $ PairDeref (PairElem elemSelector newExpr  pos1) pos2

-- POST: Annotates an AssignRHS
annotateRHS :: AssignRHS -> ScopeAnalysis AssignRHS

annotateRHS (ExprAssign expr pos) = do
  newExpr <- annotateExpr expr
  return $ ExprAssign newExpr pos

annotateRHS (ArrayLitAssign exprList pos) = do
  newExprList <- annotateExprList exprList
  return $ ArrayLitAssign newExprList pos

annotateRHS (NewPairAssign fstExpr sndExpr pos) = do
  newFstExpr <- annotateExpr fstExpr
  newSndExpr <- annotateExpr sndExpr
  return $ NewPairAssign newFstExpr newSndExpr pos

annotateRHS (PairElemAssign (PairElem selector expr pos1) pos2) = do
  newExpr <- annotateExpr expr
  return $ PairElemAssign (PairElem selector newExpr pos1) pos2

annotateRHS (FuncCallAssign f exprList pos) = do
  newFunc <- annotateIdent Function f
  newExprList <- annotateExprList exprList
  return $ FuncCallAssign newFunc newExprList pos
