{- This module annotates statements -}

module Semantics.Annotators.Statement (annotateStat) where

import Control.Monad (liftM2, mapM)
import Control.Monad.State.Strict
import qualified Data.Map as Map

{- LOCAL IMPORTS -}
import Semantics.Annotators.Expression ( annotateExpr, annotateExprList,
                                         annotateFuncCall, annotateMemberAccess)
import Semantics.Annotators.Identifier ( annotateIdent, annotateNewIdent )
import Semantics.Annotators.Type (scopeCheckType)
import Semantics.Annotators.Util
import Semantics.ErrorMessages
import Utilities.Definitions
import Debug.Trace

-- POST: Annotates statements
annotateStat :: Stat -> ScopeAnalysis Stat

annotateStat s@Skip{}
  = return s

annotateStat b@Break{}
  = return b

annotateStat c@Continue{}
  = return c

annotateStat r@ReturnVoid{}
  = return r

annotateStat (Declaration t ident rhs pos) = do
  newT     <- scopeCheckType t
  newRHS   <- annotateRHS rhs
  newIdent <- annotateNewIdent ident (Info Static newT Variable)
  return $ Declaration newT newIdent newRHS pos

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
  newCond <- annotateExpr cond
  newBody <- inChildScope $ annotateStat body
  return $ While newCond newBody pos

annotateStat (For decl cond assign loopBody pos) = inChildScope $ do
  newDecl     <- annotateStat decl
  newCond     <- annotateExpr cond
  newAssign   <- annotateStat assign
  newLoopBody <- annotateStat loopBody
  return $ For newDecl newCond newAssign newLoopBody pos

annotateStat (Block s pos) = do
  newStat <- inChildScope (annotateStat s)
  return $ Block newStat pos

annotateStat (CallFunc call pos)
  = CallFunc <$> annotateFuncCall call <*> return pos

-- TODO: make sure in typechecking we ensure that this expression
-- is always an object
annotateStat (CallMethod ma pos)
  = CallMethod <$> annotateMemberAccess ma <*> return pos

annotateStat (Seq s1 s2 pos) = do
  newStat1 <- annotateStat s1
  newStat2 <- annotateStat s2
  return $ Seq newStat1 newStat2 pos

annotateStat s
  = error $ "No pattern for annotateStat of " ++ pretty s

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

annotateLHS (MemberDeref ma pos) = do
  ma' <- annotateMemberAccess ma
  return $ MemberDeref ma' pos

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

annotateRHS (FuncCallAssign funcCall pos)
  = FuncCallAssign <$> annotateFuncCall funcCall <*> return pos

annotateRHS (ConstructAssign funcCall pos)
  = ConstructAssign <$> annotateFuncCall funcCall <*> return pos
