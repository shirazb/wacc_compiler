{-
  Annotates AST statement nodes with types and scope errors. Each function 
  defined here traverses the structure of a statement node and delegates 
  annotation of their constituent fields to annotators defined elsewhere.
-}
  
module Semantics.Annotators.Statement ( annotateStat ) where

import Semantics.Annotators.Expression ( annotateExpr, annotateExprList )
import Semantics.Annotators.Identifier ( annotateIdent, annotateNewIdent )
import Semantics.Annotators.Util ( inChildScope )
import Utilities.Definitions

annotateStat :: Stat -> LexicalScoper Stat
annotateStat s@Skip{}
  = return s

--NOTE: Must annotate RHS before LHS to allow local re-declaration of a variable 
--      declared in a parent scope.
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

annotateLHS :: AssignLHS -> LexicalScoper AssignLHS
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

annotateRHS :: AssignRHS -> LexicalScoper AssignRHS
annotateRHS (ExprAssign expr pos) = do
  newExpr <- annotateExpr expr
  return $ ExprAssign newExpr pos

annotateRHS (ArrayLitAssign exprList pos) = do
  newExprList <- annotateExprList exprList
  return $ ArrayLitAssign newExprList pos

-- NOTE: Expressions have no side effects, so fst and snd can be annotated 
--       sequentially - the annotation of fst won't affect that of snd, because
--       annotating fst won't modify the state of the symbol table.
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
