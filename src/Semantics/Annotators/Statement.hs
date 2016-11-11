{-
  Annotates AST statements with types and scope errors.
-}
  
module Semantics.Annotators.Statement ( annotateStat ) where

import Control.Monad (liftM2, mapM)
import Control.Monad.State.Strict ( liftM2, liftM3, liftM4 )

import Semantics.Annotators.Expression ( annotateExpr, annotateExprList )
import Semantics.Annotators.Identifier ( annotateIdent, annotateNewIdent )
import Semantics.Annotators.Util ( inChildScope, inChildScopeAndWrap )
import Utilities.Definitions

annotateStat :: Stat -> LexicalScoper Stat
annotateStat s@Skip{}
  = return s

--NOTE: Must annotate RHS before LHS to handle local re-declaration of variable 
--      from parent scope.
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

annotateStat (Block s pos) =
  newStat <- (annotateStat s)
  return $ inChildScope (Block newStat pos)

annotateStat (Seq s1 s2 pos) = do
  liftM3 Seq (annotateStat s1) (annotateStat s2) (return pos)

-- Annotates an AssignLHS
annotateLHS :: AssignLHS -> LexicalScoper AssignLHS
annotateLHS (Var ident pos)
  = Var <$> annotateIdent Variable ident <*> return pos
annotateLHS (ArrayDeref (ArrayElem ident exprs pos1) pos2)
  = ArrayDeref <$>
      liftM3
        ArrayElem
          (annotateIdent Variable ident)
          (annotateExprList exprs)
          (return pos1)
      <*> (return pos2)
annotateLHS (PairDeref (PairElem elemSelector expr pos1) pos2)
  = liftM2 PairDeref (PairElem elemSelector <$> (annotateExpr expr)
                                            <*> (return pos1))
                                            (return pos2)

-- Annotates an AssignRHS
annotateRHS :: AssignRHS -> LexicalScoper AssignRHS
annotateRHS (ExprAssign expr pos)
  = ExprAssign <$> annotateExpr expr <*> (return pos)

annotateRHS (ArrayLitAssign es pos)
  = ArrayLitAssign <$> annotateExprList es <*> (return pos)

-- fstExpr should not change state of sndExpr as exprs are side effect free
annotateRHS (NewPairAssign fstExpr sndExpr pos)
  = liftM3
      NewPairAssign
      (annotateExpr fstExpr)
      (annotateExpr sndExpr)
      (return pos)

annotateRHS (PairElemAssign (PairElem selector expr pos1) pos2)
  = liftM2 PairElemAssign (PairElem selector <$> (annotateExpr expr)
                                             <*> (return pos1))
                                             (return pos2)

annotateRHS (FuncCallAssign f es pos)
  = liftM3
      FuncCallAssign
      (annotateIdent Function f)
      (annotateExprList es)
      (return pos)
