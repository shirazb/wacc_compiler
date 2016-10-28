module Semantics.Annotators.Statement (
  annotateStat
) where

import Control.Monad (liftM2, mapM)
import Control.Monad.State.Strict
import qualified Data.Map as Map

import Semantics.ErrorMsgs
import Semantics.Annotators.Expression
import Semantics.Annotators.Identifier
import Semantics.Annotators.Util
import Utilities.Definitions

annotateStat :: Stat -> LexicalScoper Stat

-- Cannot use lift: Must annotate RHS first so new identifier is not in
-- its symbol table, otherwise "int x = x + 3" would be valid, for example.
annotateStat (Declaration t ident rhs) = do
  newRHS   <- annotateRHS rhs
  newIdent <- annotateNewIdent ident
  return $ Declaration t newIdent newRHS

annotateStat (Assignment lhs rhs)
  = liftM2 Assignment (annotateLHS lhs) (annotateRHS rhs)

annotateStat (Read lhs)
  = Read <$> annotateLHS lhs
annotateStat (Free expr)
  = Free <$> annotateExpr expr
annotateStat (Return expr)
  = Return <$> annotateExpr expr
annotateStat (Exit expr)
  = Exit <$> annotateExpr expr
annotateStat (Print expr)
  = Print <$> annotateExpr expr
annotateStat (Println expr)
  = Println <$> annotateExpr expr

annotateStat (If cond thenStat elseStat)
  = liftM3
      If
      (annotateExpr cond)
      (inChildScope (annotateStat thenStat))
      (inChildScope (annotateStat elseStat))

annotateStat (While cond body)
  = liftM2 While (annotateExpr cond) (inChildScope (annotateStat body))

annotateStat (Block s)
  = inChildScopeAndWrap Block (annotateStat s)

annotateStat (Seq s1 s2)
  = liftM2 Seq (annotateStat s1) (annotateStat s2)

-- Annotates an AssignLHS
annotateLHS :: AssignLHS -> LexicalScoper AssignLHS
annotateLHS (Var ident)
  = Var <$> annotateIdent ident
annotateLHS (ArrayDeref (ArrayElem ident exprs))
  = ArrayDeref <$>
      liftM2 ArrayElem (annotateIdent ident) (annotateExprList exprs)
annotateLHS (PairDeref (PairElem elemSelector expr))
  = (PairDeref . PairElem elemSelector) <$> annotateExpr expr

-- Annotates an AssignRHS
annotateRHS :: AssignRHS -> LexicalScoper AssignRHS
annotateRHS (ExprAssign expr)
  = ExprAssign <$> annotateExpr expr

annotateRHS (ArrayLitAssign es)
  = ArrayLitAssign <$> annotateExprList es

-- fstExpr should not change state of sndExpr as exprs are side effect free
annotateRHS (NewPairAssign fstExpr sndExpr)
  = liftM2 NewPairAssign (annotateExpr fstExpr) (annotateExpr sndExpr)

annotateRHS (PairElemAssign (PairElem selector expr))
  = (PairElemAssign . PairElem selector) <$> annotateExpr expr

annotateRHS (FuncCallAssign f es)
  = liftM2 FuncCallAssign (annotateIdent f) (annotateExprList es)
