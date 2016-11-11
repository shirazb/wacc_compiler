{- This module annotates statements -}

module Semantics.Annotators.Statement (annotateStat) where

import Control.Monad (liftM2, mapM)
import Control.Monad.State.Strict
import qualified Data.Map as Map

{- LOCAL IMPORTS -}
import Semantics.ErrorMessages
import Semantics.Annotators.Expression
import Semantics.Annotators.Identifier
import Semantics.Annotators.Util
import Utilities.Definitions

-- POST: Annotates statements
annotateStat :: Stat -> LexicalScoper Stat

annotateStat (Skip pos)
  = return (Skip pos)

annotateStat (Declaration t ident rhs pos) = do
  newRHS   <- annotateRHS rhs
  newIdent <- annotateNewIdent ident (Info t Variable)
  return $ Declaration t newIdent newRHS pos

annotateStat (Assignment lhs rhs pos)
  = Assignment <$> (annotateLHS lhs) <*> (annotateRHS rhs) <*> return pos

annotateStat (Read lhs pos)
  = Read <$> annotateLHS lhs <*> return pos

annotateStat (Free expr pos)
  = Free <$> annotateExpr expr <*> return pos

annotateStat (Return expr pos)
  = Return <$> annotateExpr expr <*> return pos

annotateStat (Exit expr pos)
  = Exit <$> annotateExpr expr <*> return pos

annotateStat (Print expr pos)
  = Print <$> annotateExpr expr <*> return pos

annotateStat (Println expr pos)
  = Println <$> annotateExpr expr <*> return pos

annotateStat (If cond thenStat elseStat pos)
  = liftM4
      If
      (annotateExpr cond)
      (inChildScope (annotateStat thenStat))
      (inChildScope (annotateStat elseStat))
      (return pos)

annotateStat (While cond body pos)
  = liftM3
      While
      (annotateExpr cond)
      (inChildScope (annotateStat body))
      (return pos)

annotateStat (Block s pos)
  = (inChildScopeAndWrap Block (annotateStat s)) <*> (return pos)

annotateStat (Seq s1 s2 pos)
  = liftM3 Seq (annotateStat s1) (annotateStat s2) (return pos)

-- POST: Annotates an AssignLHS
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

-- POST: Annotates an AssignRHS
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
