module Semantics.ScopeChecker where

import Utilities.Def2
type ScopeError = (String, ErrorType, Position)

scopeErrorGen :: AST -> [ScopeError]
scopeErrorGen (Program funcs main)
  = concatMap scopeErrorFunc funcs ++ scopeErrorStat main

scopeErrorFunc :: Func -> [ScopeError]
scopeErrorFunc (Func t ident paramList stat pos)
  = undefined

scopeErrorStat :: Stat -> [ScopeError]
scopeErrorStat (Declaration _ ident rhs _)
  = scopeErrorIdent ident ++ scopeErrorRHS rhs
scopeErrorStat (Assignment lhs rhs _)
  = scopeErrorLHS lhs ++ scopeErrorRHS rhs
scopeErrorStat (Read lhs _)
  = scopeErrorLHS lhs
scopeErrorStat (Free expr _)
  = scopeErrorExpr expr
scopeErrorStat (Return expr _)
  = scopeErrorExpr expr
scopeErrorStat (Exit expr _)
  = scopeErrorExpr expr
scopeErrorStat (Print expr _)
  = scopeErrorExpr expr
scopeErrorStat (Println expr _)
  = scopeErrorExpr expr
scopeErrorStat (If expr stat1 stat2 _)
  = scopeErrorExpr expr ++ scopeErrorStat stat1 ++ scopeErrorStat stat2
scopeErrorStat (While expr stat _)
  = scopeErrorExpr expr ++ scopeErrorStat stat
scopeErrorStat (Block stat _)
  = scopeErrorStat stat
scopeErrorStat (Seq stat1 stat2 _)
  = scopeErrorStat stat1 ++ scopeErrorStat stat2
scopeErrorStat _
  = []

scopeErrorRHS (ExprAssign expr _)
  = scopeErrorExpr expr
scopeErrorRHS (ArrayLitAssign exprs _)
  = concatMap scopeErrorExpr exprs
scopeErrorRHS (NewPairAssign expr1 expr2 _)
  = scopeErrorExpr expr1 ++ scopeErrorExpr expr2
scopeErrorRHS (PairElemAssign (PairElem _ expr _) _)
  = scopeErrorExpr expr
scopeErrorRHS (FuncCallAssign ident exprs _)
  = scopeErrorIdent ident ++ concatMap scopeErrorExpr exprs

scopeErrorLHS (Var ident _)
  = scopeErrorIdent ident
scopeErrorLHS (ArrayDeref (ArrayElem ident exprs _) _)
  = scopeErrorIdent ident ++ concatMap scopeErrorExpr exprs
scopeErrorLHS (PairDeref (PairElem _ expr _) _)
  = scopeErrorExpr expr

scopeErrorExpr (IdentE ident _)
  = scopeErrorIdent ident
scopeErrorExpr (ExprArray (ArrayElem ident exprs _) _)
  = scopeErrorIdent ident ++ concatMap scopeErrorExpr exprs
scopeErrorExpr (UnaryApp _ expr _)
  = scopeErrorExpr expr
scopeErrorExpr (BinaryApp _ expr1 expr2 _)
  = scopeErrorExpr expr1 ++ scopeErrorExpr expr2
scopeErrorExpr expr
  = []

scopeErrorIdent (Ident string (ScopeError err) pos)
  = [(string, err, pos)]
scopeErrorIdent _
  = []
