module Semantics.ScopeChecker where
import Utilities.Definitions
type ScopeError = (String, ScopeErrorType, Position)

scopeErrorGen :: AST -> [ScopeError]
scopeErrorGen (Program funcs main)
  = concatMap scopeErrorFunc funcs ++ scopeErrorStat main

scopeErrorFunc :: Func -> [ScopeError]
scopeErrorFunc (Func t ident (ParamList params _) stat pos)
  = scopeErrorIdent ident pos ++ concatMap scopeErrorParam params
                          ++ scopeErrorStat stat

scopeErrorStat :: Stat -> [ScopeError]
scopeErrorStat (Declaration _ ident rhs pos)
  = scopeErrorIdent ident pos ++ scopeErrorRHS rhs
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

scopeErrorParam (Param _ ident pos)
  = scopeErrorIdent ident pos

scopeErrorRHS (ExprAssign expr _)
  = scopeErrorExpr expr
scopeErrorRHS (ArrayLitAssign exprs _)
  = concatMap scopeErrorExpr exprs
scopeErrorRHS (NewPairAssign expr1 expr2 _)
  = scopeErrorExpr expr1 ++ scopeErrorExpr expr2
scopeErrorRHS (PairElemAssign (PairElem _ expr _) _)
  = scopeErrorExpr expr
scopeErrorRHS (FuncCallAssign ident exprs pos)
  = scopeErrorIdent ident pos ++ concatMap scopeErrorExpr exprs

scopeErrorLHS (Var ident pos)
  = scopeErrorIdent ident pos
scopeErrorLHS (ArrayDeref (ArrayElem ident exprs pos) _)
  = scopeErrorIdent ident pos ++ concatMap scopeErrorExpr exprs
scopeErrorLHS (PairDeref (PairElem _ expr _) _)
  = scopeErrorExpr expr

scopeErrorExpr (IdentE ident pos)
  = scopeErrorIdent ident pos
scopeErrorExpr (ExprArray (ArrayElem ident exprs pos) _)
  = scopeErrorIdent ident pos ++ concatMap scopeErrorExpr exprs
scopeErrorExpr (UnaryApp _ expr _)
  = scopeErrorExpr expr
scopeErrorExpr (BinaryApp _ expr1 expr2 _)
  = scopeErrorExpr expr1 ++ scopeErrorExpr expr2
scopeErrorExpr expr
  = []

scopeErrorIdent (Ident string (ScopeError err)) pos
  = [(string, err, pos)]
scopeErrorIdent _ _
  = []
