{- This module descends the AST and reports generated scope error messages -}

module Semantics.ScopeErrorGenerator where

{- LOCAL IMPORTS -}
import Semantics.ErrorMessages
import Utilities.Definitions

-- POST: Traverses the program and collects generated scope error messages
scopeCheckProgram :: AST -> [String]
scopeCheckProgram ast
  = map mkScopeErrMsg scopeErrs
  where
    scopeErrs = scopeErrorAST ast

-- POST: Traverses the AST and collects generated scope error messages
scopeErrorAST :: AST -> [ScopeError]
scopeErrorAST (Program funcs main)
  = concatMap scopeErrorFunc funcs ++ scopeErrorStat main

-- POST: Traverses functions and collects generated scope error messages
scopeErrorFunc :: Func -> [ScopeError]
scopeErrorFunc (Func t ident (ParamList params _) stat pos)
  = scopeErrorIdent ident pos ++ concatMap scopeErrorParam params
      ++ scopeErrorStat stat

-- POST: Traverses statements and collects generated scope error messages
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

scopeErrorStat (For stat1 expr stat2 stat3 _)
  = scopeErrorExpr expr ++ scopeErrorStat stat1 
                        ++ scopeErrorStat stat2
                        ++ scopeErrorStat stat3

scopeErrorStat (Block stat _)
  = scopeErrorStat stat


scopeErrorStat (Seq stat1 stat2 _)
  = scopeErrorStat stat1 ++ scopeErrorStat stat2

scopeErrorStat _
  = []

-- POST: Traverses parameters and collects generated scope error messages
scopeErrorParam :: Param -> [(String, ScopeErrorType, Position)]
scopeErrorParam (Param _ ident pos)
  = scopeErrorIdent ident pos

-- POST: Traverses RHS assignments and collects generated scope error messages
scopeErrorRHS :: AssignRHS -> [(String, ScopeErrorType, Position)]

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

-- POST: Traverses LHS assignments and collects generated scope error messages
scopeErrorLHS :: AssignLHS -> [(String, ScopeErrorType, Position)]

scopeErrorLHS (Var ident pos)
  = scopeErrorIdent ident pos

scopeErrorLHS (ArrayDeref (ArrayElem ident exprs pos) _)
  = scopeErrorIdent ident pos ++ concatMap scopeErrorExpr exprs

scopeErrorLHS (PairDeref (PairElem _ expr _) _)
  = scopeErrorExpr expr

-- POST: Traverses expressions and collects generated scope error messages
scopeErrorExpr :: Expr -> [(String, ScopeErrorType, Position)]

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

-- POST: Traverses identifiers and collects generated scope error messages
scopeErrorIdent :: Ident -> t -> [(String, ScopeErrorType, t)]
scopeErrorIdent (Ident string (ScopeError err)) pos
  = [(string, err, pos)]
scopeErrorIdent _ _
  = []
