{- This module descends the AST and reports generated scope error messages -}

module Semantics.ScopeErrorGenerator where

{- LOCAL IMPORTS -}
import Semantics.ErrorMessages
import Semantics.Annotators.Util
import Utilities.Definitions

-- POST: Traverses the program and collects generated scope error messages
scopeCheckProgram :: AST -> [String]
scopeCheckProgram ast
  = map mkScopeErrMsg (scopeErrorAST ast)

-- POST: Traverses the AST and collects generated scope error messages
scopeErrorAST :: AST -> [ScopeError]
scopeErrorAST (Program cs funcs main)
  = concatMap scopeErrorFunc funcs ++ concatMap scopeErrorClass cs ++ scopeErrorStat main

-- POST: Traverses a classes and collects generated scope error messages
scopeErrorClass :: Class -> [ScopeError]
scopeErrorClass (Class ident fields constr methods pos)
  = scopeErrorIdent ident pos
  ++ concatMap scopeErrorField fields
  ++ scopeErrorConstr constr
  ++ concatMap scopeErrorFunc methods

scopeErrorField :: Field -> [ScopeError]
scopeErrorField (Field t i pos)
  = scopeErrorType t pos ++ scopeErrorIdent i pos

scopeErrorConstr :: Constructor -> [ScopeError]
scopeErrorConstr (Constructor (ParamList ps _) body pos)
  = concatMap scopeErrorParam ps ++ scopeErrorStat body

-- POST: Traverses functions and collects generated scope error messages
scopeErrorFunc :: Func -> [ScopeError]
scopeErrorFunc (Func t ident (ParamList params _) stat pos)
  = scopeErrorType t pos ++ scopeErrorIdent ident pos ++ concatMap scopeErrorParam params
      ++ scopeErrorStat stat

-- POST: Traverses statements and collects generated scope error messages
scopeErrorStat :: Stat -> [ScopeError]

scopeErrorStat (Declaration t ident rhs pos)
  = scopeErrorType t pos ++ scopeErrorIdent ident pos ++ scopeErrorRHS rhs

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

scopeErrorStat (CallFunc fc pos)
  = scopeErrorFuncCall fc pos

scopeErrorStat (CallMethod ma pos)
  = scopeErrorMemAcc ma

scopeErrorStat (Seq stat1 stat2 _)
  = scopeErrorStat stat1 ++ scopeErrorStat stat2

scopeErrorStat _
  = []

-- POST: Traverses parameters and collects generated scope error messages
scopeErrorParam :: Param -> [ScopeError]
scopeErrorParam (Param _ ident pos)
  = scopeErrorIdent ident pos

-- POST: Traverses RHS assignments and collects generated scope error messages
scopeErrorRHS :: AssignRHS -> [ScopeError]

scopeErrorRHS (ExprAssign expr _)
  = scopeErrorExpr expr

scopeErrorRHS (ArrayLitAssign exprs _)
  = concatMap scopeErrorExpr exprs

scopeErrorRHS (NewPairAssign expr1 expr2 _)
  = scopeErrorExpr expr1 ++ scopeErrorExpr expr2

scopeErrorRHS (PairElemAssign (PairElem _ expr _) _)
  = scopeErrorExpr expr

scopeErrorRHS (FuncCallAssign fc pos)
  = scopeErrorFuncCall fc pos

scopeErrorRHS (ConstructAssign fc pos)
  = scopeErrorFuncCall fc pos

-- POST: Traverses LHS assignments and collects generated scope error messages
scopeErrorLHS :: AssignLHS -> [ScopeError]

scopeErrorLHS (Var ident pos)
  = scopeErrorIdent ident pos

scopeErrorLHS (ArrayDeref (ArrayElem ident exprs pos) _)
  = scopeErrorIdent ident pos ++ concatMap scopeErrorExpr exprs

scopeErrorLHS (PairDeref (PairElem _ expr _) _)
  = scopeErrorExpr expr

scopeErrorLHS (MemberDeref ma _)
  = scopeErrorMemAcc ma

-- POST: Traverses expressions and collects generated scope error messages
scopeErrorExpr :: Expr -> [ScopeError]

scopeErrorExpr (IdentE ident pos)
  = scopeErrorIdent ident pos

scopeErrorExpr (ExprArray (ArrayElem ident exprs pos) _)
  = scopeErrorIdent ident pos ++ concatMap scopeErrorExpr exprs

scopeErrorExpr (UnaryApp _ expr _)
  = scopeErrorExpr expr

scopeErrorExpr (BinaryApp _ expr1 expr2 _)
  = scopeErrorExpr expr1 ++ scopeErrorExpr expr2

scopeErrorExpr (ExprMemberAccess ma pos)
  = scopeErrorMemAcc ma

scopeErrorExpr expr
  = []

scopeErrorType :: Type -> Position -> [ScopeError]
scopeErrorType (FuncT t ts) pos
  = scopeErrorType t pos ++ concatMap (flip scopeErrorType pos) ts
scopeErrorType (NotInScopeT cname) pos
  = [(cname, ClassNotInScope cname, pos)]
scopeErrorType _ _
  = []

-- POST: Traverses member accesses and collects genereated scope error messages
scopeErrorMemAcc :: MemberAccess -> [ScopeError]
scopeErrorMemAcc (MemList inst ms pos)
  = scopeErrorInst inst ++ concatMap scopeErrorMem ms

-- POST: Traverses
scopeErrorInst :: Instance -> [ScopeError]
scopeErrorInst (VarObj i pos)
  = scopeErrorIdent i pos

scopeErrorInst (FuncReturnsObj fc pos)
  = scopeErrorFuncCall fc pos

-- POST:
scopeErrorMem :: Member -> [ScopeError]
scopeErrorMem (FieldAccess i pos)
  = scopeErrorIdent i pos

scopeErrorMem (MethodCall fc pos)
  = scopeErrorFuncCall fc pos

-- POST :
scopeErrorFuncCall :: FuncCall -> Position -> [ScopeError]
scopeErrorFuncCall (FuncCall ident es) pos
  = scopeErrorIdent ident pos ++ concatMap scopeErrorExpr es


-- POST: Traverses identifiers and collects generated scope error messages
scopeErrorIdent :: Ident -> Position -> [(String, ScopeErrorType, Position)]
scopeErrorIdent (Ident string (ScopeError err)) pos
  = [(string, err, pos)]
scopeErrorIdent (Self (ScopeError err)) pos
  = [(self, err, pos)]
scopeErrorIdent _ _
  = []
