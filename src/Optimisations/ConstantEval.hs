module Optimisations.ConstantEval where
import Utilities.Definitions
import Control.Monad.Writer


{-

TODO: Add arithmetic ErrorMessages
TODO: Add control flow analysis
-}

type ArithmeticErrors = [String]
optConstEvalAST :: AST -> Writer ArithmeticErrors AST
optConstEvalAST (Program fs body) = do
  body' <- optConstEvalStat body
  return $ Program fs body'


optConstEvalStat :: Stat -> Writer ArithmeticErrors Stat
optConstEvalStat (Declaration t ident rhs pos) = do
  rhs' <- optConstEvalAssignRHS rhs
  return $ Declaration t ident rhs' pos

optConstEvalStat (Assignment lhs rhs pos) = do
  rhs' <- optConstEvalAssignRHS rhs
  return $ Assignment lhs rhs' pos

optConstEvalStat (Free e pos) = do
  e' <- optConstEvalExpr e
  return $ Free e' pos

optConstEvalStat (Exit e pos) = do
  e' <- optConstEvalExpr e
  return $ Exit e' pos

optConstEvalStat (Print e pos) = do
  e' <- optConstEvalExpr e
  return $ Print e' pos

optConstEvalStat (Println e pos) = do
  e' <- optConstEvalExpr e
  return $ Println e' pos

optConstEvalStat (If cond s1 s2 pos) = do
  cond' <- optConstEvalExpr cond
  s1' <- optConstEvalStat s1
  s2' <- optConstEvalStat s2
  return $ If cond' s1' s2' pos

optConstEvalStat (While cond body pos) = do
  cond' <- optConstEvalExpr cond
  body' <- optConstEvalStat body
  return $ While cond' body' pos

optConstEvalStat (Block s pos) = do
  s' <- optConstEvalStat s
  return $ Block s' pos

optConstEvalStat (Seq s1 s2 pos) = do
  s1' <- optConstEvalStat s1
  s2' <- optConstEvalStat s2
  return $ Seq s1' s2' pos

optConstEvalStat s
  = return s

optConstEvalAssignRHS :: AssignRHS -> Writer ArithmeticErrors AssignRHS
optConstEvalAssignRHS (ExprAssign e pos) = do
  e' <- optConstEvalExpr e
  return $ ExprAssign e' pos

optConstEvalAssignRHS (ArrayLitAssign es pos) = do
  es' <- mapM optConstEvalExpr es
  return $ ArrayLitAssign es' pos

optConstEvalAssignRHS (NewPairAssign e1 e2 pos) = do
  e1' <- optConstEvalExpr e1
  e2' <- optConstEvalExpr e2
  return $ NewPairAssign e1' e2' pos

optConstEvalAssignRHS (PairElemAssign (PairElem s e pos) pos') = do
  e' <- optConstEvalExpr e
  return $ PairElemAssign (PairElem s e' pos) pos'

optConstEvalAssignRHS (FuncCallAssign i params pos) = do
  params' <- mapM optConstEvalExpr params
  return $ FuncCallAssign i params' pos

optConstEvalExpr :: Expr -> Writer ArithmeticErrors Expr
optConstEvalExpr (BinaryApp (Arith Add) (IntLit i pos') (IntLit i' pos) posE)
  = IntLit (i + i') pos'

optConstEvalExpr (BinaryApp (Arith Mul) (IntLit i pos') (IntLit i' pos) posE)
  = IntLit (i * i') pos'

optConstEvalExpr (BinaryApp (Arith Div) (IntLit i pos') (IntLit i' pos) posE)
  = IntLit (i `div` i') pos'

optConstEvalExpr (BinaryApp (Arith Mod) (IntLit i pos') (IntLit i' pos) posE)
  = IntLit (i `mod` i') pos'

optConstEvalExpr (BinaryApp (Arith Sub)(IntLit i pos') (IntLit i' pos) posE)
  = IntLit (i - i') pos'

optConstEvalExpr expr@(BinaryApp (Arith Add) (IntLit i pos') e pos)
  | (IntLit i' pos'') <- optConstEvalExpr e = IntLit (i + i') pos'
  | otherwise = expr

optConstEvalExpr expr@(BinaryApp (Arith Add) e (IntLit i pos') pos)
  | (IntLit i' pos'') <- optConstEvalExpr e = IntLit (i + i') pos'
  | otherwise = expr

optConstEvalExpr expr@(BinaryApp (Arith Mul) (IntLit i pos') e pos)
  | (IntLit i' pos'') <- optConstEvalExpr e = IntLit (i * i') pos'
  | otherwise = expr

optConstEvalExpr expr@(BinaryApp (Arith Mul) e (IntLit i pos') pos)
  | (IntLit i' pos'') <- optConstEvalExpr e = IntLit (i * i') pos'
  | otherwise = expr

optConstEvalExpr expr@(BinaryApp (Arith Div) (IntLit i pos') e pos)
  | (IntLit i' pos'') <- optConstEvalExpr e = IntLit (i `div` i') pos'
  | otherwise = expr

optConstEvalExpr expr@(BinaryApp (Arith Div) e (IntLit i pos') pos)
  | (IntLit i' pos'') <- optConstEvalExpr e = IntLit (i `div` i') pos'
  | otherwise = expr

optConstEvalExpr expr@(BinaryApp (Arith Sub) (IntLit i pos') e pos)
  | (IntLit i' pos'') <- optConstEvalExpr e = IntLit (i - i') pos'
  | otherwise = expr

optConstEvalExpr expr@(BinaryApp (Arith Sub) e (IntLit i pos') pos)
  | (IntLit i' pos'') <- optConstEvalExpr e = IntLit (i - i') pos'
  | otherwise = expr

optConstEvalExpr expr@(BinaryApp (Arith Mod) e (IntLit i pos') pos)
  | (IntLit i' pos'') <- optConstEvalExpr e = IntLit (i `mod` i') pos'
  | otherwise = expr

optConstEvalExpr expr@(BinaryApp (Arith Mod) (IntLit i pos') e pos)
  | (IntLit i' pos'') <- optConstEvalExpr e = IntLit (i `mod` i') pos'
  | otherwise = expr


optConstEvalExpr (UnaryApp op e' pos)
  = UnaryApp op (optConstEvalExpr e') pos

optConstEvalExpr (ExprArray (ArrayElem ident es pos) pos')
  = ExprArray (ArrayElem ident (map optConstEvalExpr es) pos) pos'

optConstEvalExpr e
  = return e
