module Optimisations.ConstantEval where
import Utilities.Definitions

optConstantEval :: AST -> AST
optConstantEval (Program fs body)
  = Program fs (optConstEvalStat body)

optConstEvalStat :: Stat -> Stat
optConstEvalStat s@Skip{}
  = s
optConstEvalStat (Declaration t ident rhs pos)
  = Declaration t ident (optConstEvalAssignRHS rhs) pos
optConstEvalStat (Assignment lhs rhs pos)
  = Assignment lhs (optConstEvalAssignRHS rhs) pos
optConstEvalStat s@Read{}
  = s
optConstEvalStat (Free e pos)
  = Free (optConstEvalExpr e) pos
optConstEvalStat (Exit e pos)
  = Exit (optConstEvalExpr e) pos
optConstEvalStat (Print e pos)
  = Print (optConstEvalExpr e) pos
optConstEvalStat (Println e pos)
  = Println (optConstEvalExpr e) pos
optConstEvalStat (If cond s s' pos)
  = If (optConstEvalExpr cond) (optConstEvalStat s) (optConstEvalStat s') pos
optConstEvalStat (While cond body pos)
  = While (optConstEvalExpr cond) (optConstEvalStat body) pos
optConstEvalStat (Block s pos)
  = Block (optConstEvalStat s) pos
optConstEvalStat (Seq s s' pos)
  = Seq (optConstEvalStat s) (optConstEvalStat s') pos


optConstEvalAssignRHS :: AssignRHS -> AssignRHS
optConstEvalAssignRHS (ExprAssign e pos)
  = ExprAssign (optConstEvalExpr e) pos
optConstEvalAssignRHS (ArrayLitAssign es pos)
  = ArrayLitAssign (map optConstEvalExpr es) pos
optConstEvalAssignRHS (NewPairAssign e e' pos)
  = NewPairAssign (optConstEvalExpr e) (optConstEvalExpr e') pos
optConstEvalAssignRHS (PairElemAssign (PairElem s e pos) pos')
  = PairElemAssign (PairElem s (optConstEvalExpr e) pos) pos'
optConstEvalAssignRHS (FuncCallAssign i es pos)
  = FuncCallAssign i (map optConstEvalExpr es) pos

optConstEvalExpr :: Expr -> Expr
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
  = e
