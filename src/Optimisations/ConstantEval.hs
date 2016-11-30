{-
  This module defines one of the phases in our Optimisations, it takes an AST
  and evaluates any constant expressions found within the AST. Detects an
  reports all overflow or divide by zero errors found as a result of evaluating
  the expressions.
-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Optimisations.ConstantEval (optConstEval) where

import Prelude hiding (LT, GT, EQ)
import Control.Monad.Writer.Strict (Writer (..), tell, when, runWriter, writer)
import Data.Maybe (fromJust)

{- LOCAL IMPORTS -}
import Semantics.ErrorMessages (overFlowError, divideByZero)
import Utilities.Definitions

-- POST: Returns arithmetic errors or an optimised AST with constants evaluated
optConstEval :: AST -> Either ArithmeticErrors AST
optConstEval ast
  = if null errors then Right ast' else Left errors
  where
  (ast', errors)  = runWriter (constEval ast)

--POST: Evaluates any constant expressions found in the AST
class ConstEval a where
  constEval :: a -> ConstantEvaluator a

instance ConstEval AST where
  constEval (Program fs body) = do
    fs'   <- mapM constEval fs
    body' <- constEval body
    return $ Program fs' body'

instance ConstEval Func where
  constEval (Func t i pl body pos)
    = (\b -> Func t i pl b pos) <$> constEval body

instance ConstEval Stat where
  constEval (Declaration t ident rhs pos) = do
    rhs' <- constEval rhs
    return $ Declaration t ident rhs' pos

  constEval (Assignment lhs rhs pos) = do
    rhs' <- constEval rhs
    return $ Assignment lhs rhs' pos

  constEval (Free e pos) = do
    e' <- constEval e
    return $ Free e' pos

  constEval (Exit e pos) = do
    e' <- constEval e
    return $ Exit e' pos

  constEval (Print e pos) = do
    e' <- constEval e
    return $ Print e' pos

  constEval (Println e pos) = do
    e' <- constEval e
    return $ Println e' pos

  constEval (If cond s1 s2 pos) = do
    cond' <- constEval cond
    s1'   <- constEval s1
    s2'   <- constEval s2
    return $ If cond' s1' s2' pos

  constEval (While cond body pos) = do
    cond' <- constEval cond
    body' <- constEval body
    return $ While cond' body' pos

  constEval (Block s pos) = do
    s' <- constEval s
    return $ Block s' pos

  constEval (Seq s1 s2 pos) = do
    s1' <- constEval s1
    s2' <- constEval s2
    return $ Seq s1' s2' pos

  constEval s
    = return s

instance ConstEval AssignRHS where
  constEval (ExprAssign e pos) = do
    e' <- constEval e
    return $ ExprAssign e' pos

  constEval (ArrayLitAssign es pos) = do
    es' <- mapM constEval es
    return $ ArrayLitAssign es' pos

  constEval (NewPairAssign e1 e2 pos) = do
    e1' <- constEval e1
    e2' <- constEval e2
    return $ NewPairAssign e1' e2' pos

  constEval (PairElemAssign (PairElem s e pos) pos') = do
    e' <- constEval e
    return $ PairElemAssign (PairElem s e' pos) pos'

  constEval (FuncCallAssign i params pos) = do
    params' <- mapM constEval params
    return $ FuncCallAssign i params' pos

-- Constant evaluates the expression, then rearranges and rebrackets the
-- expression for more constant evaluation
instance ConstEval Expr where
  constEval e
    = constEvalExpr e >>=! rearrange >>=! rebracket >>=! constEvalExpr
    where
      -- Short-circuit the fmap if there were errors
      (<$!>) :: (a -> a) -> ConstantEvaluator a -> ConstantEvaluator a
      f <$!> x = case runWriter x of
        (x', [])  -> writer (f x', [])
        (_, errs) -> x

      -- Short-circuit the bind if there were errors
      (>>=!) :: ConstantEvaluator a -> (a -> ConstantEvaluator a) -> ConstantEvaluator a
      x >>=! f = case runWriter x of
        (x', [])  -> f x'
        (_, errs)   -> x

-- POST: Evaluates constant expressions
constEvalExpr :: Expr -> ConstantEvaluator Expr
constEvalExpr expr@(BinaryApp (Arith Add) (IntLit i pos) (IntLit i' pos') posE)
  = evaluate (i + i') pos expr

constEvalExpr expr@(BinaryApp (Arith Mul) (IntLit i pos) (IntLit i' pos') posE)
  = evaluate (i * i') pos expr

constEvalExpr expr@(BinaryApp (Arith Sub) (IntLit i pos) (IntLit i' pos') posE)
  = evaluate (i - i') pos expr

constEvalExpr expr@(BinaryApp (Arith Div) (IntLit i pos) (IntLit i' pos') posE)
  | i' == 0   = tell [divideByZero posE expr] >> return expr
  | otherwise = evaluate (i `div` i') pos expr


constEvalExpr expr@(BinaryApp (Arith Mod) (IntLit i pos) (IntLit i' pos') posE)
  | i' == 0   = tell [divideByZero posE expr] >> return expr
  | otherwise = evaluate (i `mod` i') pos expr


constEvalExpr expr@(BinaryApp (Arith Add) e1 e2 pos) = do
  e1' <- constEvalExpr e1
  e2' <- constEvalExpr e2
  if | IntLit i _ <- e1', IntLit i' _ <- e2'
       -> evaluate (i + i') pos expr
     | otherwise
       -> return (BinaryApp (Arith Add) e1' e2' pos)

constEvalExpr expr@(BinaryApp (Arith Sub) e1 e2 pos) = do
 e1' <- constEvalExpr e1
 e2' <- constEvalExpr e2
 if | IntLit i _ <- e1', IntLit i' _ <- e2'
      -> evaluate (i - i') pos expr
    | otherwise
      -> return (BinaryApp (Arith Sub) e1' e2' pos)

constEvalExpr expr@(BinaryApp (Arith Div) e1 e2 pos) = do
 e1' <- constEvalExpr e1
 e2' <- constEvalExpr e2
 if | IntLit 0 _ <- e2'
      -> tell [divideByZero pos expr] >> return expr
    | IntLit i _ <- e1', IntLit i' _ <- e2'
      -> evaluate (i `div` i') pos expr
    | otherwise
      -> return (BinaryApp (Arith Div) e1' e2' pos)

constEvalExpr expr@(BinaryApp (Arith Mod) e1 e2 pos) = do
 e1' <- constEvalExpr e1
 e2' <- constEvalExpr e2
 if | IntLit 0 _ <- e2'
      -> tell [divideByZero pos expr] >> return expr
    | IntLit i _ <- e1', IntLit i' _ <- e2'
      -> evaluate (i `mod` i') pos expr
    | otherwise
      -> return (BinaryApp (Arith Mod) e1' e2' pos)

constEvalExpr expr@(BinaryApp (Arith Mul) e1 e2 pos) = do
 e1' <- constEvalExpr e1
 e2' <- constEvalExpr e2
 if | IntLit i _ <- e1', IntLit i' _ <- e2'
      -> evaluate (i * i') pos expr
    | otherwise
      -> return (BinaryApp (Arith Mul) e1' e2' pos)

constEvalExpr (BinaryApp (Logic (lookUpBoolOp -> op)) (BoolLit b pos) (BoolLit b' pos') pos'')
  = return (BoolLit (b `op` b') pos)

constEvalExpr (BinaryApp (Logic op) e1 e2 pos) = do
  e1' <- constEvalExpr e1
  e2' <- constEvalExpr e2
  let opF = lookUpBoolOp op
  if | BoolLit b _ <- e1', BoolLit b' _ <- e2'
        -> return $ BoolLit (b `opF` b') pos
     | otherwise
        -> return $ BinaryApp (Logic op) e1' e2' pos

constEvalExpr (BinaryApp (RelOp (lookUpRelOp -> op)) (CharLit c pos) (CharLit c' pos') pos'')
  = return $ BoolLit (c `op` c') pos''
constEvalExpr (BinaryApp (RelOp (lookUpRelOp -> op)) (IntLit i pos) (IntLit i' pos') pos'')
  = return $ BoolLit (i `op` i') pos''

constEvalExpr (BinaryApp (RelOp op) e1 e2 pos) = do
  e1' <- constEvalExpr e1
  e2' <- constEvalExpr e2
  let opF :: (Ord a => a -> a -> Bool); opF = lookUpRelOp op
  if | CharLit c _ <- e1', CharLit c' _ <- e2'
        -> return $ BoolLit (c `opF` c') pos
     | IntLit i _ <- e1', IntLit i' _ <- e2'
        -> return $ BoolLit ( i `opF` i') pos
     | otherwise
        -> return $ BinaryApp (RelOp LT) e1' e2' pos

constEvalExpr (BinaryApp (EquOp (lookUpEqOp -> op)) (CharLit c pos) (CharLit c' pos') pos'')
  = return $ BoolLit (c `op` c') pos''

constEvalExpr (BinaryApp (EquOp (lookUpEqOp -> op)) (IntLit i pos) (IntLit i' pos') pos'')
  = return $ BoolLit (i `op` i') pos''

constEvalExpr (BinaryApp (EquOp (lookUpEqOp -> op)) (BoolLit i pos) (BoolLit i' pos') pos'')
  = return $ BoolLit (i `op` i') pos'

constEvalExpr (BinaryApp (EquOp op) e1 e2 pos) = do
  e1' <- constEvalExpr e1
  e2' <- constEvalExpr e2
  let opF :: (Eq a => a -> a -> Bool); opF = lookUpEqOp op
  if | CharLit c _ <- e1', CharLit c' _ <- e2'
        -> return $ BoolLit (c `opF` c') pos
     | IntLit i _ <- e1', IntLit i' _ <- e2'
        -> return $ BoolLit ( i `opF` i') pos
     | BoolLit b _ <- e1', BoolLit b' _ <- e2'
        -> return $ BoolLit (b `opF` b') pos
     | otherwise
        -> return $ BinaryApp (EquOp op) e1' e2' pos

constEvalExpr (UnaryApp op e pos) = do
  e' <- constEvalExpr e
  return $ UnaryApp op e' pos

constEvalExpr (ExprArray (ArrayElem ident es pos) pos') = do
  es' <- mapM constEvalExpr es
  return $ ExprArray (ArrayElem ident es' pos) pos'

constEvalExpr e
  = return e

-- POST: notIntLit <-> parameter is not an integer literal
notAnIntLit :: Expr -> Bool
notAnIntLit IntLit{}
  = False
notAnIntLit _
  = True

-- PRE: Expression has been constant evaluation
-- POST: Rebrackets the expression for more constant evaluation
--       e.g.: (x + 2) + 3 ---> x + (2 + 3)
--             2 * (3 * x) ---> (2 * 3) * x
rebracket :: Expr -> ConstantEvaluator Expr
rebracket expr@(BinaryApp (Arith Mul) (BinaryApp (Arith Mul) e lit@(IntLit i pos') _) lit'@(IntLit i' _) pos) = do
  e' <- constEval e
  if notAnIntLit e'
    then return $ BinaryApp (Arith Mul) e (IntLit (i * i') pos') pos
    else return expr
-- e.g.: 2 + (3 + x) --> (2 + 3) + x
rebracket expr@(BinaryApp (Arith Mul) lit'@(IntLit i' pos') (BinaryApp (Arith Mul) lit@(IntLit i _) e _) pos) = do
  e' <- constEval e
  if notAnIntLit e'
    then return $ BinaryApp (Arith Mul) (IntLit (i * i') pos') e' pos
    else return expr
rebracket expr@(BinaryApp (Arith Add) (BinaryApp (Arith Add) e lit@(IntLit i pos') _) lit'@(IntLit i' _) pos)  = do
  e' <- constEval e
  if notAnIntLit e'
    then return $ BinaryApp (Arith Add) e' (IntLit (i + i') pos') pos
    else return expr
rebracket expr@(BinaryApp (Arith Add) lit'@(IntLit i' pos') (BinaryApp (Arith Add) lit@(IntLit i _) e _) pos) = do
  e' <- constEval e
  if notAnIntLit e'
    then return $ BinaryApp (Arith Add) (IntLit (i + i') pos') e' pos
    else return expr
rebracket e
  = return e

rearrange :: Expr -> ConstantEvaluator Expr
rearrange = return

-- POST: Checks if the given integer overflows, reports error if it does
evaluate :: Int -> Position -> Expr -> Writer ArithmeticErrors Expr
evaluate i pos expr = do
  when overFlowCheck (tell [overFlowError pos expr])
  return $ IntLit i pos
  where
  overFlowCheck  = i > 2147483647 || i < (-2147483647)

-- POST: Returns the corresponding haskell operator of a EqOps
lookUpEqOp :: (Eq a) => EqOps -> (a -> a -> Bool)
lookUpEqOp eqOp
  = fromJust $ lookup eqOp eqOps
  where
    eqOps = [(EQ, (==)), (NEQ, (/=))]

-- POST: Returns the corresponding haskell operator of a RelOp
lookUpRelOp :: (Ord a) => RelationalOp -> (a -> a -> Bool)
lookUpRelOp rOp
  = fromJust $ lookup rOp rOps
  where
    rOps = [(LT, (<)), (LTE, (<=)), (GTE, (>=)), (GT, (>))]

-- POST: Returns the corresponding haskell operator of a LogicalOp
lookUpBoolOp :: LogicalOp -> (Bool -> Bool -> Bool)
lookUpBoolOp bOp
  = fromJust (lookup bOp bOps)
  where
    bOps = [(AND, (&&)), (OR, (||))]
