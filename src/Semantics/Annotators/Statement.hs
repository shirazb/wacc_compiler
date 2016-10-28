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
annotateStat (Seq s1 s2)
  = liftM2 Seq (annotateStat s1) (annotateStat s2)

-- Cannot use lift: Must annotate RHS first so new identifier is not in
-- its symbol table, otherwise "int x = x + 3" would be valid, for example.
annotateStat (Declaration t ident rhs) = do
  newRHS   <- annotateRHS rhs
  newIdent <- annotateNewIdent ident
  return $ Declaration t newIdent newRHS

annotateStat (Block s)
  = inChildScopeAndWrap Block (annotateStat s);
  --   do
  -- currST  <- get
  -- put (ST currST Map.empty)
  -- s'      <- annotateStat s
  -- -- returning the scope to what it orignally was
  -- -- before entring the block
  -- put currST
  -- return $ Block s'

annotateExprList :: [Expr] -> LexicalScoper [Expr]
annotateExprList
  = mapM annotateExpr

annotateRHS :: AssignRHS -> LexicalScoper AssignRHS
annotateRHS (ExprAssign expr)
  = ExprAssign <$> annotateExpr expr

annotateRHS (ArrayLitAssign es)
  = ArrayLitAssign <$> annotateExprList es

-- fstExpr should not change state of sndExpr as exprs are side effect free
annotateRHS (NewPairAssign fstExpr sndExpr)
  = liftM2 NewPairAssign (annotateExpr fstExpr) (annotateExpr sndExpr)

annotateRHS (PairElemAssign pairElem)
  = PairElemAssign <$> annotatePairElem pairElem
  where
    annotatePairElem (Fst expr) = Fst <$> annotateExpr expr
    annotatePairElem (Snd expr) = Snd <$> annotateExpr expr

annotateRHS (FuncCallAssign f es)
  = liftM2 FuncCallAssign (annotateIdent f) (annotateExprList es)
