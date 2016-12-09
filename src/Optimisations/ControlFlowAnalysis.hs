{-
  This module defines one of the phases in our optimisations,
  it is done after constant evalaution. It removes redundant control flow.
-}
{-# LANGUAGE TypeSynonymInstances #-}

module Optimisations.ControlFlowAnalysis (cfa) where

{- LOCAL IMPORTS -}
import Utilities.Definitions

-- POST: Removes reduandant control flow in a program.
class ControlFlowAnalyser a where
  cfa :: a -> a

instance ControlFlowAnalyser AST where
  cfa (Program c fs body)
    = Program c (map cfa fs) (cfa body)

instance ControlFlowAnalyser Func where
  cfa (Func t ident pl body pos)
    = Func t ident pl (cfa body) pos

instance ControlFlowAnalyser Stat where
  cfa (If (BoolLit cond _) s1 s2 pos)
    = if cond then s1 else s2

  cfa while@(While (BoolLit cond _) s1 pos)
    = if cond then while else Skip pos

  cfa s
    = s
