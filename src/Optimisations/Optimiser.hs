{-
   Performs the optimisations on the AST. Returns either a list of errors
   or the optimised AST.
-}

module Optimisations.Optimiser (
  optimiser
) where

{- LOCAL IMPORTS -}
import Optimisations.ConstantEval
import Optimisations.ControlFlowAnalysis
import Utilities.Definitions

-- POST: Performs the optimisations 
optimiser :: AST -> Either ArithmeticErrors AST
optimiser ast
  = case optConstEval ast of
      Left errs  -> Left errs
      Right ast' -> Right (cfa ast')
