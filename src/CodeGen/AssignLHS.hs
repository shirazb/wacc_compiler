{- This module generates ARM Assembly code for AssignLHS -}

module CodeGen.AssignLHS where

import Control.Monad.StateStack
import Control.Monad.State.Strict (get, put, lift)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Debug.Trace

{- LOCAL IMPORTS -}
import CodeGen.Assembly
import CodeGen.Expression
import CodeGen.PairElem
import Utilities.Definitions hiding (Env)

instance CodeGen AssignLHS where
  codegen (Var ident _)
    = loadIdentAddr R0 ident

  -- what should this be doing???
  codegen (ArrayDeref arrayElem _) = do
    traceM "We are in the evaluation of the lhs for arrays"
    traceM $ "The arrayElem is: " ++ show arrayElem
    codegen arrayElem
  -- Ask Mark? Why do you free then malloc instead of overwriting the
  -- existing address
  codegen (PairDeref pairElem _)
    = codegen pairElem
