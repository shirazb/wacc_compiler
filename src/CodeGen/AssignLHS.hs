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
  codegen lhs@(Var ident@(Ident name info) _) = do
    (env, _) <- getStackInfo
    let offset = fromJust $ Map.lookup name env
    let size = sizeOfLHS lhs
    return [STR size NoIdx R0 [RegOp SP, ImmI offset]]


  -- what should this be doing???
  codegen (ArrayDeref arrayElem _) = do
    traceM "We are in the evaluation of the lhs for arrays"
    traceM $ "The arrayElem is: " ++ show arrayElem
    codegen arrayElem
  -- Ask Mark? Why do you free then malloc instead of overwriting the
  -- existing address
  codegen (PairDeref pairElem _)
    = codegen pairElem
-- Returns the Size (word, byte etc.) of an AssignLHS
sizeOfLHS :: AssignLHS -> Size
sizeOfLHS (Var (Ident _ (Info t _)) _)
  = sizeFromType typeSizesSTR t
sizeOfLHS (ArrayDeref (ArrayElem (Ident _ (Info t _)) _ _) _)
  = sizeFromType typeSizesSTR t
sizeOfLHS (PairDeref (PairElem _ expr _) _)
  = sizeFromType typeSizesSTR (typeOfExpr expr)
sizeOfLHS _
  = error "are we hitting an error case in sizeoflhs"
