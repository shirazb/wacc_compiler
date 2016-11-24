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
  codegen ad@(ArrayDeref arrayElem@(ArrayElem ident idxs _) _) = do
    saveR0R4 <- push [R0, R4]
    getElemAddr <- codegen arrayElem
    let movAddrR1 = [Mov R1 (RegOp R4)]
    restoreR0R4 <- pop [R0, R4]
    let size = sizeOfLHS ad
    let store = [STR size NoIdx R0 [RegOp R1]]
    return $ saveR0R4 ++
             getElemAddr ++
             movAddrR1 ++
             restoreR0R4 ++
             store

  codegen (PairDeref pairElem _)
    = codegen pairElem

-- Returns the Size (word, byte etc.) of an AssignLHS
sizeOfLHS :: AssignLHS -> Size
sizeOfLHS (Var (Ident _ (Info t _)) _)
  = sizeFromType typeSizesSTR t
sizeOfLHS (ArrayDeref ae@ArrayElem{} _)
  = sizeFromType typeSizesSTR (typeOfArrayElem ae)
sizeOfLHS (PairDeref (PairElem selector expr _) _)
  = sizeFromType typeSizesSTR pt
  where
    PairT t t' = typeOfExpr expr
    pt         = case selector of
                    Fst -> t
                    Snd -> t'
