{- This module generates ARM11 Assembly code for AssignLHS -}

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
  codegen var@(Var ident _) = do
    offset   <- spToVarOffset ident
    let size  = sizeOfLHS var
    return [STR size NoIdx R0 [RegOp SP, ImmI offset]]

  codegen ad@(ArrayDeref arrayElem _) = do
    putElemAddrIntoR1 <- loadArrayElemAddrR1 arrayElem
    let size  = sizeOfLHS ad
    let store = [STR size NoIdx R0 [RegOp R1]]
    return $ putElemAddrIntoR1 ++ store

  codegen pe@(PairDeref pairElem _) = do
    saveRHS     <- push [R0]
    getElemAddr <- codegen pairElem
    restoreRHS  <- pop [R1]
    let store    = [STR (sizeOfLHS pe) NoIdx R1 [RegOp R0]]
    return $ saveRHS ++ getElemAddr ++ restoreRHS ++ store

-- POST: Reads address of the AssignLHS into R0
readLHS :: AssignLHS -> CodeGenerator [Instr]
readLHS (Var var _) = do
  offset <- spToVarOffset var
  return [ADD NF R0 SP (ImmOp2 offset)]

readLHS (ArrayDeref ae@ArrayElem{} _) = do
  putElemAddrIntoR1   <- loadArrayElemAddrR1 ae
  let setR0ToElemAddr  = [ADD NF R0 R1 (ImmOp2 0)]
  return $ putElemAddrIntoR1 ++ setR0ToElemAddr

readLHS pairDeref@PairDeref{}
  = codegen pairDeref

-- POST: Retrieves the offset to the SP of the location of the variable
spToVarOffset :: Ident -> CodeGenerator Int
spToVarOffset (Ident name info) = do
  (env, _)   <- getStackInfo
  let offset  = fromJust $ Map.lookup name env
  return offset

-- POST: Places the address of the ArrayElem into R1
loadArrayElemAddrR1 :: ArrayElem -> CodeGenerator [Instr]
loadArrayElemAddrR1 arrayElem@(ArrayElem ident idxs _) = do
  saveR0R4      <- push [R0, R4]
  getElemAddr   <- codegen arrayElem
  let movAddrR1  = [Mov R1 (RegOp R4)]
  restoreR0R4   <- pop [R0, R4]
  return $ saveR0R4 ++ getElemAddr ++ movAddrR1 ++ restoreR0R4

-- POST: Returns the Size (word, byte etc.) of an AssignLHS
sizeOfLHS :: AssignLHS -> Size
sizeOfLHS (Var (Ident _ (Info t _)) _)
  = sizeFromType typeSizesSTR t
sizeOfLHS (ArrayDeref ae _)
  = sizeFromType typeSizesSTR (typeOfArrayElem ae)
sizeOfLHS (PairDeref pe _)
  = sizeFromType typeSizesSTR pt
  where
    pt = typeOfPairElem pe

-- POST: Returns the type of the AssignLHS
typeOfLHS :: AssignLHS -> Type
typeOfLHS (Var (Ident _ (Info t _)) _)
  = t
typeOfLHS (ArrayDeref ae _)
  = typeOfArrayElem ae
typeOfLHS (PairDeref pe _)
  = typeOfPairElem pe
