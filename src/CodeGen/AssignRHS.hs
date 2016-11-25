{- This module generates ARM Assembly code for AssignRHS -}

module CodeGen.AssignRHS where

import Control.Monad.StateStack
import Control.Monad.State.Strict (get, put, lift)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Debug.Trace

{- LOCAL IMPORTS -}
import CodeGen.Assembly
import CodeGen.Expression
import CodeGen.PairElem (codegen)
import Utilities.Definitions

instance CodeGen AssignRHS where
  codegen (ExprAssign e _)
    = codegen e
  codegen (ArrayLitAssign exprs _) = do
    let sizeArrayObj = 4 + sum (map exprSize exprs)
    let mallocArray  = malloc sizeArrayObj
    populateArray    <- moveExprsIntoArray exprs 4
    return $
      mallocArray ++
      [Mov R3 (RegOp R0)] ++
      populateArray ++
      [Mov R0 (ImmI (length exprs))] ++
      [STR W NoIdx R0 [RegOp R3]] ++
      [Mov R0 (RegOp R3)]
    where
      moveExprsIntoArray :: [Expr] -> Int -> CodeGenerator [Instr]
      moveExprsIntoArray [] _
        = return []
      moveExprsIntoArray (e : es) offset = do
        evalE         <- codegen e
        let size      = sizeFromType typeSizesSTR (typeOfExpr e)
        let newOffset = offset + exprSize e
        moveRemaining <- moveExprsIntoArray es newOffset
        return $
          evalE ++
          [STR size NoIdx R0 [RegOp R3, ImmI offset]] ++
          moveRemaining
  -- im pretty sure we have to  do a null ptr check?
  codegen (NewPairAssign e e' _) = do
    exprInstrForHeap <- storeInHeap e
    exprInstrForHeap' <- storeInHeap e'
    let callMallocPair = malloc pairSize
    restoreAddresses <- pop [R1, R2]
    let storeAddOfFirst = [STR W NoIdx R2 [RegOp R0]]
    let storeAddOfSecond = [STR W NoIdx R1 [RegOp R0, ImmI pointerSize]]
    return $
      exprInstrForHeap   ++
      exprInstrForHeap'  ++
      callMallocPair     ++
      restoreAddresses   ++
      storeAddOfFirst    ++
      storeAddOfSecond
    where
      storeInHeap :: Expr -> CodeGenerator [Instr]
      storeInHeap e = do
        instr1 <- codegen e
        saveExpr <- push [R0]
        let callMalloc = malloc (exprSize e)
        restoreExpr <- pop [R1]
        let storeExprInMem = [STR (sizeFromType typeSizesSTR $ typeOfExpr e) NoIdx R1 [RegOp R0]]
        saveAddr1 <- push [R0]
        return $
          instr1         ++
          saveExpr       ++
          callMalloc     ++
          restoreExpr    ++
          storeExprInMem ++
          saveAddr1
  -- check for null ptr
  codegen (PairElemAssign pairElem@(PairElem pos e _)_) = do
    getPairElemAddr <- codegen pairElem
    let size        = sizeFromType typeSizesLDR (typeOfPairElem pairElem)
    let getElem     = [LDR size NoIdx R0 [RegOp R0]]
    return $
       getPairElemAddr ++
       getElem
  codegen (FuncCallAssign ident@(Ident name info) es _) = do
    let FuncT retType paramTypes = typeInfo info
    params <- mapM codegen es
    let pushParams = concat $ reverse $ zipWith (\p t -> p ++ pushParam t) params paramTypes
    let callFunc = [BL ("f_" ++ name)]
    let paramSpace  = sum (map typeSize paramTypes)
    let clearParams = [ADD NF SP SP (ImmOp2 paramSpace)]
    return $ pushParams ++ callFunc ++ clearParams
    where
      -- FIXME: DOES NOT CHANGE OFFSETS IN ENVIRONMENT.
      -- Possibly hange STR / LDR Size to be a function that manages env,
      -- like push and pop.
      pushParam :: Type -> [Instr]
      pushParam t
        = [STR size Pre R0 [RegOp SP, ImmI (- typeSize t)]]
        where
          size = sizeFromType typeSizesSTR t

-- Get type of pair elem elements
typeOfPairElem :: PairElem -> Type
typeOfPairElem (PairElem pos e _)
  = case pos of
      Fst -> p1
      Snd -> p2
  where
    PairT p1 p2 = typeOfExpr e

-- Gens instrs to malloc the given amount
malloc :: Int -> [Instr]
malloc n
  = Mov R0 (ImmI n):[BL "malloc"]
