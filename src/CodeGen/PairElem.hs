{- This module generates ARM11 Assembly code for PairElem. It returns the
address of the element -}

module CodeGen.PairElem (
  codegen,
  typeOfPairElem
) where

{- Local imports -}
import CodeGen.Assembly
import CodeGen.Expression
import CodeGen.InBuiltFunctions
import Utilities.Definitions

-- POST: Generates assembly that returns the address of a pairelem in register
--       R0
instance CodeGen PairElem where
  codegen pairElem@(PairElem pos e _) = do
    instr1       <- codegen e
    nullptr      <- branchWithFunc genNullPtrFunc BL
    let posInstr  = case pos of
                      Fst -> [LDR W NoIdx R0 [RegOp R0]]
                      Snd -> [LDR W NoIdx R0 [RegOp R0, ImmI 4]]
    return $ instr1 ++ nullptr ++ posInstr

-- POST: Returns the type of a given element in a pair
typeOfPairElem :: PairElem -> Type
typeOfPairElem (PairElem selector expr _)
  = let PairT t t' = typeOfExpr expr in
      case selector of
        Fst -> t
        Snd -> t'
