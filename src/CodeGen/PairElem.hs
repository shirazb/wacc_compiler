{- CodeGen for PairElem. Returns the address of the element. -}

module CodeGen.PairElem (
  codegen
) where

{- Local imports -}
import CodeGen.Assembly
import CodeGen.Expression
import Utilities.Definitions

instance CodeGen PairElem where
  codegen pairElem@(PairElem pos e _) = do
    instr1       <- codegen e
    let nullptr  = [BL "p_check_null_pointer"]
    let posInstr = case pos of
                       Fst -> [LDR W NoIdx R0 [RegOp R0]]
                       Snd -> [LDR W NoIdx R0 [RegOp R0, ImmI 4]]
    return $
        instr1  ++
        nullptr ++
        posInstr
