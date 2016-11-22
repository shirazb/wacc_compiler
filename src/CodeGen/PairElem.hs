{- CodeGen for PairElem. Returns the address of the element. -}

module CodeGen.PairElem (
  codegen
) where

{- Local imports -}
import CodeGen.Assembly
import CodeGen.Expression
import Utilities.Definitions


genNullPtrFunc = do
  msgNum <- getNextMsgNum
  saveLR <- push [LR]
  let checkForNullPtr = [CMP R0 (ImmOp2 0)]
  let createMsg = MSG msgNum "NullReferenceError: dereference a null reference\n\0"
  addData createMsg
  let ldrMsg = [LDREQ W NoIdx R0 [MsgName msgNum], BLEQ "p_throw_runtime_error"]
  restorePC <- pop [PC]
  let newFunc = FuncA "p_check_null_pointer" (saveLR ++ checkForNullPtr ++ ldrMsg ++ restorePC)
  addFunction newFunc
  genPrintString
  return []

instance CodeGen PairElem where
  codegen pairElem@(PairElem pos e _) = do
    instr1       <- codegen e
    genNullPtrFunc
    let nullptr  = [BL "p_check_null_pointer"]
    let posInstr = case pos of
                       Fst -> [LDR W NoIdx R0 [RegOp R0]]
                       Snd -> [LDR W NoIdx R0 [RegOp R0, ImmI 4]]
    return $
        instr1  ++
        nullptr ++
        posInstr
