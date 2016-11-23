{- This module defines a number of templates to generate
  inbuilt functions -}
module CodeGen.InBuiltFunctions where


{- Local Imports-}
import CodeGen.Assembly

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

genPrintLn = do
  msgNum <- getNextMsgNum
  let createMsg = MSG msgNum "\0"
  addData createMsg
  saveLR <- push [LR]
  let ldrMsg = [LDR W NoIdx R0 [MsgName msgNum]]
  let callPuts = [ADD NF R0 R0 (ImmOp2 4), BL "puts"]
  let callFflush = [Mov R0 (ImmI 0), BL "fflush"]
  restorePC <- pop [PC]
  let newFunc = FuncA "p_print_ln" (saveLR ++ ldrMsg ++ callPuts ++ callFflush ++ restorePC)
  addFunction newFunc
  return []

genRunTimeError = do
  genPrintString
  let setExitCode = [Mov R0 (ImmI (-1))]
  let runtimeFunc = FuncA "p_throw_runtime_error" ([BL "p_print_string"] ++ setExitCode ++ [BL "exit"])
  addFunction runtimeFunc
  return []

genPrintReference = do
  msgNum <- getNextMsgNum
  let genMsg = MSG msgNum "%p\0"
  addData genMsg
  saveLR <- push [LR]
  let ldrMsg = [Mov R1 (RegOp R0), LDR W NoIdx R0 [MsgName msgNum]]
  let callPrintF = [ADD NF R0 R0 (ImmOp2 4), BL "printf"]
  let callFflush = [Mov R0 (ImmI 0), BL "fflush"]
  restorePC <- pop [PC]
  let printRefFunc = FuncA "p_print_reference" (saveLR ++ ldrMsg ++ callPrintF ++ callFflush ++ restorePC)
  addFunction printRefFunc
  return []

genOverFlowFunction = do
  msgNum <- getNextMsgNum
  let genMsg = MSG msgNum "OverflowError: the result is too small/large to store in a 4-byte signed-integer.\n"
  addData genMsg
  let loadData = [LDR W NoIdx R0 [MsgName msgNum]]
  let branch = [BL "p_throw_runtime_error"]
  let newFunc = FuncA "p_throw_overflow_error" (loadData ++ branch)
  addFunction newFunc
  genRunTimeError
  genPrintString
  return []

genPrintString = do
  saveLR <- push [LR]
  let nextInstr = [LDR W NoIdx R1 [RegOp R0], ADD NF R2 R0 (ImmOp2 4)]
  msgNum <- getNextMsgNum
  let createMsg = MSG msgNum "%.*s\0"
  addData createMsg
  let loadMsg   = [LDR W NoIdx R0 [MsgName msgNum]]
  let nextInstr1 = [ADD NF R0 R0 (ImmOp2 4), BL "printf", Mov R0 (ImmI 0), BL "fflush"]
  restorePC <- pop [PC]
  let newFunc = FuncA "p_print_string" (saveLR ++ nextInstr ++ loadMsg ++ nextInstr1 ++ restorePC)
  addFunction newFunc
  return []
