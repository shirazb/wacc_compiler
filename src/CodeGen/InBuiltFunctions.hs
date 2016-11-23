{- This module defines a number of templates to generate
  inbuilt functions -}
module CodeGen.InBuiltFunctions where


{- Local Imports-}
import CodeGen.Assembly

genNullPtrFunc :: CodeGenerator ()
genNullPtrFunc = do
  DataSeg _ msgNum <- getData
  genMsg "NullReferenceError: dereference a null reference\n\0"
  saveLR <- push [LR]
  let checkForNullPtr = [CMP R0 (ImmOp2 0)]
  let ldrMsg = [LDREQ W NoIdx R0 [MsgName msgNum], BLEQ "p_throw_runtime_error"]
  restorePC <- pop [PC]
  genFunc "p_check_null_pointer" (saveLR ++ checkForNullPtr ++ ldrMsg ++ restorePC)
  genPrintString

genPrintLn :: CodeGenerator ()
genPrintLn = do
  DataSeg _ msgNum <- getData
  genMsg "\0"
  saveLR <- push [LR]
  let ldrMsg = [LDR W NoIdx R0 [MsgName msgNum]]
  restorePC <- pop [PC]
  genFunc "p_print_ln" (saveLR ++ ldrMsg ++ genPuts ++ genFflush ++ restorePC)

genRunTimeError :: CodeGenerator ()
genRunTimeError = do
  genPrintString
  let setExitCode = [Mov R0 (ImmI (-1))]
  genFunc "p_throw_runtime_error" ([BL "p_print_string"] ++ setExitCode ++ [BL "exit"])

genPrintReference :: CodeGenerator ()
genPrintReference = do
  DataSeg _ msgNum <- getData
  genMsg "%p\0"
  saveLR <- push [LR]
  let ldrMsg = [Mov R1 (RegOp R0), LDR W NoIdx R0 [MsgName msgNum]]
  restorePC <- pop [PC]
  genFunc "p_print_reference" (saveLR ++ ldrMsg ++ genPrintF ++ genFflush ++ restorePC)

genPrintInt :: CodeGenerator ()
genPrintInt = do
  DataSeg _ msgNum <- getData
  genMsg "%d\0"
  saveLR <- push [LR]
  let ldrMsg = [Mov R1 (RegOp R0), LDR W NoIdx R0 [MsgName msgNum]]
  restoreLR <- pop [PC]
  genFunc "p_print_int" (saveLR ++ ldrMsg ++ genPrintF ++ genFflush ++ restoreLR)

genPrintBool :: CodeGenerator ()
genPrintBool = do
  DataSeg _ msgNum <- getData
  genMsg "true\0"
  DataSeg _ msgNum' <- getData
  genMsg "false\0"
  saveLR <- push [LR]
  let chooseMsg = [CMP R0 (ImmOp2 0), LDRNE W NoIdx R0 [MsgName msgNum], LDREQ W NoIdx R0 [MsgName msgNum']]
  restoreLR <- pop [PC]
  genFunc "p_print_bool" (saveLR ++ chooseMsg ++ genPrintF ++ genFflush ++ restoreLR)

genOverFlowFunction :: CodeGenerator ()
genOverFlowFunction = do
  DataSeg _ msgNum <- getData
  genMsg "OverflowError: the result is too small/large to store in a 4-byte signed-integer.\n"
  let loadData = [LDR W NoIdx R0 [MsgName msgNum]]
  let branch = [BL "p_throw_runtime_error"]
  genFunc "p_throw_overflow_error" (loadData ++ branch)
  genRunTimeError
  genPrintString

genPrintString :: CodeGenerator ()
genPrintString = do
  saveLR <- push [LR]
  let nextInstr = [LDR W NoIdx R1 [RegOp R0], ADD NF R2 R0 (ImmOp2 4)]
  DataSeg _ msgNum <- getData
  genMsg "%.*s\0"
  let loadMsg   = [LDR W NoIdx R0 [MsgName msgNum]]
  restorePC <- pop [PC]
  genFunc "p_print_string" (saveLR ++ nextInstr ++ loadMsg ++ genPrintF ++ genFflush ++ restorePC)

genCheckArrayBounds :: CodeGenerator ()
genCheckArrayBounds = do
  saveLR    <- push [LR]
  negMsgNum <- genMsg' "ArrayIndexOutOfBoundsError: negative index\n\0"
  let checkTooLow = [
        CMP R0 (ImmOp2 0),
        LDRLT W NoIdx R0 [MsgName negMsgNum],
        BLLT "p_throw_runtime_error"]
  largeMsgNum <- genMsg' "ArrayIndexOutOfBoundsError: index too large\n\0"
  let checkTooHigh = [
        LDR W NoIdx R1 [RegOp R4], CMP R0 (RegOp2 R1),
        LDRCS W NoIdx R0 [MsgName largeMsgNum],
        BLCS "p_throw_runtime_error"]
  ret <- pop [PC]
  genFunc "p_check_array_bounds" $
      saveLR ++
      checkTooLow ++
      checkTooHigh ++
      ret

genReadInt :: CodeGenerator ()
genReadInt = do
  saveLR <- push [LR]
  msgNum <- genMsg' "%d\0"
  ret    <- pop [PC]
  genFunc "p_read_int" $
      saveLR ++
      [Mov R1 (RegOp R0)] ++
      [LDR W NoIdx R0 [MsgName msgNum]] ++
      [ADD NF R0 R0 (ImmOp2 4)] ++
      [BL "scanf"] ++
      ret

genReadChar :: CodeGenerator ()
genReadChar = do
  saveLR <- push [LR]
  msgNum <- genMsg' " %c\0"
  ret    <- pop [PC]
  genFunc "p_read_char" $
      saveLR ++
      [Mov R1 (RegOp R0)] ++
      [LDR W NoIdx R0 [MsgName msgNum]] ++
      [ADD NF R0 R0 (ImmOp2 4)] ++
      [BL "scanf"] ++
      ret

genFreePair :: CodeGenerator ()
genFreePair = do
  saveLR <- push [LR]
  msgNum <- genMsg' "msg45"
  let checkNotNull = [
        CMP R0 (ImmOp2 0),
        LDREQ W NoIdx R0 [MsgName msgNum],
        BEQ "p_throw_runtime_error"]
  savePairAddr <- push [R0]
  let freeFst = [
        LDR W NoIdx R0 [RegOp R0],
        BL "free"]
  let freeSnd = [
        LDR W NoIdx R0 [RegOp SP],
        LDR W NoIdx R0 [RegOp R0, ImmI 4],
        BL "free"]
  getPairAddr <- pop [R0]
  let freePair = [BL "free"]
  ret    <- pop [PC]
  genFunc "p_free_pair" $
    saveLR ++
    checkNotNull ++
    savePairAddr ++
    freeFst ++
    freeSnd ++
    getPairAddr ++
    freePair ++
    ret


{- Utility Functions -}

-- Preferred this instead - Shiraz
genMsg' :: String -> CodeGenerator Int
genMsg' msg = do
  msgNum <- getNextMsgNum
  addData (MSG msgNum msg)
  return msgNum

-- POST: Adds the given message to the data segment with a unique label.
genMsg :: String -> CodeGenerator ()
genMsg msg = do
  msgNum <- getNextMsgNum
  let genMsg = MSG msgNum msg
  addData genMsg

genFunc :: String -> [Instr] -> CodeGenerator ()
genFunc name body = do
  let newFunc = FuncA name body
  addFunction newFunc

genPuts, genFflush, genPrintF :: [Instr]
genPuts
  = [ADD NF R0 R0 (ImmOp2 4), BL "puts"]

genFflush
  = [Mov R0 (ImmI 0), BL "fflush"]

genPrintF
  = [ADD NF R0 R0 (ImmOp2 4), BL "printf"]
