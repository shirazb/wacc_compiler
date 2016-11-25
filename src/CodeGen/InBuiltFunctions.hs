{- This module defines a number of templates to generate in-built functions -}

module CodeGen.InBuiltFunctions where

{- Local Imports-}
import CodeGen.Assembly

genNullPtrFunc :: CodeGenerator String
genNullPtrFunc = do
  msgNum              <- genMsg ("NullReferenceError: dereference" ++
                                " a null reference\n\0")
  saveLR              <- push [LR]
  let checkForNullPtr  = [CMP R0 (ImmOp2 0)]
  let ldrMsg           = [LDREQ W NoIdx R0 [MsgName msgNum],
                          BLEQ "p_throw_runtime_error"]
  restorePC           <- pop [PC]
  genFunc "p_check_null_pointer"
          (saveLR ++ checkForNullPtr ++ ldrMsg ++ restorePC)
  genPrintString
  genRunTimeError
  return "p_check_null_pointer"

genRunTimeError :: CodeGenerator String
genRunTimeError = do
  genPrintString
  let setExitCode = [Mov R0 (ImmI (-1))]
  genFunc "p_throw_runtime_error"
          ([BL "p_print_string"] ++ setExitCode ++ [BL "exit"])
  return "p_throw_runtime_error"

genOverFlowFunction :: CodeGenerator String
genOverFlowFunction = do
  msgNum       <- genMsg ("OverflowError: the result is too small/large" ++
                          " to store in a 4-byte signed-integer.\n")
  let loadData  = [LDR W NoIdx R0 [MsgName msgNum]]
  let branch    = [BL "p_throw_runtime_error"]
  genFunc "p_throw_overflow_error" (loadData ++ branch)
  genRunTimeError
  genPrintString
  return "p_throw_overflow_error"

genPrintLn :: CodeGenerator String
genPrintLn    = do
  msgNum     <- genMsg "\0"
  saveLR     <- push [LR]
  let ldrMsg  = [LDR W NoIdx R0 [MsgName msgNum]]
  restorePC  <- pop [PC]
  genFunc "p_print_ln" (saveLR ++ ldrMsg ++ genPuts ++ genFflush ++ restorePC)
  return "p_print_ln"

genPrintReference :: CodeGenerator String
genPrintReference = do
  msgNum     <- genMsg "%p\0"
  saveLR     <- push [LR]
  let ldrMsg  = [Mov R1 (RegOp R0), LDR W NoIdx R0 [MsgName msgNum]]
  restorePC  <- pop [PC]
  genFunc "p_print_reference"
          (saveLR ++ ldrMsg ++ genPrintF ++ genFflush ++ restorePC)
  return "p_print_reference"

genPrintInt :: CodeGenerator String
genPrintInt   = do
  msgNum     <- genMsg "%d\0"
  saveLR     <- push [LR]
  let ldrMsg  = [Mov R1 (RegOp R0), LDR W NoIdx R0 [MsgName msgNum]]
  restoreLR  <- pop [PC]
  genFunc "p_print_int"
          (saveLR ++ ldrMsg ++ genPrintF ++ genFflush ++ restoreLR)
  return "p_print_int"

genPrintBool :: CodeGenerator String
genPrintBool = do
  msgNum        <- genMsg "true\0"
  msgNum'       <- genMsg "false\0"
  saveLR        <- push [LR]
  let chooseMsg  = [CMP R0 (ImmOp2 0), LDRNE W NoIdx R0 [MsgName msgNum],
                    LDREQ W NoIdx R0 [MsgName msgNum']]
  restoreLR     <- pop [PC]
  genFunc "p_print_bool"
          (saveLR ++ chooseMsg ++ genPrintF ++ genFflush ++ restoreLR)
  return "p_print_bool"

genPrintString :: CodeGenerator String
genPrintString = do
  saveLR        <- push [LR]
  let nextInstr  = [LDR W NoIdx R1 [RegOp R0], ADD NF R2 R0 (ImmOp2 4)]
  msgNum        <- genMsg "%.*s\0"
  let loadMsg    = [LDR W NoIdx R0 [MsgName msgNum]]
  restorePC     <- pop [PC]
  genFunc "p_print_string" (saveLR ++ nextInstr ++ loadMsg ++ genPrintF ++
                            genFflush ++ restorePC)
  return "p_print_string"

genCheckArrayBounds :: CodeGenerator String
genCheckArrayBounds = do
  saveLR          <- push [LR]
  negMsgNum       <- genMsg "ArrayIndexOutOfBoundsError: negative index\n\0"
  let checkTooLow  = [CMP R0 (ImmOp2 0),
                      LDRLT W NoIdx R0 [MsgName negMsgNum],
                      BLLT "p_throw_runtime_error"]
  largeMsgNum <- genMsg "ArrayIndexOutOfBoundsError: index too large\n\0"
  let checkTooHigh = [LDR W NoIdx R1 [RegOp R4], CMP R0 (RegOp2 R1),
                      LDRCS W NoIdx R0 [MsgName largeMsgNum],
                      BLCS "p_throw_runtime_error"]
  ret             <- pop [PC]
  genRunTimeError
  genFunc "p_check_array_bounds" $
    saveLR ++
    checkTooLow ++
    checkTooHigh ++
    ret
  return "p_check_array_bounds"

genReadInt :: CodeGenerator String
genReadInt = do
  saveLR <- push [LR]-- Preferred this instead - Shiraz
  msgNum <- genMsg "%d\0"
  ret    <- pop [PC]
  genFunc "p_read_int" $
    saveLR                            ++
    [Mov R1 (RegOp R0)]               ++
    [LDR W NoIdx R0 [MsgName msgNum]] ++
    [ADD NF R0 R0 (ImmOp2 4)]         ++
    [BL "scanf"]                      ++
    ret
  return "p_read_int"

genReadChar :: CodeGenerator String
genReadChar = do
  saveLR <- push [LR]
  msgNum <- genMsg " %c\0"
  ret    <- pop [PC]
  genFunc "p_read_char" $
    saveLR                            ++
    [Mov R1 (RegOp R0)]               ++
    [LDR W NoIdx R0 [MsgName msgNum]] ++
    [ADD NF R0 R0 (ImmOp2 4)]         ++
    [BL "scanf"]                      ++
    ret
  return "p_read_char"

genFreePair :: CodeGenerator String
genFreePair         = do
  saveLR           <- push [LR]
  msgNum           <- genMsg "msg45"
  let checkNotNull  = [CMP R0 (ImmOp2 0),
                       LDREQ W NoIdx R0 [MsgName msgNum],
                       BEQ "p_throw_runtime_error"]
  savePairAddr     <- push [R0]
  let freeFst       = [LDR W NoIdx R0 [RegOp R0], BL "free"]
  let freeSnd       = [LDR W NoIdx R0 [RegOp SP],
                       LDR W NoIdx R0 [RegOp R0, ImmI 4],
                       BL "free"]
  getPairAddr      <- pop [R0]
  let freePair      = [BL "free"]
  ret              <- pop [PC]
  genRunTimeError
  genFunc "p_free_pair" $
    saveLR       ++
    checkNotNull ++
    savePairAddr ++
    freeFst      ++
    freeSnd      ++
    getPairAddr  ++
    freePair     ++
    ret
  return "p_free_pair"

genCheckDivideByZero :: CodeGenerator String
genCheckDivideByZero = do
  saveLR      <- push [LR]
  msgNum      <- genMsg "DivideByZeroError: divide or modulo by zero\n\0"
  let compare  = [CMP R1 (ImmOp2 0),
                  LDREQ W NoIdx R0 [MsgName msgNum],
                  BLEQ "p_throw_runtime_error"]
  ret         <- pop [PC]
  genRunTimeError
  genFunc "p_check_divide_by_zero" $
    saveLR  ++
    compare ++
    ret
  return "p_check_divide_by_zero"

{- Utility Functions -}

genMsg :: String -> CodeGenerator Int
genMsg = addUniqueData

genFunc :: String -> [Instr] -> CodeGenerator ()
genFunc name body = do
  let newFunc = FuncA name body
  addFunction newFunc

branchWithFunc :: CodeGenerator String -> (String -> Instr) ->
                  CodeGenerator [Instr]
branchWithFunc func branch = do
  name <- func
  return [branch name]

genPuts, genFflush, genPrintF :: [Instr]
genPuts
  = [ADD NF R0 R0 (ImmOp2 4), BL "puts"]

genFflush
  = [Mov R0 (ImmI 0), BL "fflush"]

genPrintF
  = [ADD NF R0 R0 (ImmOp2 4), BL "printf"]
