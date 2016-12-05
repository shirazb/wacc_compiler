{- This module generates ARM11 Assembly code for statements -}

module CodeGen.Statement where

import Control.Monad.StateStack
import Control.Monad.State.Strict (lift)
import qualified Data.Map as Map
import Data.Maybe                 (fromJust)

{- LOCAL IMPORTS -}
import CodeGen.Assembly
import CodeGen.Expression
import CodeGen.AssignLHS
import CodeGen.AssignRHS
import CodeGen.InBuiltFunctions
import Utilities.Definitions

-- POST: Generates assembly code for statements
instance CodeGen Stat where
  codegen (Declaration t ident@(Ident name _) rhs _) = do
    instr          <- codegen rhs
    (map', offset) <- getStackInfo
    let newOffset   = offset - typeSize t
    let newMap      = Map.insert name newOffset map'
    let chckLookUp  = fromJust $ Map.lookup name newMap
    putStackInfo (newMap, newOffset)
    let str         = [STR (sizeFromType typeSizesSTR t) NoIdx R0
                      [RegOp SP, ImmI newOffset]]
    return $ instr ++ str

  codegen (Block s _)
    = genInNewScope s

  codegen (Seq s1 s2 _) = do
    instr1 <- codegen s1
    instr2 <- codegen s2
    return $ instr1 ++ instr2

  codegen (Skip _)
    = return []

  codegen (Break _) = do
    (clearStackSpace, _, loopEndLabel) <- getLoopContext
    return $ clearStackSpace ++ [BT loopEndLabel]

  codegen (Continue _) = do
    (clearStackSpace, loopCondLabel, _) <- getLoopContext
    return $ clearStackSpace ++ [BT loopCondLabel]
    
  codegen (Free e _) = do
    evalE     <- codegen e
    freeInstr <- getFreeExprInstr (typeOfExpr e)
    return $ evalE ++ freeInstr

  codegen (Assignment lhs rhs _) = do
    evalRHS <- codegen rhs
    evalLHS <- codegen lhs
    return $ evalRHS ++ evalLHS

  codegen ret@(Return expr _) = do
    returnExpr <- codegen expr
    clearStack <- getFunctionContext
    restorePC <- pop [PC]
    return $ returnExpr ++ clearStack ++ restorePC

  codegen (Exit expr _) = do
    evalExpr <- codegen expr
    return $ evalExpr ++ [BL "exit"]

  codegen (If cond thenStat elseStat _) = do
    condInstr          <- codegen cond
    elseStatLabel      <- getNextLabel
    let branchIfFalse   = [CMP R0 (ImmOp2 0), BEQ elseStatLabel]
    execThenStat       <- genInNewScope thenStat
    execElseStat       <- genInNewScope elseStat
    afterIfLabel       <- getNextLabel
    let branchAfterIf   = [BT afterIfLabel]
    return $
      condInstr           ++
      branchIfFalse       ++
      execThenStat        ++
      branchAfterIf       ++
      [Def elseStatLabel] ++
      execElseStat        ++
      [Def afterIfLabel]

  codegen (While cond body _) = do
    loopBodyLabel <- getNextLabel
    loopCondLabel <- getNextLabel
    loopEndLabel  <- getNextLabel
    evalCond      <- codegen cond
    execBody      <- genInNewWhileScope body loopCondLabel loopEndLabel
    return $
      [BT loopCondLabel, Def loopBodyLabel]  ++
      execBody                               ++
      [Def loopCondLabel]                    ++
      evalCond                               ++
      [CMP R0 (ImmOp2 1), BEQ loopBodyLabel] ++
      [Def loopEndLabel]

  codegen (For decl cond assign body _) = do
    loopBodyLabel     <- getNextLabel
    loopCondLabel <- getNextLabel
    loopEndLabel      <- getNextLabel

    saveStackInfo

    -- Add the declaration to the inner scope
    execDecl <- codegen decl

    -- Condition is evaluated outside of body's new stack space, but
    -- contains the declaration in its env.
    evalCond <- codegen cond

    -- Get the instrs to manage the new stack space of body's inner scope
    let sizeOfScope = scopeSize body
    (createStackSpace, clearStackSpace) <- manageStack sizeOfScope

    -- Modify stack offets
    (env, _)          <- getStackInfo
    let envWithOffset  = Map.map (+ sizeOfScope) env
    putStackInfo (envWithOffset, sizeOfScope)

    --Generate assignment first, needed in loopContext
    execAssign <- codegen assign

    --Include assignment in look clearup context
    previousLoopContext                 <- getLoopContext
    putLoopContext (execAssign ++ clearStackSpace, loopCondLabel, loopEndLabel)
    
    -- Generate both body and assignment in this new scope
    execBody   <- codegen body
    
    putLoopContext previousLoopContext
    restoreStackInfo

    return $
      execDecl                               ++
      [BT loopCondLabel, Def loopBodyLabel]  ++
      createStackSpace                       ++
      execBody                               ++
      execAssign                             ++
      clearStackSpace                        ++
      [Def loopCondLabel]                    ++
      evalCond                               ++
      [CMP R0 (ImmOp2 1), BEQ loopBodyLabel] ++
      [Def loopEndLabel]

  codegen (Print e _) = do
    evalE      <- codegen e
    printInstr <- getExprPrintInstr (typeOfExpr e)
    return $ evalE ++ printInstr

  codegen (Println e p) = do
    printE  <- codegen (Print e p)
    printLn <- branchWithFunc genPrintLn BL
    return $ printE ++ printLn

  codegen (Read lhs _) = do
    calcAddrToReadInto <- readLHS lhs
    readIntoLHS        <- getReadIntoLHS lhs
    return $ calcAddrToReadInto ++ readIntoLHS

  codegen s
    = error $ "CodeGen.Statement.codegen: Attempting to codegen the statement: "
      ++ "\n    " ++ show s

-- POST: Generates assembly to enter a new scope by modifying variable mappings
--       and producing instructions to allocate and deallocate on the stack
prepareScope :: Stat -> CodeGenerator ([Instr] , [Instr])
prepareScope s = do
  saveStackInfo
  let sizeOfScope                      = scopeSize s
  (env, _)                            <- getStackInfo
  let envWithOffset                    = Map.map (+ sizeOfScope) env
  putStackInfo (envWithOffset, sizeOfScope)
  manageStack sizeOfScope

-- POST: Generates assembly code for the given statement within a new scope
genInNewScope :: Stat -> CodeGenerator [Instr]
genInNewScope s = do
  (createStackSpace, clearStackSpace) <- prepareScope s
  instrs <- codegen s
  restoreStackInfo
  return $ createStackSpace ++ instrs ++ clearStackSpace

-- POST: Generates assembly code of a statement within a while-loop scope
genInNewWhileScope :: Stat -> Label -> Label  -> CodeGenerator [Instr]
genInNewWhileScope s condLabel endLabel = do
  (createStackSpace, clearStackSpace) <- prepareScope s

  loopContext <- getLoopContext
  putLoopContext (clearStackSpace, condLabel, endLabel)
  instrs <- codegen s
  putLoopContext loopContext

  restoreStackInfo
  return $ createStackSpace ++ instrs ++ clearStackSpace

-- POST: Generates a branch to the correct subroutine for freeing the given
--       type
getFreeExprInstr :: Type -> CodeGenerator [Instr]
getFreeExprInstr t = case t of

   PairT _ _ -> branchWithFunc genFreePair BL
   _         -> return [BL "free"]

-- POST: Generates correct read function based on type and returns branch with
--       link to that function
getReadIntoLHS :: AssignLHS -> CodeGenerator [Instr]
getReadIntoLHS lhs = case t of
  BaseT BaseChar   -> branchWithFunc genReadChar BL
  BaseT BaseInt    -> branchWithFunc genReadInt  BL
  _                -> error $ "Assertion failed in \
                        \CodeGen.Statement.getReadIntoLHS: Called with lhs of \
                        \incorrect type. \n\
                        \  AssignLHS: " ++ show lhs ++ "\n\
                        \  Type:" ++ show t
  where
    t = typeOfLHS lhs

-- POST: Generates correct print function based on type and returns branch with
--       link to that function
getExprPrintInstr :: Type -> CodeGenerator [Instr]
getExprPrintInstr t = case t of
  BaseT BaseChar            -> return [BL "putchar"]
  BaseT BaseInt             -> branchWithFunc genPrintInt       BL
  BaseT BaseBool            -> branchWithFunc genPrintBool      BL
  BaseT BaseString          -> branchWithFunc genPrintString    BL
  ArrayT 1 (BaseT BaseChar) -> branchWithFunc genPrintString    BL
  _                         -> branchWithFunc genPrintReference BL
