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

  codegen (Free e _) = do
    evalE     <- codegen e
    freeInstr <- getFreeExprInstr (typeOfExpr e)
    return $ evalE ++ freeInstr

  codegen (Assignment lhs rhs _) = do
    evalRHS <- codegen rhs
    evalLHS <- codegen lhs
    return $ evalRHS ++ evalLHS

  codegen (Return expr _)
    = codegen expr

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

  codegen (While cond stat _) = do
    loopBodyLabel <- getNextLabel
    loopCondLabel <- getNextLabel
    evalCond      <- codegen cond
    execBody      <- genInNewScope stat
    return $
      [BT loopCondLabel, Def loopBodyLabel]  ++
      execBody                               ++
      [Def loopCondLabel]                    ++
      evalCond                               ++
      [CMP R0 (ImmOp2 1), BEQ loopBodyLabel]

  codegen (For decl cond assign loopBody _) = do
    loopBodyLabel <- getNextLabel
    loopCondLabel <- getNextLabel

    -- The stack info before entering the for loop is what will br reverted to
    -- after the for loop (it has no permanent effect on the env or offset)
    saveStackInfo

    -- Add the declaration to the inner scope
    execDecl <- codegen decl

    -- The condition is evaluated outside of the loopBody's new stack space, but
    -- contains the declaration in its env.
    evalCond <- codegen cond

    -- Get the instrs to manage the new stack space of loopBody's inner scope
    let sizeOfScope = scopeSize loopBody
    (createStackSpace, clearStackSpace) <- manageStack sizeOfScope

    -- Amend the stack info for the new scope before geerating code for the
    -- statements inside the new scope
    (env, _)          <- getStackInfo
    let envWithOffset = Map.map (+ sizeOfScope) env
    putStackInfo (envWithOffset, sizeOfScope)

    -- Generate the body and assign in this new scope
    execBody   <- codegen loopBody
    execAssign <- codegen assign

    -- Leave the scope (revert to previous environment and offset)
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
      [CMP R0 (ImmOp2 1), BEQ loopBodyLabel]

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
