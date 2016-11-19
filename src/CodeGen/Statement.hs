{- This module generates ARM Assembly code for statements -}

module CodeGen.Statement where

import Control.Monad.StateStack
import Control.Monad.State.Strict (get, put, lift)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

{- LOCAL IMPORTS -}
import CodeGen.Assembly
import CodeGen.Expression
import CodeGen.AssignLHS
import CodeGen.AssignRHS
import Utilities.Definitions
import Debug.Trace


instance CodeGen Stat where
  codegen (Declaration t ident@(Ident name _) rhs _) = do
    instr <- codegen rhs
    (map', offset) <- get
    let newOffset = offset - typeSize t
    let newMap = Map.insert name newOffset map'
    put (newMap, newOffset)
    let str = [STR W NoIdx R0 [SP,ImmI newOffset]]
    return $ instr ++ str
  codegen (Block s _)
    = genInNewScope s
  codegen (Seq s1 s2 _) = do
    instr1 <- codegen s1
    instr2 <- codegen s2
    return $ instr1 ++ instr2
  codegen (Skip _)
    = return []
  -- NEEDS TO CALL STR FOR THE CORRECT NUMBER OF BYTES
  codegen (Assignment lhs rhs _) = do
    evalRHS <- codegen rhs
    saveRHS <- push [R0]
    evalLHS <- codegen lhs
    getRHS  <- pop [R1]
    let size = sizeOfLHS lhs
    let doAssignment = [STR sizeOfLHS NoIdx R0 [R1]]
    return $ evalRHS ++ saveRHS ++ evalLHS ++ getRHS ++ doAssignment
  codegen (Return expr _)
    = codegen expr
  codegen (Exit expr _) = do
    evalExpr <- codegen expr
    return $ evalExpr ++ [BL "exit"]
  codegen (If cond thenStat elseStat _) = do
    let evalCond       = [CMP R0 (ImmOp2 0)]
    elseStatLabel      <- getNextLabel
    let branchIfFalse  = [BEQ elseStatLabel]
    execThenStat       <- genInNewScope thenStat
    execElseStat       <- genInNewScope elseStat
    afterIfLabel       <- getNextLabel
    let branchAfterIf  = [BT afterIfLabel]
    return $  evalCond ++
              branchIfFalse ++
              execThenStat ++
              branchAfterIf ++
              [Def elseStatLabel] ++
              execElseStat ++
              [Def afterIfLabel]
  codegen (While cond stat _) = do
    loopBodyLabel <- getNextLabel
    loopCondLabel <- getNextLabel
    -- does order of these two matter?
    evalCond      <- codegen cond
    execBody      <- genInNewScope stat
    return $
      [BT loopCondLabel] ++
      [Def loopBodyLabel] ++
      execBody ++
      [Def loopCondLabel] ++
      evalCond ++
      [CMP R0 (ImmOp2 1)] ++
      [BEQ loopBodyLabel]
  codegen _
    = error "not yet implemented"

-- Codegens the statement inside of a new scope
genInNewScope :: Stat -> InstructionMonad [Instr]
genInNewScope s = do
  save
  let sizeOfScope = scopeSize s
  (env, _) <- get
  let envWithOffset = Map.map (+ sizeOfScope) env
  put (envWithOffset, sizeOfScope)
  let createStackSpace = [SUB NF SP SP (ImmOp2 sizeOfScope)]
  instrs <- codegen s
  let clearStackSpace = [ADD NF SP SP (ImmOp2 sizeOfScope)]
  restore
  return $ createStackSpace ++ instrs ++ clearStackSpace

-- Returns the Size (word, byte etc.) of an AssignLHS
sizeOfLHS :: AssignLHS -> Size
sizeOfLHS (Var (Ident _ (Info t _)) _)
  = sizeFromType t
sizeOfLHS (ArrayDeref (ArrayElem (Ident (Info t _)) _) _)
  = sizeFromType t
sizeOfLHS (PairDeref (PairElem _ expr _) _)
  = error "don't know how to get type of PairDeref"
