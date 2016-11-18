{- This module generates ARM Assembly code for statements -}

module CodeGen.Statement where

import Control.Monad.StateStack
import Control.Monad.State(get, put, lift)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

{- LOCAL IMPORTS -}
import CodeGen.Assembly
import CodeGen.Expression
import CodeGen.AssignLHS
import CodeGen.AssignRHS
import Utilities.Definitions


instance CodeGen Stat where
  codegen (Declaration t ident@(Ident name _) rhs _) = do
    instr <- codegen rhs
    (map', offset) <- get
    let newMap = Map.insert name offset map'
    let newOffset = offset + typeSize t
    put (newMap, newOffset)
    let str = [STR W NoIdx R0 [SP,ImmI offset]]
    return $ instr ++ str
  codegen (Block s _) = do
    save
    let sizeOfscope = scopeSize s
    (map', offset) <- get
    let newMap = Map.map (+ sizeOfscope) map'
    put (newMap, 0)
    let makeRoomStack = [SUB NF SP SP (ImmI sizeOfscope)]
    instr <- codegen s
    let clearSpace = [ADD NF SP SP (ImmI sizeOfscope)]
    restore
    return $ makeRoomStack ++ instr ++ clearSpace
  codegen (Seq s1 s2 _) = do
    instr1 <- codegen s1
    instr2 <- codegen s2
    return $ instr1 ++ instr2
  codegen (Skip _)
    = return []
  -- NEEDS TO CALL STR FOR THE CORRECT NUMBER OF BYTES
  codegen (Assignment lhs rhs _) = do
    evalRHS <- codegen rhs
    let saveRHS = push [R0]
    evalLHS <- codegen lhs
    let getRHS  = pop [R1]
    let doAssignment = [STR W NoIdx R0 [R1]]
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
    execThenStat       <- codegen thenStat
    execElseStat       <- codegen elseStat
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
    execBody      <- codegen stat
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
