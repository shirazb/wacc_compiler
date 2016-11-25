{- This module generates ARM Assembly code for functions -}

module CodeGen.Function where

import Control.Monad.StateStack
import Control.Monad.State(get, put, lift)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Debug.Trace

{- LOCAL IMPORTS -}
import CodeGen.Assembly
import CodeGen.Expression
import CodeGen.Statement
import Utilities.Definitions

-- TODO: Avoid code duplication with genInNewScope. Only difference
--       is that this does addParamsToEnv after save.
-- pretty sure we have fogrgotten a lot of stuff
-- in the functions
-- what i think is interesting is that in functions we need to do a check for returns and actually make them
-- sooo we need to refactor out this insane amount of duplication :)
codeGenFunc :: Int -> Stat -> CodeGenerator [Instr]
codeGenFunc sizeOfScope (Skip _)
  = return []
codeGenFunc sizeOfScope dec@Declaration{}
  = codegen dec

codeGenFunc sizeOfScope assignment@Assignment{}
  = codegen assignment

codeGenFunc sizeOfScope rd@Read{}
  = codegen rd

codeGenFunc sizeOfScope free@Free{}
  = codegen free

codeGenFunc sizeOfScope ret@Return{} = do
  instr <- codegen ret
  let clearStack = [ADD NF SP SP (ImmOp2 sizeOfScope)]
  restorePC <- pop [PC]
  return $ instr ++ clearStack ++ restorePC

codeGenFunc sizeOfScope exit@Exit{}
  = codegen exit

codeGenFunc sizeOfScope print@Print{}
  = codegen print

codeGenFunc sizeOfScope println@Println{}
  = codegen println

codeGenFunc sizeOfScope ifStat@(If cond thenStat elseStat _) = do
  condInstr          <- codegen cond
  elseStatLabel      <- getNextLabel
  let branchIfFalse  = [CMP R0 (ImmOp2 0), BEQ elseStatLabel]
  execThenStat       <- genInNewScopeFunc sizeOfScope thenStat
  execElseStat       <- genInNewScopeFunc sizeOfScope elseStat
  afterIfLabel       <- getNextLabel
  let branchAfterIf  = [BT afterIfLabel]
  return $  condInstr ++
            branchIfFalse ++
            execThenStat ++
            branchAfterIf ++
            [Def elseStatLabel] ++
            execElseStat ++
            [Def afterIfLabel]

codeGenFunc sizeOfScope seq@(Seq s1 s2 _) = do
  s1Instr <- codeGenFunc sizeOfScope s1
  s2Instr <- codeGenFunc sizeOfScope s2
  return $ s1Instr ++ s2Instr

codeGenFunc sizeOfScope while@(While cond st _) = do
  loopBodyLabel <- getNextLabel
  loopCondLabel <- getNextLabel

  evalCond      <- codegen cond

  execBody      <- genInNewScopeFunc sizeOfScope st
  return $
    [BT loopCondLabel, Def loopBodyLabel] ++
    execBody ++
    [Def loopCondLabel] ++
    evalCond ++
    [CMP R0 (ImmOp2 1), BEQ loopBodyLabel]

codeGenFunc sizeOfScope blk@(Block s _)
  = genInNewScopeFunc sizeOfScope s

genInNewScopeFunc :: Int -> Stat -> CodeGenerator [Instr]
genInNewScopeFunc outerScopeSize s = do
  (createStackSpace, clearStackSpace) <- prepareScope s
  instrs <- codeGenFunc outerScopeSize s
  restoreStackInfo
  return $ createStackSpace ++ instrs ++ clearStackSpace

instance CodeGen Func where
  codegen (Func t ident@(Ident name _) (ParamList params _) body _) = do
    saveStackInfo
    addParamsToEnv params 0
    saveLR <- push [LR]
    traceM $ "The first instruction is in the function is: " ++ show saveLR
    let sizeOfScope = scopeSize body
    (env, _) <- getStackInfo
    let envWithOffset = Map.map (+ sizeOfScope) env
    putStackInfo (envWithOffset, sizeOfScope)
    (createStackSpace, clearStackSpace) <- manageStack sizeOfScope
    instrs <- codeGenFunc sizeOfScope body
    traceM $ "This is the body of the function: " ++ show instrs
    restorePC <- pop [PC]
    let listOfInstrs = saveLR ++ createStackSpace ++ instrs ++ [LTORG] -- ++ clearStackSpace ++ restorePC
    let newFunc = FuncA ("f_" ++ name) listOfInstrs
    addFunction newFunc
    restoreStackInfo
    return []

addParamsToEnv :: [Param] -> Int -> CodeGenerator ()
addParamsToEnv [] _
  = return ()
addParamsToEnv (Param t (Ident name _) _ : ps) offsetToParam = do
  (env, offset)         <- getStackInfo
  let newEnv            = Map.insert name offsetToParam env
  let offsetToNextParam = offsetToParam + typeSize t
  putStackInfo (newEnv, offset)
  addParamsToEnv ps offsetToNextParam
