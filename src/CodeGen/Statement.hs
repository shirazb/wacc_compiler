{- This module generates ARM Assembly code for statements -}

module CodeGen.Statement where

import Control.Monad.StateStack
import Control.Monad.State.Strict ( lift )
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Debug.Trace

{- LOCAL IMPORTS -}
import CodeGen.Assembly
import CodeGen.Expression
import CodeGen.AssignLHS
import CodeGen.AssignRHS
import CodeGen.InBuiltFunctions
import Utilities.Definitions

instance CodeGen Stat where
  codegen (Declaration t ident@(Ident name _) rhs _) = do
    instr <- codegen rhs
    (map', offset) <- getStackInfo
    let newOffset = offset - typeSize t
    let newMap = Map.insert name newOffset map'
    let chckLookUp  = fromJust $ Map.lookup name newMap
    putStackInfo (newMap, newOffset)
    let str = [STR (sizeFromType typeSizesSTR t) NoIdx R0 [RegOp SP, ImmI newOffset]]
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
    evalE <- codegen e
    freeInstr <- getFreeExprInstr (typeOfExpr e)
    return $
      evalE ++
      freeInstr

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
    let branchIfFalse  = [CMP R0 (ImmOp2 0), BEQ elseStatLabel]
    execThenStat       <- genInNewScope thenStat
    execElseStat       <- genInNewScope elseStat
    afterIfLabel       <- getNextLabel
    let branchAfterIf  = [BT afterIfLabel]
    return $  condInstr ++
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
      [BT loopCondLabel, Def loopBodyLabel] ++
      execBody ++
      [Def loopCondLabel] ++
      evalCond ++
      [CMP R0 (ImmOp2 1), BEQ loopBodyLabel]

  codegen (Print e _) = do
    evalE      <- codegen e
    printInstr <- getExprPrintInstr (typeOfExpr e)
    return $ evalE ++ printInstr

  codegen (Println e p) = do
    printE  <- codegen (Print e p)
    printLn <- branchWithFunc genPrintLn BL
    return $ printE ++ printLn

  codegen (Read (Var ident@(Ident name _) _) p) = do
    let e = IdentE ident p
    (map', _) <- getStackInfo
    let offset = fromJust $ Map.lookup name map'
    let loadAddr = [ADD NF R0 SP (ImmOp2 offset)]
    readInstr <- getExprReadInstr (typeOfExpr e)
    return $ loadAddr ++ readInstr

-- Codegens the statement inside of a new scope
genInNewScope :: Stat -> CodeGenerator [Instr]
genInNewScope s = do
  saveStackInfo
  let sizeOfScope = scopeSize s
  (env, _) <- getStackInfo
  let envWithOffset = Map.map (+ sizeOfScope) env
  putStackInfo (envWithOffset, sizeOfScope)
  (createStackSpace, clearStackSpace) <- manageStack sizeOfScope
  instrs <- codegen s
  restoreStackInfo
  return $ createStackSpace ++ instrs ++ clearStackSpace



getFreeExprInstr :: Type -> CodeGenerator [Instr]
getFreeExprInstr t = case t of
  PairT _ _ -> branchWithFunc genFreePair BL
  _         -> return [BL "free"]

getExprReadInstr :: Type -> CodeGenerator [Instr]
getExprReadInstr t = case t of
  BaseT BaseChar   -> branchWithFunc genReadChar BL
  BaseT BaseInt    -> branchWithFunc genReadInt BL
  _                -> error "Read called with incorrect type in code-gen"

getExprPrintInstr :: Type -> CodeGenerator [Instr]
getExprPrintInstr t = case t of
  BaseT BaseChar   -> return [BL "putchar"]
  BaseT BaseInt    -> branchWithFunc genPrintInt BL
  BaseT BaseBool   -> branchWithFunc genPrintBool BL
  BaseT BaseString -> branchWithFunc genPrintString BL
  _                -> branchWithFunc genPrintReference BL
