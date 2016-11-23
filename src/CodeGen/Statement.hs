{- This module generates ARM Assembly code for statements -}

module CodeGen.Statement where

import Control.Monad.StateStack
import Control.Monad.State.Strict ( lift )
import qualified Data.Map as Map
import Data.Maybe (fromJust)

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
  -- NEEDS TO CALL STR FOR THE CORRECT NUMBER OF BYTES
  codegen (Assignment lhs rhs _) = do
    --evalRHS <- codegen rhs
    --saveRHS <- push [R0]
    --evalLHS <- codegen lhs
    --getRHS  <- pop [R1]
    --let size = sizeOfLHS lhs
    --let doAssignment = [STR size NoIdx R1 [RegOp R0]]
    --return $ evalRHS ++ saveRHS ++ evalLHS ++ getRHS ++ doAssignment
    evalRHS <- codegen rhs
    let size = sizeOfLHS lhs
    let store = [STR size NoIdx R0 [RegOp SP]]
    return $ evalRHS ++ store
--    evalLHS <- codegen lhs
--    let doAssignment = [STR size NoIdx R1 [RegOp R0]]
--    return $ evalRHS ++ evalLHS ++ doAssignment

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
  codegen (Print e _) = do
    evalE      <- codegen e
    printInstr <- getExprPrintInstr (typeOfExpr e)
    return $ evalE ++ printInstr
  codegen (Println e p) = do
    printE  <- codegen (Print e p)
    printLn <- branchWithFunc genPrintLn BL
    return $ printE ++ printLn
  codegen (Read (Var ident _) p) = do
    let e = IdentE ident p
    evalE <- codegen e
    readInstr <- getExprReadInstr (typeOfExpr e)
    return $
      evalE ++
      readInstr

-- Codegens the statement inside of a new scope
genInNewScope :: Stat -> CodeGenerator [Instr]
genInNewScope s = do
  saveStackInfo
  let sizeOfScope = scopeSize s
  (env, _) <- getStackInfo
  let envWithOffset = Map.map (+ sizeOfScope) env
  putStackInfo (envWithOffset, sizeOfScope)
  let createStackSpace = [SUB NF SP SP (ImmOp2 sizeOfScope)]
  instrs <- codegen s
  let clearStackSpace = [ADD NF SP SP (ImmOp2 sizeOfScope)]
  restoreStackInfo
  return $ createStackSpace ++ instrs ++ clearStackSpace

-- Returns the Size (word, byte etc.) of an AssignLHS
sizeOfLHS :: AssignLHS -> Size
sizeOfLHS (Var (Ident _ (Info t _)) _)
  = sizeFromType typeSizesSTR t
sizeOfLHS (ArrayDeref (ArrayElem (Ident _ (Info t _)) _ _) _)
  = sizeFromType typeSizesSTR t
sizeOfLHS (PairDeref (PairElem _ expr _) _)
  = sizeFromType typeSizesSTR (typeOfExpr expr)
sizeOfLHS _
  = error "are we hitting an error case in sizeoflhs"

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
