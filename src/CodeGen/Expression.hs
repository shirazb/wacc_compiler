{- This module generates ARM Assembly code for expressions -}

module CodeGen.Expression where

import qualified Prelude
import Prelude hiding (LT, GT, EQ)
import Control.Monad.StateStack
import Control.Monad.State.Strict (get, put, lift)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Debug.Trace

{- LOCAL IMPORTS -}
import CodeGen.Assembly
import Utilities.Definitions
import CodeGen.InBuiltFunctions


instance CodeGen Expr where
  -- move proper escaping to addData and change the inbuilt funcs
  codegen (StringLit s _) = do
    msgNum <- getNextMsgNum
    let directive = MSG msgNum s (length s)
    addData directive
    return [LDR W NoIdx R0 [MsgName msgNum]]
  codegen (IntLit i _)
    = return [LDR W NoIdx R0 [ImmLDRI i]]
  codegen (CharLit c _)
    = if c == '\0'
        then return [Mov R0 (ImmI 0)]
        else return [Mov R0 (ImmC c)]
  codegen (BoolLit b _)
    = return [Mov R0 (ImmI bInt)]
    where
      bInt = if b then 1 else 0
  codegen (PairLiteral _)
    = return [Mov R0 (ImmI 0)]
  codegen (IdentE (Ident name (Info t _)) _) = do
    let size = sizeFromType typeSizesLDR t
    (env, _) <- getStackInfo
    let offset = fromJust (Map.lookup name env)
    return [LDR size NoIdx R0 [RegOp SP, ImmI offset]]
  codegen expr@(ExprArray ae _) = do
    saveR4     <- push [R4]
    loadAddrR4 <- codegen ae
    let size = sizeFromType typeSizesLDR (typeOfExpr expr)
    let loadElem = [LDR size NoIdx R4 [RegOp R4], Mov R0 (RegOp R4)]
    restoreR4       <- pop [R4]
    return $ saveR4     ++
             loadAddrR4 ++
             loadElem   ++
             restoreR4
  codegen (UnaryApp Not e _) = do
    instr <- codegen e
    let notE = [EOR R0 R0 (ImmI 1)]
    return $ instr ++ notE
  codegen (UnaryApp Neg e _) = do
    instr <- codegen e
    let negE = [RSBS R0 R0 (ImmI 0)]
    return $ instr ++ negE
  -- len does not work like this
  codegen (UnaryApp Len e _) = do
    instr <- codegen e
    let getLen = [LDR W NoIdx R0 [RegOp R0]]
    return $ instr ++ getLen
  -- codegen (UnaryApp Ord (CharLit c _) _)
  --   = return [Mov R0 (ImmC c)]
  codegen (UnaryApp _ e _)
    = codegen e
  -- codegen (BinaryApp op@(Logic _) e@(BoolLit b _) (BoolLit b' _) _) = do
  --   firstExpr <- codegen e
  --   performLogicOp <- getBinOpInstr op
  --   return $ firstExpr ++ performLogicOp
  codegen(BinaryApp op@(Logic _) e e' _) = do
    firstExpr <- codegen e
    performLogicOp <- generateLogicInstr e' op
    return $ firstExpr ++ performLogicOp
  codegen (BinaryApp op e e' _) = do
    instr     <- codegen e
    saveFirst <- push [R0]
    instr1    <- codegen e'
    let evaluate = [Mov R1 (RegOp R0)]
    restoreR0 <- pop [R0]
    traceM "We get to this binary case?"
    binOpInstr <- getBinOpInstr op
    return $ instr      ++
             saveFirst  ++
             instr1     ++
             evaluate   ++
             restoreR0  ++
             binOpInstr

instance CodeGen ArrayElem where

  codegen (ArrayElem ident@(Ident name (Info (BaseT BaseString) ctxt)) [i] pos)
    = codegen (ArrayElem (Ident name (Info (ArrayT 1 (BaseT BaseChar)) ctxt)) [i] pos)

  codegen ae@(ArrayElem ident@(Ident _ info) idxs _) = do
    let t@(ArrayT _ innerType) = typeInfo info
    loadIdentIntoR4 <- loadIdentAddr R4 ident

    derefInnerTypes <- codeGenArrayElem t idxs

    return $
      loadIdentIntoR4  ++
      derefInnerTypes
-- POST : leaves the address of the element in R4
codeGenArrayElem :: Type -> [Expr] -> CodeGenerator [Instr]

codeGenArrayElem t [i]
  = loadArrayElemAddr t i
codeGenArrayElem array@(ArrayT dim innerType) (i : is) = do
  calcAddrOfElem <- calcAddrOfElem array i

  let dereference  = [LDR W NoIdx R4 [RegOp R4]]

  derefInnerArray  <- codeGenArrayElem array is
  return $
    calcAddrOfElem  ++
    dereference     ++
    derefInnerArray
codeGenArrayElem t is
  = error $ "Error in codeGenArrayElem: Did not match any case.\n" ++
            "Type:     " ++ show t ++ "\n" ++
            "Indexes:  " ++ show is ++ "\n"

calcAddrOfElem :: Type -> Expr -> CodeGenerator [Instr]
calcAddrOfElem (ArrayT dim innerType) idx = do
  -- now we generate the code to get the expressions
  -- we make the assumption that we store the value in register zero
  calcIdx          <- codegen idx
  errorHandling    <- branchWithFunc genCheckArrayBounds BL
  -- this is where we skip the length of the array
  -- in wacc the lenght of the array is always stored as the
  -- first element in the array
  let skipDim      = [ADD NF R4 R4 (ImmOp2 4)]
  -- this is where we skip to the correct element
  -- because we areusing lsl we have to divide the size of the type by 2
  let skipToElem   = [ADD NF R4 R4 (Shift R0 LSL (typeSize innerType `div` 2))]
  return $
      calcIdx ++
      errorHandling ++
      skipDim ++
      skipToElem
loadArrayElemAddr t e
  = error $ "Error in CodeGen.Expression.loadArrayElemAddr: Calling with non array type.\n" ++
            "Type: " ++ show t ++ "\n" ++
            "Idx:  " ++ show e ++ "\n"

-- Op must be a reg
loadIdentAddr :: Reg -> Ident -> CodeGenerator [Instr]
loadIdentAddr r (Ident name info) = do
  (env, offsetSP) <- getStackInfo
  let offsetToVar = fromJust (Map.lookup name env)
  return [LDR W NoIdx r [RegOp SP, ImmI offsetToVar]]

generateLogicInstr :: Expr -> BinOp -> CodeGenerator [Instr]
generateLogicInstr e (Logic op) = do
  label <- getNextLabel
  let fstCMP = [CMP R0  (ImmOp2 (logicNum op)), BEQ label]
  instr1 <- codegen e
  let labelJump = [Def label]
  return $ fstCMP ++ instr1 ++ labelJump


logicNum :: LogicalOp -> Int
logicNum op = case op of
             AND -> 0
             OR  -> 1

invertLogicalNum :: Int -> Int
invertLogicalNum 0
  = 1
invertLogicalNum 1
  = 0
invertLogicalNum _
  = error "invertLogicalNum called with a value which is not in [0,1]"

getAdditiveOpInstr op = do
  let operation = [op S R0 R0 (RegOp2 R1)]
  errorHandling <- branchWithFunc genOverFlowFunction BLVS
  return $ operation ++ errorHandling

getBinOpInstr :: BinOp -> CodeGenerator [Instr]
getBinOpInstr (Arith Add)
  = getAdditiveOpInstr ADD
getBinOpInstr (Arith Sub)
  = getAdditiveOpInstr SUB
getBinOpInstr (Arith Div) = do
  errorHandling <- branchWithFunc genCheckDivideByZero BL
  return $ errorHandling ++ [BL "__aeabi_idiv"]
getBinOpInstr (Arith Mod) = do
   errorHandling <- branchWithFunc genCheckDivideByZero BL
   return $ errorHandling ++ [BL "__aeabi_idivmod", Mov R0 (RegOp R1)]
getBinOpInstr (Arith Mul) = do
  let operation = [SMULL R0 R1 R0 R1, CMP R1 (Shift R0 ASR 31)]
  errorHandling <- branchWithFunc genOverFlowFunction BLNE
  return $ operation ++ errorHandling
getBinOpInstr (Logic op) = do
  label <- getNextLabel
  let fstCMP = [CMP R0 (ImmOp2 (logicNum op)), BEQ label]
  let setFalse = [Mov R0 (ImmI (invertLogicalNum $ logicNum op))]
  let labelJump = [Def label]
  return $ fstCMP ++ setFalse ++ labelJump
getBinOpInstr (RelOp op) = do
  let comp = [CMP R0 (RegOp2 R1)]
  instr <- generateRelInstr op
  return $ comp ++ instr
  where
    generateRelInstr LT
      = return [MOVLT R0 (ImmOp2 1), MOVGE R0 (ImmOp2 0)]
    generateRelInstr LTE
      = return [MOVLE R0 (ImmOp2 1), MOVGT R0 (ImmOp2 0)]
    generateRelInstr GTE
      = return [MOVGE R0 (ImmOp2 1), MOVLT R0 (ImmOp2 0)]
    generateRelInstr GT
      = return [MOVGT R0 (ImmOp2 1), MOVLE R0 (ImmOp2 0)]

-- we should check taht removing this ++ [CMP R0 (ImmOp2 0)] didnt break anything
getBinOpInstr (EquOp op) = do
  let comp = [CMP R0 (RegOp2 R1)]
  instr <- generateEqualityInstr op
  return $ comp ++ instr
  where
    generateEqualityInstr EQ
      = return [MOVEQ R0 (ImmOp2 1), MOVNE R0 (ImmOp2 0)]
    generateEqualityInstr NEQ
      = return [MOVNE R0 (ImmOp2 1), MOVEQ R0 (ImmOp2 0)]

-- Returns number of bytes an expression occupies
exprSize :: Expr -> Int
exprSize
 = typeSize . typeOfExpr

-- Returns type of the expression
typeOfExpr :: Expr -> Type
typeOfExpr e = case e of
  StringLit{}    -> BaseT BaseString -- ArrayT 1 (BaseT BaseChar)
  CharLit{}      -> BaseT BaseChar
  IntLit{}       -> BaseT BaseInt
  BoolLit{}      -> BaseT BaseBool
  PairLiteral{}  -> Pair
  IdentE (Ident _ info) _    -> typeInfo info
  ExprArray ae@ArrayElem{} _ -> typeOfArrayElem ae
  UnaryApp unOp _ _      -> typeOfUnOp unOp
  BinaryApp binOp _ _ _  -> typeOfBinOp binOp

typeOfArrayElem :: ArrayElem -> Type
typeOfArrayElem (ArrayElem (Ident _ (Info (BaseT BaseString) _)) idxs _)
  = BaseT BaseChar
typeOfArrayElem (ArrayElem (Ident _ info) idxs _)
  | derefDim == 0  = innerType
  | otherwise      = ArrayT derefDim innerType
  where
    ArrayT dim innerType = typeInfo info
    derefDim             = dim - length idxs
typeOfArrayElem ae
  = error $ "CodeGen.Expression.typeOfArrayElem. Calling on ArrayElem: "
            ++ show ae

typeOfUnOp :: UnOp -> Type
typeOfUnOp unOp = case unOp of
  Not -> BaseT BaseBool
  Neg -> BaseT BaseInt
  Len -> BaseT BaseInt
  Ord -> BaseT BaseInt
  Chr -> BaseT BaseChar

typeOfBinOp :: BinOp -> Type
typeOfBinOp binOp = case binOp of
  Logic _ -> BaseT BaseBool
  Arith _ -> BaseT BaseInt
  RelOp _ -> BaseT BaseBool
  EquOp _ -> BaseT BaseBool
