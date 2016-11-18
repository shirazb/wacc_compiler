{- This module generates ARM Assembly code for expressions -}

module CodeGen.Expression where

import Control.Monad.StateStack
import Control.Monad.State(get, put, lift)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

{- LOCAL IMPORTS -}
import CodeGen.Assembly
import Utilities.Definitions

instance CodeGen Expr where
  codegen (IntLit i _)
    = return [LDR W NoIdx R0 [ImmLDRI i]]
  codegen (CharLit c _)
    = return [Mov R0 (ImmC c)]
  codegen (BoolLit b _)
    = return [Mov R0 (ImmI bInt)]
    where
      bInt = if b then 1 else 0
  codegen (PairLiteral _)
    = return [Mov R0 (ImmI 0)]
  codegen (IdentE (Ident name (Info t _)) _) = do
    let size = sizeFromType t
    (env, _) <- get
    let offset = fromJust $ Map.lookup name env
    return [LDR size NoIdx R0 [SP, ImmI offset]]
  codegen (ExprArray _ _)
    = undefined
  codegen (UnaryApp Not e _) = do
    instr <- codegen e
    let notE = [EOR R0 R0 (ImmI 1)]
    return $ instr ++ notE
  codegen (UnaryApp Neg e _) = do
    instr <- codegen e
    let negE = [RSBS R0 R0 (ImmI 0)]
    return $ instr ++ negE
  codegen (UnaryApp Len e _) = do
    instr <- codegen e
    let getLen = [LDR W NoIdx R0 [R0]]
    return $ instr ++ getLen
  codegen (UnaryApp Ord (CharLit c _) _)
    = return [Mov R0 (ImmC c)]
  codegen (UnaryApp Ord e _)
    = codegen e
  -- refactor this so
  -- that our chooseop function
  -- ranges over all bin ops
  -- then we dont have to duplicate
  -- the other code
  codegen (BinaryApp op e e' _) = do
    instr <- codegen e
    let saveFirst = [Push R0]
    instr1 <- codegen e'
    let evaluate = [Mov R1 R0, Pop R0]
    return $ instr ++ saveFirst ++ instr1 ++ evaluate ++ chooseBinOp op

-- minimal bytes
-- length of array stored first
-- requries out of bounds check
-- first save reg used to point to array elem
-- then get ident location
-- then calc idx
-- check idx in bounds
-- add 4 to loc to point to first elem
-- add offset (ideally with LSL) (say R4 now points to element)
-- Mov R4, [R4]
-- recurse
-- mov found value into R0
-- PROBABLY BROKEN...
instance CodeGen ArrayElem where
  codegen (ArrayElem ident@(Ident name info) idxs _) = do
    let ArrayT _ innerType = typeInfo info
    saveR4          <- push [R4]
    loadIdentIntoR4 <- loadIdentAddr R4 ident
    derefInnerTypes <- codeGenArrayElem innerType idxs
    restoreR4       <- pop [R4]
    return $
      saveR4           ++
      loadIdentIntoR4  ++
      derefInnerTypes  ++
      restoreR4

codeGenArrayElem :: Type -> [Expr] -> InstructionMonad [Instr]
codeGenArrayElem t []
  = return [Mov R0 R4]
codeGenArrayElem (ArrayT dim innerType) (i : is) = do
  calcIdx          <- codegen i
  let skipDim      = [ADD NF R4 R4 (ImmOp2 4)]
  let skipToElem   = [ADD NF R4 R4 (Shift R0 LSL (typeSize innerType))]
  let dereference  = [LDR (sizeFromType innerType) NoIdx R4 [R4]]
  derefInnerArray  <- codeGenArrayElem innerType is
  return $
    calcIdx          ++
    -- check array index in bounds
    skipDim          ++
    skipToElem       ++
    dereference      ++
    derefInnerArray

-- Op must be a reg
loadIdentAddr :: Op -> Ident -> InstructionMonad [Instr]
loadIdentAddr r (Ident name info) = do
  (env, offsetSP) <- get
  let offsetToVar = fromJust (Map.lookup name env) + offsetSP
  return [LDR W NoIdx r [SP, ImmLDRI offsetToVar]]

chooseBinOp :: BinOp -> [Instr]
chooseBinOp (Arith Add)
  = [ADD S R0 R0 (RegOp2 R1)]
chooseBinOp (Arith Sub)
  = [SUB S R0 R0 (RegOp2 R1)]
chooseBinOp (Arith Div)
  = error "Division not implemented"
chooseBinOp (Arith Mod)
  = error "Mod not implemented"
chooseBinOp (Arith Mul)
   = [SMULL R0 R1 R0 R1, CMP R1 (RegShift R0 ASR)]
