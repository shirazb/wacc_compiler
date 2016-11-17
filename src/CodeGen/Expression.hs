{- This module generates ARM Assembly code for expressions -}

module CodeGen.Expression where

{- LOCAL IMPORTS -}
import CodeGen.Assembly
import Utilities.Definitions
import Control.Monad.State(get, put, lift)
import qualified Data.Map as Map
import Control.Monad.StateStack
import Data.Maybe (fromJust)

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
  codegen (BinaryApp arith@(Arith op) e e' _) = do
    instr <- codegen e
    let saveFirst = [Push R0]
    instr1 <- codegen e'
    let evaluate = [Mov R1 R0, Pop R0, chooseArithOp op]
    return $ instr ++ saveFirst ++ instr1 ++ evaluate

chooseArithOp :: ArithOp -> Instr
chooseArithOp Add
  = ADD S R0 R0 R1
chooseArithOp Sub
  = SUB S R0 R0 R1
chooseArithOp _
   = error "Not all arithmetic ops defined"
