{- This module generates ARM Assembly code for statements -}

module CodeGen.Statement where

{- LOCAL IMPORTS -}

import CodeGen.Assembly
import Utilities.Definitions

instance CodeGen Stat where
  codegen (Skip _)
    = [Mov (OpReg (Reg 0)) (ImmMov 0)]

  codegen (Exit e _)
    = codegen e ++ [BL "exit"] ++ [Mov (OpReg (Reg 0)) (ImmMov 0)]

instance CodeGen Expr where
  codegen (IntLit i _)
    = [LDR (OpReg (Reg 0)) (ImmLDR i)]
{-
  codegen (StringLit _ _)
    = undefined
  codegen (IdentE ident pos)
    = undefined
  -- how do we know???
  -- where the variable is on the stack??
  --
  codegen (BinaryApp (Arith op) e e' _)
    = codegen e ++ [Push (Reg 0)] ++
      codegen e'++ [Mov (Reg 1) (Reg 0)]
      ++ [Pop (Reg 0)] ++ [chooseArithInstr op]

chooseArithInstr :: ArithOp -> Instr
chooseArithInstr Add
  = ADDS (Reg 0) (Reg 0) (Reg 1)
chooseArithInstr Sub
  = SUBS (Reg 0) (Reg 0) (Reg 1)
chooseArithInstr Mul
  = SMULL (Reg 0) (Reg 1) (Reg 0) (Reg 1)

-}
