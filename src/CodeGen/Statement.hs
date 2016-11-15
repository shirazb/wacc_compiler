{- This module generates ARM Assembly code for statements -}

module CodeGen.Statement where


{- LOCAL IMPORTS -}
import CodeGen.Assembly
import Utilities.Definitions
import Parser.BasicCombinators
import Parser.Program
import Semantics.Annotators.AST


-- instance CodeGen Stat where
--   codegen (Skip _)
--     = [Mov (OpReg (Reg 0) (ImmMov 0))]
