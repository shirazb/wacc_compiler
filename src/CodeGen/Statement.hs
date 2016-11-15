{- This module generates ARM Assembly code for statements -}

module CodeGen.Statement where

{- LOCAL IMPORTS -}
import CodeGen.Assembly
import Utilities.Definitions
import Control.Monad.State(get, put, lift)
import qualified Data.Map as Map
import Control.Monad.StateStack


instance CodeGen Stat where
  codegen (Declaration t ident@(Ident name _) rhs _) = do
    instr <- codegen rhs
    (map', offset) <- get
    let newMap = Map.insert name offset map'
    let newOffset = offset + typeSize t
    put (newMap, newOffset)
    let str = [STR (OpReg (Reg 0)) [OpReg SP,Imm offset]]
    return $ instr ++ str
  codegen (Block s _) = do
    let sizeOfscope = scopeSize s
    save
    (map', offset) <- get
    let newMap = Map.map (+ sizeOfscope) map'
    put (newMap, 0)
    let assembleSpace = [SUB (OpReg SP) (OpReg SP) (Imm sizeOfscope)]
    instr <- codegen s
    restore
    return $ assembleSpace ++ instr


instance CodeGen AssignRHS where
  codegen (ExprAssign e _)
    = codegen e

instance CodeGen Expr where
  codegen (IntLit i _)
    = return [LDR (OpReg (Reg 0)) (Imm i)]

scopeSize :: Stat -> Int
scopeSize (Declaration t _ _ _)
  = typeSize t
scopeSize (Seq s1 s2 _)
  = scopeSize s1 + scopeSize s2
scopeSize _
  = 0


typeSize :: Type -> Int
typeSize (BaseT BaseInt)
  = 4
typeSize _
  = 0
