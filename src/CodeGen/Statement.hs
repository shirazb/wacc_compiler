{- This module generates ARM Assembly code for statements -}

module CodeGen.Statement where


import Control.Monad.State(get, put, lift)
import qualified Data.Map as Map
import Control.Monad.StateStack
import Data.Maybe (fromJust)

{- LOCAL IMPORTS -}
import CodeGen.Assembly
import CodeGen.Expression
import CodeGen.Function
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
