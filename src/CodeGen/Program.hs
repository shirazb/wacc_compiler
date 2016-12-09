{- This module generates ARM11 code for our program -}

module CodeGen.Program where

import Control.Monad.StateStack
import qualified Data.Map as Map
import Control.Monad.State.Strict (get, put, lift)

{- LOCAL IMPORTS -}
import CodeGen.Assembly
import CodeGen.Statement
import CodeGen.Expression
import CodeGen.Function
import CodeGen.AssignRHS
import Utilities.Definitions hiding (Env)

-- POST: Generates assembly code for entire program using the AST
genInstrFromAST :: AST -> CodeGenerator [Instr]
genInstrFromAST (Program _ fs body) = do
  mapM_ codegen fs
  let defMain                  = [Def "main"]
  pLr                         <- push [LR]
  let sizeOfscope              = scopeSize body
  putStackInfo (Map.empty, sizeOfscope)
  (makeRoomStack, clearSpace) <- manageStack sizeOfscope
  instr                       <- codegen body
  let succesfulExit            = [Mov R0 (ImmI 0)]
  popPC                       <- pop [PC]
  return $
    [Def "main"]  ++
    pLr           ++
    makeRoomStack ++
    instr         ++
    clearSpace    ++
    succesfulExit ++
    popPC
