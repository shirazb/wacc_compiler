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
instrsFromAST :: AST -> CodeGenerator [Instr]
instrsFromAST (Program fs body) = do
  mapM_ codegen fs
  saveLR       <- push [LR]
  instructions <- genInNewScope body
  let succesfulExit = [Mov R0 (ImmI 0)]
  restorePC    <- pop [PC]
  return $
    [Def "main"]  ++
    saveLR        ++
    instructions  ++
    succesfulExit ++
    restorePC
