{- This module produces ARM code for our program -}

module CodeGen.Program where

import qualified Data.Map as Map
import Control.Monad.State

{- LOCAL IMPORTS -}
import CodeGen.Assembly
import CodeGen.Statement
import Utilities.Definitions hiding (Env)


genInstrFromAST :: AST -> InstructionMonad [Instr]
genInstrFromAST (Program fs body) = do
  let sizeOfscope = scopeSize body
  put (Map.empty, sizeOfscope)
  let makeRoomStack = [SUB NF SP SP (ImmI sizeOfscope)]
  instr <- codegen body
  let clearSpace = [ADD NF SP SP (ImmI sizeOfscope)]
  let succesfulExit = [Mov R0 (ImmI 0)]
  return $ makeRoomStack ++ instr ++ clearSpace ++ succesfulExit
