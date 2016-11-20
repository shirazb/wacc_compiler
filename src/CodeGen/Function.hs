{- This module generates ARM Assembly code for functions -}

module CodeGen.Function where

import Control.Monad.StateStack
import Control.Monad.State(get, put, lift)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

{- LOCAL IMPORTS -}
import CodeGen.Assembly
import CodeGen.Expression
import CodeGen.Statement
import Utilities.Definitions

-- TODO: Avoid code duplication with genInNewScope. Only difference
--       is that this does addParamsToEnv after save.
instance CodeGen Func where
  codegen (Func t ident (ParamList params _) body _) = do
    save
    addParamsToEnv params 0
    let sizeOfScope = scopeSize body
    (env, _) <- get
    let envWithOffset = Map.map (+ sizeOfScope) env
    put (envWithOffset, sizeOfScope)
    let createStackSpace = [SUB NF SP SP (ImmOp2 sizeOfScope)]
    instrs <- codegen body
    let clearStackSpace = [ADD NF SP SP (ImmOp2 sizeOfScope)]
    restore
    return $ createStackSpace ++ instrs ++ clearStackSpace

addParamsToEnv :: [Param] -> Int -> InstructionMonad ()
addParamsToEnv (Param t (Ident name _) _ : ps) offsetToParam = do
  (env, offset)         <- get
  let newEnv            = Map.insert name offsetToParam env
  let offsetToNextParam = offsetToParam + typeSize t
  put (newEnv, offset)
  addParamsToEnv ps offsetToNextParam
