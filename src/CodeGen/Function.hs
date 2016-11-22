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
-- pretty sure we have fogrgotten a lot of stuff
-- in the functions
instance CodeGen Func where
  codegen (Func t ident@(Ident name _) (ParamList params _) body _) = do
    saveStackInfo
    saveLR <- push [LR]
    addParamsToEnv params 0
    let sizeOfScope = scopeSize body
    (env, _) <- getStackInfo
    let envWithOffset = Map.map (+ sizeOfScope) env
    putStackInfo (envWithOffset, sizeOfScope)
    let createStackSpace = [SUB NF SP SP (ImmOp2 sizeOfScope)]
    instrs <- codegen body
    let clearStackSpace = [ADD NF SP SP (ImmOp2 sizeOfScope)]
    restorePC <- pop [PC]
    let listOfInstrs = saveLR ++ createStackSpace ++ instrs ++ clearStackSpace ++ restorePC
    let newFunc = FuncA name listOfInstrs
    addFunction newFunc
    restoreStackInfo
    return []

addParamsToEnv :: [Param] -> Int -> CodeGenerator ()
addParamsToEnv [] _
  = return ()
addParamsToEnv (Param t (Ident name _) _ : ps) offsetToParam = do
  (env, offset)         <- getStackInfo
  let newEnv            = Map.insert name offsetToParam env
  let offsetToNextParam = offsetToParam + typeSize t
  putStackInfo (newEnv, offset)
  addParamsToEnv ps offsetToNextParam
