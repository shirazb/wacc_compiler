{- This module generates ARM11 Assembly code for functions -}

module CodeGen.Function where

import Control.Monad.StateStack
import Control.Monad.State       (get, put, lift)
import qualified Data.Map as Map
import Data.Maybe                (fromJust)

{- LOCAL IMPORTS -}
import CodeGen.Assembly
import CodeGen.Expression
import CodeGen.Statement
import Utilities.Definitions

-- POST: Generates assembly code for functions
instance CodeGen Func where
  codegen (Func t ident@(Ident name _) (ParamList params _) body _) = do
    saveStackInfo
    addParamsToEnv params 0
    saveLR            <- push [LR]

    let sizeOfScope    = scopeSize body
    (env, _)          <- getStackInfo
    let envWithOffset  = Map.map (+ sizeOfScope) env
    putStackInfo (envWithOffset, sizeOfScope)
    (createStackSpace, clearStackSpace) <- manageStack sizeOfScope

    functionContext   <- getFunctionContext
    putFunctionContext clearStackSpace

    instrs            <- codegen body

    restorePC         <- pop [PC]
    let listOfInstrs   = saveLR ++ createStackSpace ++ instrs ++ [LTORG]
    let newFunc        = FuncA ("f_" ++ name) listOfInstrs
    addFunction newFunc
    putFunctionContext functionContext
    restoreStackInfo
    return []

-- POST: Adds the parameters of a function to the variable mappings
addParamsToEnv :: [Param] -> Int -> CodeGenerator ()
addParamsToEnv [] _
  = return ()
addParamsToEnv (Param t (Ident name _) _ : ps) offsetToParam = do
  (env, offset)         <- getStackInfo
  let newEnv             = Map.insert name offsetToParam env
  let offsetToNextParam  = offsetToParam + typeSize t
  putStackInfo (newEnv, offset)
  addParamsToEnv ps offsetToNextParam
