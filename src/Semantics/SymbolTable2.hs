import qualified Data.Map as Map

module SymbolTable (

) where

type Env k a = Map k a

data ST = SymbolTable [ST] Env

initST :: ST
initST
  = SymbolTable [] (Map.empty)

emptyST :: ST
emptyST
  = SymbolTable [] emptyMap

buildST :: Program -> ST
buildST (Program funcs main)
  = SymbolTable [map buildSTFromFunc funcs emptyMap, func_that_traverses_stat_building_map main]

buildSTFromFunc :: Func -> Env -> ST
buildSTFromFunc (Func returnType name params body) env
  = SymbolTable
    (func_that_traverses_stat_building_STs body (env ++ paramListToEnv params))
    ()

func_that_traverses_stat_building_STs :: Stat -> Map -> [ST]
func_that_traverses_stat_building_STs
  = undefined

paramListToEnv :: ParamList -> Env
paramListToEnv _
  = undefined
