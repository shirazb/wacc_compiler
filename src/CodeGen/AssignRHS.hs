{- This module generates ARM Assembly code for AssignRHS -}

module CodeGen.AssignRHS where

import Control.Monad.StateStack
import Control.Monad.State(get, put, lift)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

{- LOCAL IMPORTS -}
import CodeGen.Assembly
import CodeGen.Expression
import Utilities.Definitions

instance CodeGen AssignRHS where
  codegen (ExprAssign e _)
    = codegen e
  codegen (ArrayLitAssign expr _)
    = undefined
  codegen NewPairAssign{}
    = undefined
  codegen (FuncCallAssign ident@(Ident name info) es _) = do
    let FuncT retType paramTypes = typeInfo info
    params <- mapM codegen es
    let pushParams = concat $ reverse $ zipWith (\p t -> p ++ pushParam t) params paramTypes
    let callFunc = [BL name]
    let paramSpace  = sum (map typeSize paramTypes)
    let clearParams = [ADD NF SP SP (ImmI paramSpace)]
    return $ pushParams ++ callFunc ++ clearParams
    where
      pushParam :: Type -> [Instr]
      pushParam t
        = [STR size Pre R0 [SP, ImmI (- typeSize t)]]
        where
          size = sizeFromType t
