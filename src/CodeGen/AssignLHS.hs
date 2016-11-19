{- This module generates ARM Assembly code for AssignLHS -}

module CodeGen.AssignLHS where

import Control.Monad.StateStack
import Control.Monad.State.Strict (get, put, lift)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

{- LOCAL IMPORTS -}
import CodeGen.Assembly
import CodeGen.Expression
import Utilities.Definitions hiding (Env)

instance CodeGen AssignLHS where
  codegen (Var ident _)
    = loadIdentAddr R0 ident
  codegen (ArrayDeref arrayElem _)
    = codegen arrayElem
