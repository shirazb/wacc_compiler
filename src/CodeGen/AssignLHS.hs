{- This module generates ARM Assembly code for AssignLHS -}

module CodeGen.AssignLHS where

import Control.Monad.StateStack
import Control.Monad.State(get, put, lift)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

{- LOCAL IMPORTS -}
import CodeGen.Assembly
import CodeGen.Expression
import Utilities.Definitions

instance CodeGen AssignLHS where
  codegen _
    = error "TODO: AssignLHS"
