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

instance CodeGen Func where
  codegen _
    = return []
