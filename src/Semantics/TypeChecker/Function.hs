module Semantics.TypeChecker.Function (
  typeCheckFunc
) where

import Semantics.TypeChecker.Statement
import Utilities.Definitions

typeCheckFunc :: Func -> TypeChecker ()
typeCheckFunc (Func t name params body)
  = typeCheckStat body
