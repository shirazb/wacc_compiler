module Semantics.TypeChecker.Program (
  typeCheckProgram
) where

import Semantics.TypeChecker.Function
import Semantics.TypeChecker.Statement
import Utilities.Definitions

typeCheckProgram :: Program -> TypeChecker ()
typeCheckProgram (Program fs main) = do
  mapM_ typeCheckFunc fs
  typeCheckStat main
