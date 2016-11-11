module Semantics.TypeChecker.Program (
  generateTypeErrorMessages
) where

import Control.Monad.Writer.Strict

import Semantics.TypeChecker.Function
import Semantics.TypeChecker.Statement
import Utilities.Definitions

typeCheckProgram :: Program -> TypeChecker ()
typeCheckProgram (Program fs main) = do
  when (checkForReturnInMain main) (tell ["Semantic Error: Return is not allowed in main"])
  mapM_ typeCheckFunc fs
  typeCheckStat main

checkForReturnInMain :: Stat -> Bool
checkForReturnInMain (Return _ _)
  = True
checkForReturnInMain (Seq s1 s2 _)
  = checkForReturnInMain s1 || checkForReturnInMain s2
checkForReturnInMain (If _ s1 s2 _)
  = checkForReturnInMain s1 || checkForReturnInMain s2
checkForReturnInMain (While _ s1 _)
  = checkForReturnInMain s1
checkForReturnInMain _
  = False

generateTypeErrorMessages :: Program -> [String]
generateTypeErrorMessages
  = execWriter . typeCheckProgram
