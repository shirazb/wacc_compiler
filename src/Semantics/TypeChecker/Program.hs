{- This module type checks the entire program -}

module Semantics.TypeChecker.Program (generateTypeErrorMessages) where

import Control.Monad.Writer.Strict

{- LOCAL IMPORTS -}
import Semantics.TypeChecker.Function
import Semantics.TypeChecker.Statement
import Semantics.ErrorMessages
import Utilities.Definitions

-- POST: Type checks the program
typeCheckProgram :: Program -> TypeChecker ()
typeCheckProgram (Program fs main) = do
  when (checkForReturnInMain main) (tell [returnInMain])
  when (checkForBreakOutsideLoop main) (tell [breakWithoutLoop])
  when (checkForContinueOutsideLoop main) (tell [continueWithoutLoop])
  mapM_ typeCheckFunc fs
  typeCheckStat main

{- HELPER FUNCTIONS -}

-- POST: Returns true if a return statement is present in the main body
checkForReturnInMain :: Stat -> Bool
checkForReturnInMain (Return _ _)
  = True
checkForReturnInMain (Seq s1 s2 _)
  = checkForReturnInMain s1 || checkForReturnInMain s2
checkForReturnInMain (If _ s1 s2 _)
  = checkForReturnInMain s1 || checkForReturnInMain s2
checkForReturnInMain (While _ s _)
  = checkForReturnInMain s
checkForReturnInMain (For _ _ _ s _)
  = checkForReturnInMain s
checkForReturnInMain (Block s _)
  = checkForReturnInMain s
checkForReturnInMain _
  = False

checkForBreakOutsideLoop :: Stat -> Bool
checkForBreakOutsideLoop (Break _)
  = True
checkForBreakOutsideLoop (Seq s1 s2 _)
  = checkForBreakOutsideLoop s1 || checkForBreakOutsideLoop s2
checkForBreakOutsideLoop (If _ s1 s2 _)
  = checkForBreakOutsideLoop s1 || checkForBreakOutsideLoop s2
checkForBreakOutsideLoop (Block s _)
  = checkForBreakOutsideLoop s
checkForBreakOutsideLoop (For _ _ _ s _)
  = False
checkForBreakOutsideLoop (While _ s _)
  = False
checkForBreakOutsideLoop _
  = False

checkForContinueOutsideLoop :: Stat -> Bool
checkForContinueOutsideLoop (Continue _)
  = True
checkForContinueOutsideLoop (Seq s1 s2 _)
  = checkForContinueOutsideLoop s1 || checkForContinueOutsideLoop s2
checkForContinueOutsideLoop (If _ s1 s2 _)
  = checkForContinueOutsideLoop s1 || checkForContinueOutsideLoop s2
checkForContinueOutsideLoop (Block s _)
  = checkForContinueOutsideLoop s
checkForContinueOutsideLoop (For _ _ _ s _)
  = False
checkForContinueOutsideLoop (While _ s _)
  = False
checkForContinueOutsideLoop _
  = False

-- POST: Generates type error messages
generateTypeErrorMessages :: Program -> [String]
generateTypeErrorMessages
  = execWriter . typeCheckProgram
