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
  checkForReturnInMain main
  checkForBreakOutsideLoop main
  checkForContinueOutsideLoop main
  mapM_ typeCheckFunc fs
  typeCheckStat main

{- HELPER FUNCTIONS -}

-- POST: Generates errors for invalid return statements
checkForReturnInMain :: Stat -> TypeChecker ()
checkForReturnInMain (Return _ pos)
  = tell [returnInMain pos]
checkForReturnInMain (Seq s1 s2 _) = do
  checkForReturnInMain s1
  checkForReturnInMain s2
checkForReturnInMain (If _ s1 s2 _) = do
  checkForReturnInMain s1
  checkForReturnInMain s2
checkForReturnInMain (While _ s _)
  = checkForReturnInMain s
checkForReturnInMain (For _ _ _ s _) 
  = checkForReturnInMain s
checkForReturnInMain (Block s _)
  = checkForReturnInMain s
checkForReturnInMain _
  = return ()

-- POST: Generates errors for invalid break statements
checkForBreakOutsideLoop :: Stat -> TypeChecker ()
checkForBreakOutsideLoop (Break pos)
  = tell [breakWithoutLoop pos]
checkForBreakOutsideLoop (Seq s1 s2 _) = do
  checkForBreakOutsideLoop s1
  checkForBreakOutsideLoop s2
checkForBreakOutsideLoop (If _ s1 s2 _) = do
  checkForBreakOutsideLoop s1
  checkForBreakOutsideLoop s2
checkForBreakOutsideLoop (Block s _)
  = checkForBreakOutsideLoop s
checkForBreakOutsideLoop _
  = return ()

-- POST: Generates errors for invalid continue statements
checkForContinueOutsideLoop :: Stat -> TypeChecker ()
checkForContinueOutsideLoop (Continue pos)
  = tell [continueWithoutLoop pos]
checkForContinueOutsideLoop (Seq s1 s2 _) = do
  checkForContinueOutsideLoop s1
  checkForContinueOutsideLoop s2
checkForContinueOutsideLoop (If _ s1 s2 _) = do
  checkForContinueOutsideLoop s1
  checkForContinueOutsideLoop s2
checkForContinueOutsideLoop (Block s _)
  = checkForContinueOutsideLoop s
checkForContinueOutsideLoop _
  = return ()

-- POST: Generates type error messages
generateTypeErrorMessages :: Program -> [String]
generateTypeErrorMessages
  = execWriter . typeCheckProgram
