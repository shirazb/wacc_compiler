{- This module type checks functions -}

module Semantics.TypeChecker.Function (typeCheckFunc) where

import Control.Monad.Writer.Strict

{- LOCAL IMPORTS -}
import Semantics.TypeChecker.Statement
import Semantics.TypeChecker.Expression
import Semantics.ErrorMessages
import Utilities.Definitions

-- POST: Type checks functions
typeCheckFunc :: Func -> TypeChecker ()
typeCheckFunc (Func (FuncT rT plT) name params body pos)
  = typeCheckStat body >> checkFunctionReturn body rT

{- HELPER FUNCTIONS -}

-- POST: Checks if all the return statements in a function body have an
--       expression which matches the return type of the function
checkFunctionReturn :: Stat -> Type -> TypeChecker ()

checkFunctionReturn (Skip _) expT
  = return ()

checkFunctionReturn ret@(Return expr _) expT = do
  t <- typeCheckExpr expr
  when (t /= expT) (tell [typeMismatch expT t (getPosExpr expr) ret])
  return ()

checkFunctionReturn (If _ s1 s2 _) expT = do
  checkFunctionReturn s1 expT
  checkFunctionReturn s2 expT

checkFunctionReturn (While _ s1 _) expT
  = checkFunctionReturn s1 expT

checkFunctionReturn (Block s1 _) expT
  = checkFunctionReturn s1 expT

checkFunctionReturn (Seq s1 s2 _) expT = do
  checkFunctionReturn s1 expT
  checkFunctionReturn s2 expT

checkFunctionReturn _ _
  = return ()
