module Semantics.TypeChecker.Function (
  typeCheckFunc
) where

import Semantics.TypeChecker.Statement
import Utilities.Definitions
import Semantics.TypeChecker.Expression
import Control.Monad.Writer.Strict

typeCheckFunc :: Func -> TypeChecker ()
typeCheckFunc (Func (FuncT rT plT) name params body pos)
  = typeCheckStat body >> checkFunctionReturn body rT

checkFunctionReturn :: Stat -> Type -> TypeChecker ()
checkFunctionReturn (Skip _) expT
  = return ()
checkFunctionReturn (Return expr _) expT = do
  t <- typeCheckExpr expr
  when (t /= expT) (tell ["FUNCTION RETURN TYPE DOES NOT MATCH"])
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
checkFunctionReturn _ _ = return ()
