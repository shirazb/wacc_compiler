module Semantics.Annotators.AST (
  annotateAST
) where

import qualified Data.Map as Map
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict

import Semantics.Annotators.Expression
import Semantics.Annotators.Function
import Semantics.Annotators.Statement
import Semantics.ErrorMsgs
import Utilities.Definitions

-- How do you have fs in the parent scope of stat?
annotateAST :: AST -> AST
annotateAST (Program fs stat)
  = undefined
