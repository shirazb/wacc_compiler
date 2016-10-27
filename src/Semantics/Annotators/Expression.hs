module Semantics.Annotators.Expression (
  annotateExpr
) where

import Control.Monad.State.Strict
import qualified Data.Map as Map

import Semantics.Annotators.Identifier
import Semantics.ErrorMsgs
import Utilities.Definitions

annotateExpr :: Expr -> LexicalScoper Expr
annotateExpr
  = undefined
