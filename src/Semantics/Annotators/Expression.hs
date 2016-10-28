module Semantics.Annotators.Expression (
  annotateExpr
) where

import Control.Monad.State.Strict
import qualified Data.Map as Map

import Semantics.Annotators.Identifier
import Semantics.ErrorMsgs
import Utilities.Definitions

annotateExpr :: Expr -> LexicalScoper Expr
annoateExpr (IdentE ident)
  = IdentE <$> annotateIdent ident
annotateExpr i@(IntLit _)
  = return i
annotateExpr e
  = return e
