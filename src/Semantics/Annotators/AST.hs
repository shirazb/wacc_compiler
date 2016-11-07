module Semantics.Annotators.AST (
  annotateAST
) where

import qualified Data.Map as Map
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict

import Semantics.Annotators.Expression
import Semantics.Annotators.Function
import Semantics.Annotators.Statement
import Semantics.Annotators.Util
import Semantics.ErrorMsgs
import Utilities.Definitions

annotateAST :: AST -> AST
annotateAST (Program fs main)
  = newAST
  where
    (newAST, _) = runState annotateProgram (ST None Map.empty)
    annotateProgram = do
      mapM_ addFuncDeclToST fs
      newFs <- mapM annotateFunc fs
      inChildScopeAndWrap (Program newFs) (annotateStat main)
