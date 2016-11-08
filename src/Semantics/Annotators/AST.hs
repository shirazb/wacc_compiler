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
import Debug.Trace


annotateAST :: AST -> AST
annotateAST (Program fs main)
  = newAST
  where
    (newAST, _) = runState annotateProgram (ST None Map.empty)
    annotateProgram = do
      idents <- mapM addFuncDeclToST fs
      let newFs = zipWith (replaceIdent) idents fs
      newFs' <- mapM annotateFunc newFs
      inChildScopeAndWrap (Program newFs') (annotateStat main)


replaceIdent :: Ident -> Func -> Func
replaceIdent i (Func t _ pl st pos)
  = Func t i pl st pos
