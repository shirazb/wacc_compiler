{- This module annotates the AST -}

module Semantics.Annotators.AST (annotateAST) where

import Control.Monad.State.Strict     (runState, runStateT, get)
import qualified Data.Map as Map

{- LOCAL IMPORTS -}
import Semantics.Annotators.Class     (annotateClass, addClassDeclToST)
import Semantics.Annotators.Function  (annotateFunc, addFuncDeclToST)
import Semantics.Annotators.Statement
import Semantics.Annotators.Util
import Semantics.ErrorMessages
import Utilities.Definitions

-- POST: Traverses the AST anontating it and performing lexical scoping.
--       Returns the annotated AST and the list of valid types.
annotateAST :: AST -> AST
annotateAST (Program cs fs main)
  = runScopeAnalysis annotateProgram
  where
    annotateProgram = do
      -- Populating global scope with the classes
      newCs  <- mapM addClassDeclToST cs
      -- Populating global scope with the functions
      newFs  <- mapM addFuncDeclToST fs
      newCs' <- mapM annotateClass newCs
      newFs' <- mapM annotateFunc newFs
      inChildScopeAndWrap (Program newCs' newFs') (annotateStat main)
