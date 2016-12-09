{-
  TODO: THIS EXPLANATION IS DPERECATED -- UPDATE FOR CLASSES

  This module traverses through and annotates the AST.

  These annotators descend the AST performing lexical scoping whilst
  simultaneously adding type information to each identifier encountered. This
  is so that, during type checking, the type of an identifier encountered
  anywhere in the AST is known.

  Lexical scoping is performed using the following symbol table data type:

    data SymbolTable
      = ST SymbolTable Env
      | None

  where Env is a Data.Map.Map of `(String, Context) ==> Info`. Both String and
  Context are needed as functions and variables in the same scope can share the
  same name.

  Each new scope corresponds to its own SymbolTable and Env.
  The inner SymbolTable will point to its direct parent scope, which in turn
  will point to its parent scope, and so on. Thus, the None constructor is
  required to signify when we have reached the global scope that has no parent
  scope.

  With this design, before entering a new scope, we must get the current
  symbol table <st> and use a new symbol table that has an empty map and points
  up to <st>.

  Only declaration statements and function declarations introduce new
  identifiers to the symbol table. To check if these are already in scope, we
  check the current Env and, if required, look upwards through the parent scopes
  for an identifier with the same name and context. If no match is found, we add
  a new entry to the current Env and annotate the identifier in the AST with its
  type information. Otherwise, we mark the identifier as a duplicate in the AST.

  Whenever an identifer is used (not declared), we perform the same lookup
  to check if an identifer is already in scope. If it is, we add the retrieved
  type info to the identifier in the AST. Otherwise, we mark it as not in
  scope. -}

module Semantics.Annotators.AST (annotateAST) where

import Control.Monad.State.Strict (runState, runStateT, get)
import qualified Data.Map as Map

{- LOCAL IMPORTS -}
import Semantics.Annotators.Class (annotateClass, addClassDeclToST)
import Semantics.Annotators.Function (annotateFunc, addFuncDeclToST)
import Semantics.Annotators.Statement
import Semantics.Annotators.Util
import Semantics.ErrorMessages
import Utilities.Definitions
import Debug.Trace

-- POST: Traverses the AST anontating it and performing lexical scoping.
--       Returns the annotated AST and the list of valid types.
annotateAST :: AST -> AST
annotateAST (Program cs fs main)
  = runScopeAnalysis annotateProgram
  where
    annotateProgram = do
      -- populating global scope with the classes
      newCs  <- mapM addClassDeclToST cs
      newFs  <- mapM addFuncDeclToST fs
      -- populating global scope with the functions
      newCs' <- mapM annotateClass newCs
      newFs' <- mapM annotateFunc newFs
      inChildScopeAndWrap (Program newCs' newFs') (annotateStat main)
