{-
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

  Each new scope corresponds to its own SymbolTable, meaning it has its own Env.
  The inner SymbolTable will point to its direct parent scope, which in turn
  will point to its parent scope, and so on. Thus, the None constructor is
  required to signify when we have reached the global scope that has no parent
  scope.

  With this design, before entering a new scope, we must get the current
  symbol table <st> and use a new symbol table that has an empty map and points
  up to <st>.

  Only declaration statements and function declarations introduce new
  identifiers to the symbol table. To check if these are in scope, we check the
  current Env and, if required, look upwards through the parent scopes
  for an already in scope identifier of the same name and context. If
  none is found, we add a new entry to the current Env and annotate the
  identifier in the AST with its type info. Otherwise, we mark the identifier
  as a duplicate in the AST.

  Whenever an identifer is used (not declared), we perform the same lookup
  to check if an identifer is already in scope. If it is, we add the retrieved
  type info to the identifier in the AST. Otherwise, we mark
  it as not in scope.
-}

module Semantics.Annotators.AST (
  annotateAST
) where

import Control.Monad.State.Strict (runState)
import qualified Data.Map as Map

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
      idents <- mapM addFuncDeclToST fs
      let newFs = zipWith replaceIdent idents fs
      newFs' <- mapM annotateFunc newFs
      inChildScopeAndWrap (Program newFs') (annotateStat main)

replaceIdent :: Ident -> Func -> Func
replaceIdent i (Func t _ pl st pos)
  = Func t i pl st pos
