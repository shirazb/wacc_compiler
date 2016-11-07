-- DOESN'T WORK NOW -- WRONG INFO TYPE

module Semantics.Annotators.Test where

import Utilities.Definitions
import Semantics.Annotators.Statement
import Semantics.Annotators.Identifier
import Utilities.Declarations
import qualified Data.Map as Map
import Control.Monad.State.Strict

s1 = Declaration (BaseT BaseInt) (Ident "x" (Info (BaseT BaseInt) Variable (Just (IntLit 1)) NoError)) (ExprAssign (IntLit 1)) (0,0)
s2 = Declaration (BaseT BaseInt) (Ident "y" (Info (BaseT BaseInt) Variable (Just (IntLit 1)) NoError)) (ExprAssign (IntLit 1)) (0,0)
st   = ST None (Map.fromList [(("x", Function), (Info (BaseT BaseInt)) Function Nothing NoError)])

testDeclarationVarSameNameAsFunc
  = runState (annotateStat decl) st
  where
      decl = Declaration (BaseT BaseInt) (Ident "x" (Info (BaseT BaseInt) Variable (Just (IntLit 1)) NoError)) (ExprAssign (IntLit 1)) (0,0)
      st   = ST None (Map.fromList [(("x", Function), (Info (BaseT BaseInt)) Function Nothing NoError)])

testSeq
  = runState (annotateStat (Seq s1 s2)) st

testBind = runState (do {
    annotateIdent (Ident "x" (Info (BaseT BaseInt) Variable (Just (IntLit 1)) NoError));
    annotateIdent (Ident "y" (Info (BaseT BaseInt) Variable (Just (IntLit 1)) NoError)); })
    (ST None (Map.fromList [(("x", Function), (Info (BaseT BaseInt)) Function Nothing NoError)]))

testFindsDuplicateVar
  = runState (annotateStat s1) st
  where
    st = ST None (Map.fromList [(("x", Variable), (Info (BaseT BaseInt)) Variable Nothing NoError)])
