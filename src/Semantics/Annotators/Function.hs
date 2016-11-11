{-
  Annotates function identifiers and variables within the body of functions.
-}
module Semantics.Annotators.Function (
  annotateFunc,
  addFuncDeclToST
) where

import Control.Monad.State.Strict (get, put)
import qualified Data.Map as Map

import Semantics.Annotators.Identifier
import Semantics.Annotators.Statement
import Semantics.Annotators.Util
import Semantics.ErrorMessages
import Utilities.Definitions

addFuncDeclToST :: Func -> LexicalScoper Ident
addFuncDeclToST (Func t ident paramList body pos)
  = annotateNewIdent ident (Info t Function)

-- PRE: Function name ident already annotated
-- POST: Annotates all the identifiers within the function body.
annotateFunc :: Func -> LexicalScoper Func
annotateFunc (Func t ident paramList body pos) = do
  globalST     <- get

  -- Enter new function scope
  put (ST globalST Map.empty)

  -- Annotate the function body
  newParamList <- annotateParamList paramList
  newBody      <- annotateStat body

  -- Exit function scope
  put globalST

  return $ Func t ident newParamList newBody pos

annotateParamList :: ParamList -> LexicalScoper ParamList
annotateParamList (ParamList ps pos)
  = ParamList <$> mapM annotateParam ps <*> return pos

annotateParam :: Param -> LexicalScoper Param
annotateParam (Param t ident pos)
  = Param t <$> annotateNewIdent ident (Info t Variable) <*> return pos
