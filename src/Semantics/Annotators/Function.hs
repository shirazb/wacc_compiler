module Semantics.Annotators.Function (
  annotateFunc,
  addFuncDeclToST
) where

import Control.Monad.State.Strict
import qualified Data.Map as Map
import Debug.Trace
import Semantics.ErrorMsgs
import Utilities.Definitions
import Semantics.Annotators.Identifier
import Semantics.Annotators.Statement
import Semantics.Annotators.Util

addFuncDeclToST :: Func -> LexicalScoper ()
addFuncDeclToST (Func t ident paramList body pos)
  = void $ annotateNewIdent ident (Info t Function)

-- PRE: Ident already annotated
-- TODO: Refactor to make look nicer, try use Util.inChildScope(AndWrap)
annotateFunc :: Func -> LexicalScoper Func
annotateFunc (Func t ident paramList body pos) = do
  globalST     <- get

  -- Enter new function scope
  put (ST globalST Map.empty)

  -- Annotate the function
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
