module Semantics.Annotators.Function (
  annotateFunc
) where

import Control.Monad.State.Strict
import qualified Data.Map as Map
import Debug.Trace
import Semantics.ErrorMsgs
import Utilities.Def2
import Semantics.Annotators.Identifier
import Semantics.Annotators.Statement
import Semantics.Annotators.Util

-- TODO: Refactor to make look nicer, try use Util.inChildScope(AndWrap)
annotateFunc :: Func -> LexicalScoper Func
annotateFunc (Func t ident paramList body pos) = do
  globalST          <- get
  newIdent          <- annotateNewIdent ident (Info t Function)

  -- We will update the global st only if the function ident was valid
  let newGlobalST = if identHasError newIdent
      then globalST
      else addToST newIdent globalST

  -- Enter new function scope
  put (ST newGlobalST Map.empty)

  -- Annotate the function
  newParamList      <- annotateParamList paramList
  newBody           <- annotateStat body

  -- Exit function scope
  put newGlobalST

  return $ Func t newIdent newParamList newBody pos

annotateParamList :: ParamList -> LexicalScoper ParamList
annotateParamList (ParamList ps pos)
  = ParamList <$> mapM annotateParam ps <*> (return pos)

annotateParam :: Param -> LexicalScoper Param
annotateParam (Param t ident pos)
  = Param t <$> annotateNewIdent ident (Info t Variable) <*> (return pos)
