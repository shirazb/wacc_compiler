module Semantics.Annotators.Function (
  annotateFunc
) where

import Semantics.ErrorMsgs
import Utilities.Definitions

annotateFunc :: Func -> LexicalScoper Func
annotateFunc
  = return
