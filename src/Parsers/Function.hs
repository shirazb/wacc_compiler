---
-- Function Parsing.
---
module Parsers.Function (parseFunction) where

import Control.Monad

import Parsers.Lexer
import Parsers.Statement
import Parsers.Type
import Utility.BasicCombinators
import Utility.Declarations
import Utility.Definitions
import Debug.Trace

parseFunction :: Parser Func
parseFunction = do
  returnType <- parseType
  name <- identifier
  paramList <- bracket (punctuation '(') parseParamList (punctuation ')')
  keyword "is"
  funcBody <- parseStatement
  keyword "end"
  return $ Func returnType name paramList funcBody

-- are we accounting for the fact
-- that the parameter list could not be there
parseParamList :: Parser ParamList
parseParamList
  = ParamList <$> sepby parseParam (punctuation ',')

parseParam :: Parser Param
parseParam
  = liftM2 Param parseType identifier
