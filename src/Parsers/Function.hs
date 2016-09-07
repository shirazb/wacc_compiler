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

parseFunction :: Parser Func
parseFunction = do
  returnType <- parseType
  name       <- identifier
  paramList  <- bracket (char '(') parseParamList (char ')')
  keyword "is"
  funcBody   <- parseStatement
  keyword "end"
  return $ Func returnType name paramList funcBody

parseParamList :: Parser ParamList
parseParamList
  = ParamList <$> sepby' parseParam (punctuation ',')

parseParam :: Parser Param
parseParam
  = liftM2 Param parseType identifier
