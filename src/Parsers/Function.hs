---
-- Function Parsing.
---
module Parsers.Function (parseFunction) where

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
  string "is"
  funcBody   <- parseStatement
  string "end"
  return $ Func returnType name paramList funcBody

parseParamList :: Parser ParamList
parseParamList
  = ParamList <$> sepby' parseParam (char ',')

parseParam :: Parser Param
parseParam
  = liftM2 Param parseType identifier
