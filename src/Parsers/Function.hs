---
-- Function Parsing.
---
module Parsers.Function  where

import           Control.Monad

import           Debug.Trace
import           Parsers.Lexer
import           Parsers.Statement
import           Parsers.Type
import           Utility.BasicCombinators
import           Utility.Declarations
import           Utility.Definitions

-- PRE: None
-- POST: Parses a function defintion.
parseFunction :: Parser Char Func
parseFunction = do
  returnType <- parseType
  name <- identifier
  paramList <- bracket (punctuation '(') (locationReporter parseParamList "Invalid function parameter list") (locationReporter (punctuation ')') "Missing closing parenthesis to function param list")
  locationReporter (keyword "is") "Missing 'is' keyword"
  funcBody <- locationReporter parseStatement "Invalid function body"
  locationReporter (keyword "end") "Missing 'end' keyword"
  return $ Func returnType name paramList funcBody

-- PRE: None
-- POST: Attempts to parse a list of parameters if there is one.
parseParamList :: Parser Char ParamList
parseParamList
  = ParamList <$> sepby parseParam (punctuation ',')

-- PRE: None
-- POST: Parses a parameter
-- Example Usage: parse parseParam "intname" will return Param Int "name". Note whitespace is not accounted for here.
parseParam :: Parser Char Param
parseParam
  = liftM2 Param parseType identifier
