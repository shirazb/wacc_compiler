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


-- maybeParse p v = Parser $ \s -> case parse p s of
--                               res -> return res
--                               [] -> [([],s)]

-- we need to try define some sort maybe parser

parseFunction :: Parser Func
parseFunction = do
  traceM "  WE ARE PARSING THE FUNCTION  "
  returnType <- parseType
  traceM ("The returnType is: " ++ show returnType)
  name <- identifier
  traceM ("The name of the function: " ++ show name)
  paramList <- bracket (punctuation '(') parseParamList (punctuation ')')
  traceM ("The parameterList is: " ++ show paramList)
  keyword "is"
  traceM "We are parsing the keyword is "
  funcBody <- parseStatement
  traceM ("We are parsing the funcBody: " ++ show funcBody)
  keyword "end"
  traceM "We are parsing the keyword end "
  return $ Func returnType name paramList funcBody

-- are we accounting for the fact
-- that the parameter list could not be there
parseParamList :: Parser ParamList
parseParamList
  = ParamList <$> sepby parseParam (punctuation ',')

parseParam :: Parser Param
parseParam
  = liftM2 Param parseType identifier
