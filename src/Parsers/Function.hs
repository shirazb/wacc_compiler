{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeFamilies               #-}
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
import           Control.Monad.Except



parseFunctionBody :: Parser Char Stat
parseFunctionBody = do
  funcBody  <- parseStatement
  parseStatAndCheckExecPathEnds funcBody
  return funcBody

parseStatAndCheckExecPathEnds :: Stat -> Parser Char Stat
parseStatAndCheckExecPathEnds s@Return{}
 = return s
parseStatAndCheckExecPathEnds s@Exit{}
 = return s
parseStatAndCheckExecPathEnds (If _ s1 s2)
 = parseStatAndCheckExecPathEnds s1 >> parseStatAndCheckExecPathEnds s2
parseStatAndCheckExecPathEnds (While _ s1)
 = parseStatAndCheckExecPathEnds s1
parseStatAndCheckExecPathEnds (Block s1)
 = parseStatAndCheckExecPathEnds s1
parseStatAndCheckExecPathEnds (Seq s1 s2)
 = parseStatAndCheckExecPathEnds s2
parseStatAndCheckExecPathEnds _ = do
  pos <- getPosition
  throwError ("Mising return or exit statement in function body ending at: ", pos)

-- we can actually add error handling to all parts of parseFunction
-- because it will only get called if there are functions
-- so if it is going to fail then it is destined to fail
-- PRE: None
-- POST: Parses a function defintion.
parseFunction :: Parser Char Func
parseFunction = do
  returnType   <- parseType
  -- do we check for a space?
  name         <- identifier
  paramList    <- bracket (punctuation '(') parseParamList (locationReporter (punctuation ')') "Invalid parameter list")
  locationReporter (keyword "is") "Missing 'is' keyword"
  funcBody     <- locationReporter parseFunctionBody "Invalid function body"
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
