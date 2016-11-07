{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeFamilies               #-}

module Parser.Function  where

import Control.Monad
import Control.Monad.Except

import Parser.Lexer
import Parser.Statement
import Parser.Type
import Parser.Combinators
import Utilities.Declarations
import Utilities.Def2

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

-- POST: Parses a function defintion.
parseFunction :: Parser Char Func
parseFunction = do
  returnType   <- parseType
  name         <- identifier
  paramList    <- bracket (punctuation '(') parseParamList (tryParser (punctuation ')') "Invalid parameter list")
  tryParser (keyword "is") "Missing 'is' keyword"
  funcBody     <- tryParser parseFunctionBody "Invalid function body"
  tryParser (keyword "end") "Missing 'end' keyword"
  pos          <- getPosition
  return $ Func returnType name paramList funcBody pos

-- POST: Attempts to parse a list of parameters if there is one.
parseParamList :: Parser Char ParamList
parseParamList
  = ParamList <$> sepby parseParam (punctuation ',') <*> getPosition

-- PRE: None
-- POST: Parses a parameter
-- Example Usage: parse parseParam "intname" will return Param Int "name". Note whitespace is not accounted for here.
parseParam :: Parser Char Param
parseParam
  = liftM3 Param parseType identifier getPosition
