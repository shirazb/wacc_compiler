{- 
Parser combinators for functions.
-}
module Parser.Function  where

import Control.Monad ( liftM3 )
import Control.Monad.Except ( throwError )

import Parser.Identifier ( identifier )
import Parser.Lexer
import Parser.Statement ( parseStatement )
import Parser.Type ( parseType )
import Parser.Combinators ( Parser, getPosition, require, sepby )
import Utilities.Definitions

parseFunctionBody :: Parser Char Stat
parseFunctionBody = do
  body  <- parseStatement
  checkExecutionPath body
  return body

{-
  Should check patterns (Seq (Return _) _) and (Seq (Exit _) _), throwing an
  error; unreachable statements are not permitted.
-}
checkExecutionPath :: Stat -> Parser Char Stat
checkExecutionPath s@Return{}
 = return s
checkExecutionPath s@Exit{}
 = return s
checkExecutionPath (If _ s1 s2 _)
 = checkExecutionPath s1 >> checkExecutionPath s2
checkExecutionPath (While _ s1 _)
 = checkExecutionPath s1
checkExecutionPath (Block s1 _)
 = checkExecutionPath s1
checkExecutionPath (Seq Return{} _ _) = do
   pos <- getPosition
   throwError ("Unreachable statement after return", pos)
checkExecutionPath (Seq s1 s2 _)
 = checkExecutionPath s2
checkExecutionPath _ = do
  pos <- getPosition
  throwError ("Mising return or exit statement in function body ending at: ",
    pos)

-- POST: Parses a function defintion.
parseFunction :: Parser Char Func
parseFunction = do
  returnType   <- parseType
  name         <- identifier
  paramList    <- bracket (punctuation '(') parseParamList (require
                    (punctuation ')') "Invalid parameter list")
  require (keyword "is") "Missing 'is' keyword"
  funcBody     <- require parseFunctionBody "Invalid function body"
  require (keyword "end") "Missing 'end' keyword"
  pos          <- getPosition
  let parameterTypes = map parameterType (parameters paramList)
  let functionType = FuncT returnType parameterTypes
  return $ Func functionType name paramList funcBody pos

parameterType :: Param -> Type
parameterType (Param t _ _)
  = t

parameters :: ParamList -> [Param]
parameters (ParamList list _) = list

-- POST: Attempts to parse a list of parameters if there is one.
parseParamList :: Parser Char ParamList
parseParamList
  = ParamList <$> sepby parseParam (punctuation ',') <*> getPosition

-- POST:    Parses a parameter
-- EXAMPLE: parse parseParam "intname" will return
--          Param Int "name"
--          Note whitespace is not accounted for here.
parseParam :: Parser Char Param
parseParam
  = liftM3 Param parseType identifier getPosition
