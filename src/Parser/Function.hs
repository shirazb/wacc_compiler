{- This module defines parser combinators for functions -}

module Parser.Function  where

import Control.Monad ( liftM3 )
import Control.Monad.Except ( throwError )

{- LOCAL IMPORTS -}
import Parser.Identifier ( identifier )
import Parser.Lexer
import Parser.Statement ( parseStatement )
import Parser.Type ( parseType )
import Parser.Combinators ( Parser, getPosition, require, sepby )
import Utilities.Definitions

-- POST: Parses a function defintion
parseFunction :: Parser Char Func
parseFunction = do
  returnType   <- parseType
  name         <- identifier
  paramList    <- bracket (punctuation '(') parseParamList (require
                    (punctuation ')') "Invalid parameter list")
  require (keyword "is") "Missing 'is' keyword"
  funcBody     <- parseFunctionBody
  require (keyword "end") "Missing 'end' keyword"
  pos          <- getPosition
  let parameterTypes = map parameterType (parameters paramList)
  let functionType = FuncT returnType parameterTypes
  return $ Func functionType name paramList funcBody pos
  where
    parameterType (Param t _ _) = t
    parameters (ParamList list _) = list

-- POST: Parses a function body
parseFunctionBody :: Parser Char Stat
parseFunctionBody = do
  body  <- parseStatement
  checkExecutionPath body
  return body

{- HELPER FUNCTIONS -}

-- POST: Produces an error if an execution path does not end with exit or 
--       return, or if there are trailing statements after an exit or return 
--       statement. Checks for patterns (Seq Return{} _ _) and (Seq Exit{} _ _)
checkExecutionPath :: Stat -> Parser Char ()

checkExecutionPath s@Return{}
  = return ()

checkExecutionPath s@Exit{}
  = return ()

checkExecutionPath (If _ s1 s2 _)
  = checkExecutionPath s1 >> checkExecutionPath s2

checkExecutionPath (While _ s1 _)
  = checkExecutionPath s1

checkExecutionPath (Block s1 _)
  = checkExecutionPath s1

checkExecutionPath (Seq Return{} _ _) = do
  pos <- getPosition
  throwError ("Syntax Error: Unreachable statement after return", pos)

checkExecutionPath (Seq Exit{} _ _) = do
  pos <- getPosition
  throwError ("Syntax Error: Unreachable statement after exit", pos)

checkExecutionPath (Seq s1 s2 _)
  = checkExecutionPath s2

checkExecutionPath _ = do
  pos <- getPosition
  throwError 
    ("Syntax Error: Mising return or exit statement in function" ++
     " body ending at: ", pos)

-- POST: Parses comma-delimited list of parameters
parseParamList :: Parser Char ParamList
parseParamList
  = ParamList <$> sepby parseParam (punctuation ',') <*> getPosition

-- POST:    Parses a single parameter
-- EXAMPLE: (parse parseParam "int name") produces (Param Int "name")
parseParam :: Parser Char Param
parseParam
  = liftM3 Param parseType identifier getPosition
