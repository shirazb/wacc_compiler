module Parser.Function  where

import Control.Monad
import Control.Monad.Except

import Parser.Lexer
import Parser.Statement
import Parser.Type
import Parser.Combinators
import Utilities.Definitions

parseFunctionBody :: Parser Char Stat
parseFunctionBody = do
  funcBody  <- parseStatement
  checkExecPathEnds funcBody
  return funcBody

{-
  Should check patterns (Seq (Return _) _) and (Seq (Exit _) _), throwing an
  error; unreachable statements are not permitted.
-}
checkExecPathEnds :: Stat -> Parser Char Stat
checkExecPathEnds s@Return{}
 = return s
checkExecPathEnds s@Exit{}
 = return s
checkExecPathEnds (If _ s1 s2 _)
 = checkExecPathEnds s1 >> checkExecPathEnds s2
checkExecPathEnds (While _ s1 _)
 = checkExecPathEnds s1
checkExecPathEnds (Block s1 _)
 = checkExecPathEnds s1
checkExecPathEnds (Seq Return{} _ _) = do
   pos <- getPosition
   throwError ("Unreachable statement after return", pos)
checkExecPathEnds (Seq s1 s2 _)
 = checkExecPathEnds s2
checkExecPathEnds _ = do
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
  let paramTypes = map getTypeOfParam (getListOfParams paramList)
  let functionType = FuncT returnType paramTypes
  return $ Func functionType name paramList funcBody pos

getTypeOfParam :: Param -> Type
getTypeOfParam (Param t _ _)
  = t

getListOfParams :: ParamList -> [Param]
getListOfParams (ParamList list _)
  = list

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
