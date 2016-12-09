{-
  This module defines a parser for classes and all the components
  of a class
-}
module Parser.Class where

import Control.Applicative       (Alternative (..))

{- LOCAL IMPORTS -}
import Utilities.Definitions
import Parser.BasicCombinators
import Parser.Expression (parseExpr)
import Parser.Identifier (identifier)
import Parser.Type (parseType)
import Parser.Function (parseFunction, parseParamList)
import Parser.LexicalResolver
import Parser.Statement (parseStatement, checkNoReturnStat)

parseClass :: Parser Char Class
parseClass = do
  pos         <- getPosition
  keyword "class"
  className   <- require identifier "Class has not been given valid name"
  require (keyword "is") "Missing is keyword in class declaration"
  fields      <- many $ parseField <* require (punctuation ';') "Missing \';\' denoting end of field declaration"
  constructor <- require parseConstructor ("Class " ++ pretty className ++ " has no constructor. \nPlease provide an \n    init(<param-list>) is \n  <stat>\n    end.")
  methods     <- many parseFunction
  require (keyword "end") "Missing keyword end in class declaration"
  return $ Class className fields constructor methods pos

parseField :: Parser Char Field
parseField = do
  pos        <- getPosition
  typeField  <- parseType
  name       <- identifier
  return $ Field typeField name pos

parseConstructor :: Parser Char Constructor
parseConstructor = do
  pos      <- getPosition
  keyword "init"
  require (char '(') "Missing opening bracket in parameter list"
  params   <- require parseParamList "Invalid paramter list in constructor"
  require (char ')') "Missing closing bracket in parameter list"
  require (keyword "is") "Missing \'is\' keyword denoting start of constructor body"
  body     <- require parseStatement "Invalid constructor body"
  require (keyword "end") "Missing \'end\' keyword denoting end of constructor body"
  checkNoReturnStat body
  return $ Constructor params body pos
