{-
This module defines a number of combinators which are used to parse the types in the wacc language. Refer to the BNF spec of the wacc language for all the types.
-}
module Parsers.Type (parseType) where

import           Control.Applicative

import           Parsers.Lexer
import           Utility.BasicCombinators
import           Utility.Declarations
import           Utility.Definitions

--PRE: None
--POST: Parser of types for the WACC language, built up using the more basic parsers of types.
-- Returns type wrapped in appropriate data constructor.
parseType :: Parser Char Type
parseType
  =   parseArrayType
  <|> PairT  <$>  parsePairType
  <|> BaseT  <$> parseBaseType


parseBaseType :: Parser Char BaseType
parseBaseType
  = parseFromMap baseTypes


multiDimArray :: Parser Char (ArrayType -> Type)
multiDimArray
  = token "[]" >> rest ArrayT
  where
    rest x = (do
      token "[]"
      rest (ArrayT . x)) <|> return x


parseArrayType :: Parser Char ArrayType
parseArrayType = do
  t          <- (BaseT <$> parseBaseType) <|> (PairT <$> parsePairType)
  dimension  <- multiDimArray
  return (dimension t)

parsePairType :: Parser Char PairType
parsePairType = trimWS $ do
  token "pair"
  locationReporter (punctuation '(') "Missing opening parenthesis in pair-type"
  t1 <- locationReporter parsePairElemType "Invalid first pair-type"
  locationReporter (punctuation ',') "Missing comma in pair-type declaration"
  t2 <- locationReporter parsePairElemType "Invalid second pair-type"
  locationReporter (punctuation ')') "Missing closing parenthesis in pair-type"
  return $ PairType t1 t2

parseNestedPairType :: Parser Char PairElemType
parseNestedPairType
  = token "pair" >> return Pair

parsePairElemType :: Parser Char PairElemType
parsePairElemType
  =   parseNestedPairType
  <|> ArrayP <$> parseArrayType
  <|> BaseP  <$> parseBaseType
