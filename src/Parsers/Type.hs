---
-- Type Parsing
---
module Parsers.Type (parseType) where

import Control.Applicative

import Parsers.Lexer
import Utility.BasicCombinators
import Utility.Declarations
import Utility.Definitions

parseType :: Parser Type
parseType
  =   ArrayT <$> parseArrayType
  <|> PairT  <$>  parsePairType
  <|> BaseT  <$> parseBaseType

parseBaseType :: Parser BaseType
parseBaseType
  = parseFromMap baseTypes

multiDimArray :: Parser (ArrayType -> Type)
multiDimArray
  = string "[]" >> rest ArrayT
  where
    rest x = (do
      string "[]"
      rest (ArrayT . x)) <|> return x

parseArrayType :: Parser ArrayType
parseArrayType = do
  t          <- (BaseT <$> parseBaseType) <|> (PairT <$> parsePairType)
  dimension  <- multiDimArray
  return (dimension t)

parsePairType :: Parser PairType
parsePairType = do
  string "pair"
  char '('
  t1 <- parsePairElemType
  char ','   -- TODO: FIX WHITESPACE AROUND COMMA
  t2 <- parsePairElemType
  char ')'
  return $ PairType t1 t2

parseNestedPairType :: Parser PairElemType
parseNestedPairType
  = string "pair" >> return Pair

parsePairElemType :: Parser PairElemType
parsePairElemType
  =   parseNestedPairType
  <|> ArrayP <$> parseArrayType
  <|> BaseP  <$> parseBaseType
