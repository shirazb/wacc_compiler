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
  = token "[]" >> rest ArrayT
  where
    rest x = (do
      token "[]"
      rest (ArrayT . x)) <|> return x

parseArrayType :: Parser ArrayType
parseArrayType = do
  t          <- (BaseT <$> parseBaseType) <|> (PairT <$> parsePairType)
  dimension  <- multiDimArray
  return (dimension t)

parsePairType :: Parser PairType
parsePairType = trimWS $ do
  token "pair"
  punctuation '('
  t1 <- parsePairElemType
  punctuation ','
  t2 <- parsePairElemType
  punctuation ')'
  return $ PairType t1 t2

parseNestedPairType :: Parser PairElemType
parseNestedPairType
  = token "pair" >> return Pair

parsePairElemType :: Parser PairElemType
parsePairElemType
  =   parseNestedPairType
  <|> ArrayP <$> parseArrayType
  <|> BaseP  <$> parseBaseType
