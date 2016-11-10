{-
  This module defines a number of combinators which are used to parse the types
  in the wacc language. Refer to the BNF spec of the wacc language for all the
  types.
-}

module Parser.Type (parseType) where

import Control.Applicative (Alternative (..))
import Data.Maybe          (fromJust)

import Parser.Combinators
import Parser.Identifier
import Parser.Lexer
import Utilities.Definitions

--POST: Parser of types for the WACC language, built up using the more basic
--      parsers of types.
parseType :: Parser Char Type
parseType
  =   parseArrayType
  <|> parsePairType
  <|> parseBaseType

-- Parses the base types of WACC.
parseBaseType :: Parser Char Type
parseBaseType = do
  baseTypeString <- foldr1 (<|>) baseTypeKeywords
  return $ BaseT (fromJust (lookup baseTypeString baseTypes))
  where
    baseTypeKeywords = map (keyword . fst) baseTypes

-- Parses the '[]'s in an array type to get the dimension of the array.
arrayDim :: Parser Char Int
arrayDim
  = token "[]" >> rest 1
  where
    rest x = (do
      token "[]"
      rest (x + 1)) <|> return x

-- Parses an array type. The innermost type of an array cannot be an array type.
parseArrayType :: Parser Char Type
parseArrayType = do
  t         <- parseBaseType <|> parsePairType
  dimension <- arrayDim
  return $ ArrayT dimension t

-- Parses a nested pair type. These have no type information other than that
-- they are a pair.
parseNestedPairType :: Parser Char Type
parseNestedPairType
    = token "pair" >> return Pair

-- Parses a pair type.
parsePairType :: Parser Char Type
parsePairType = trimWS $ do
  token "pair"
  require (punctuation '(') "Missing opening parenthesis in pair-type"
  t1 <- require parseInnerPairType "Invalid first pair-type"
  require (punctuation ',') "Missing comma in pair-type declaration"
  t2 <- require parseInnerPairType "Invalid second pair-type"
  require (punctuation ')') "Missing closing parenthesis in pair-type"
  return $ PairT t1 t2

-- Parses types that are allowed to be within a pair. This does not include
-- types of the form pair(t, t').
parseInnerPairType :: Parser Char Type
parseInnerPairType
  = parseNestedPairType <|> parseArrayType <|> parseBaseType
