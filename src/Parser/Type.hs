{-
This module defines a number of combinators which are used to parse the types
in the wacc language. Refer to the BNF spec of the wacc language for all the
types.
-}

module Parser.Type (parseType) where

import Control.Monad
import Control.Applicative
import Data.Maybe

import Parser.Identifier
import Parser.Lexer
import Parser.Combinators
import Utilities.Definitions

--POST: Parser of types for the WACC language, built up using the more basic
--      parsers of types. Returns type wrapped in appropriate data constructor.
parseType :: Parser Char Type
parseType
  =   parseArrayType
  <|> parsePairType
  <|> parseBaseType

parseBaseType :: Parser Char Type
parseBaseType = do
  baseTypeString <- foldr1 (<|>) (map (keyword . fst) baseTypes)
  return $ BaseT (fromJust (lookup baseTypeString baseTypes))

multiDimArray :: Parser Char Int
multiDimArray
  = token "[]" >> rest 1
  where
    rest x = (do
      token "[]"
      rest (x + 1)) <|> return x

parseArrayType :: Parser Char Type
parseArrayType = do
  t         <-  parseBaseType <|> parsePairType
  dimension <- multiDimArray
  return $ ArrayT dimension t

parseNestedPairType :: Parser Char Type
parseNestedPairType
    = token "pair" >> return Pair

parsePairType :: Parser Char Type
parsePairType = trimWS $ do
  token "pair"
  require (punctuation '(') "Missing opening parenthesis in pair-type"
  t1 <- require parseInnerPairType "Invalid first pair-type"
  require (punctuation ',') "Missing comma in pair-type declaration"
  t2 <- require parseInnerPairType "Invalid second pair-type"
  require (punctuation ')') "Missing closing parenthesis in pair-type"
  return $ PairT t1 t2

parseInnerPairType :: Parser Char Type
parseInnerPairType
  = parseNestedPairType <|> parseArrayType <|> parseBaseType
