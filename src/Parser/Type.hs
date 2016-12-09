{- This module defines a number of parser combinators which are used to parse
types -}

module Parser.Type (parseType) where

import Control.Applicative (Alternative (..))
import Control.Monad       (guard)
import Data.Maybe          (fromJust)

{- LOCAL IMPORTS -}
import Parser.BasicCombinators
import Parser.Identifier (ident)
import Parser.LexicalResolver
import Utilities.Definitions

-- POST: Parses types
parseType :: Parser Char Type
parseType
  =   parseArrayType
  <|> parsePairType
  <|> parseBaseType
  <|> parseVoidType
  <|> parseClassIdentifer

-- Parse class names as types
parseClassIdentifer :: Parser Char Type
parseClassIdentifer = do
  identT <- ident
  guard (identT `notElem` keywords)
  return $ ClassT identT

-- POST: Parse Void type
parseVoidType :: Parser Char Type
parseVoidType = keyword "void" >> return Void

-- POST: Parses base types
parseBaseType :: Parser Char Type
parseBaseType = do
  baseTypeString <- foldr1 (<|>) baseTypeKeywords
  return $ BaseT (fromJust (lookup baseTypeString baseTypes))
  where
    baseTypeKeywords = map (keyword . fst) baseTypes

-- POST: Parses an array type
-- NOTE: The innermost type of an array cannot be an array type
parseArrayType :: Parser Char Type
parseArrayType = do
  t         <- parseBaseType <|> parsePairType
  dimension <- arrayDim
  return $ ArrayT dimension t

-- POST: Parses a nested pair type
-- NOTE: These have no type information other than that they are a pair
parseNestedPairType :: Parser Char Type
parseNestedPairType
  = token "pair" >> return Pair

-- POST: Parses a pair type
parsePairType :: Parser Char Type
parsePairType = trimWS $ do
  token "pair"
  require (punctuation '(') "Missing opening parenthesis in pair-type"
  t1 <- require parseInnerPairType "Invalid first pair-type"
  require (punctuation ',') "Missing comma in pair-type declaration"
  t2 <- require parseInnerPairType "Invalid second pair-type"
  require (punctuation ')') "Missing closing parenthesis in pair-type"
  return $ PairT t1 t2

{- HELPER FUNCTIONS -}

-- POST: Parses types that are allowed to be within a pair.
-- NOTE: This does not include types of the form pair(t, t').
parseInnerPairType :: Parser Char Type
parseInnerPairType
  = parseNestedPairType <|> parseArrayType <|> parseBaseType

-- POST: Parses the '[]'s in an array type to get the dimension of the array
arrayDim :: Parser Char Int
arrayDim
  = token "[]" >> rest 1
  where
    rest x = (do
      token "[]"
      rest (x + 1)) <|> return x
