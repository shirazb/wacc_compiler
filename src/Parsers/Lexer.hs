---
-- Lexical Issues
---
module Parsers.Lexer {-(export list?)-} where

import Control.Applicative
import Control.Monad

import Utility.BasicCombinators
import Utility.Declarations
import Utility.Definitions

comments :: Parser ()
comments
  = void $ char '#' >> many (satisfy (/= '\n')) >> char '\n'

spacesAndComments :: Parser ()
spacesAndComments
  = void $ many (spaces <|> comments)

leadWSC :: Parser a -> Parser a
leadWSC p
  = spacesAndComments >> p

token :: Parser a -> Parser a
token p = do
  parsedValue <- p
  spacesAndComments
  return parsedValue

keyword :: String -> Parser String
keyword xs
  = token (string xs)

identifiers :: Parser String
identifiers
  = token identifier

ident :: Parser String
ident
  = liftA2 (:) (char '_' <|> letter) (many (alphanum <|> char '_'))

identifier :: Parser String
identifier = do
  x <- ident
  guard (x `notElem` keywords)
  return x
