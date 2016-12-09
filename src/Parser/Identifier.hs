{- This module defines a parser combinator for identifiers -}

module Parser.Identifier (identifier, ident) where

import Control.Applicative (Alternative (..))
import Control.Monad       (liftM2, guard)

{- LOCAL IMPORTS -}
import Parser.BasicCombinators
import Parser.LexicalResolver
import Utilities.Definitions

-- POST: Parses strings that are permitted to be identifiers
ident :: Parser Char String
ident
  = liftM2 (:) (char '_' <|> letter) (many (alphanum <|> char '_'))

-- POST: Parses identifiers, removing surround whitespace and checking that it
--       is not a keyword
identifier :: Parser Char Ident
identifier = trimWS (do
    name <- ident
    guard (name `notElem` keywords)
    return $ Ident name NoInfo)
  <|> parseSelf

-- POST: Parses the 'self' keyword
parseSelf :: Parser Char Ident
parseSelf
  = token "self" >> return (Self NoInfo)
