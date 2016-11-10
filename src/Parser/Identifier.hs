{-
  Parses WACC identifiers.
-}
module Parser.Identifier (identifier) where

import Control.Applicative  (Alternative (..))
import Control.Monad        (liftM2, guard)

import Parser.Combinators
import Parser.Lexer
import Utilities.Definitions

-- Parses strings that are permitted to be identifiers.
ident :: Parser Char String
ident
  = liftM2 (:) (char '_' <|> letter) (many (alphanum <|> char '_'))

-- Parses identifiers, removing surround whitespace and checking it is
-- not a keyword.
identifier :: Parser Char Ident
identifier = trimWS $ do
  name  <- ident
  guard (name `notElem` keywords)
  return $ Ident name NoInfo
