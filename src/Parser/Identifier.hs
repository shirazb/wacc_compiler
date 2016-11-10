module Parser.Identifier (identifier) where

import Control.Monad        (liftM2, guard)
import Control.Applicative  (Alternative (..))
import Utilities.Definitions
import Parser.Combinators
import Parser.Lexer

ident :: Parser Char String
ident
  = liftM2 (:) (char '_' <|> letter) (many (alphanum <|> char '_'))

identifier :: Parser Char Ident
identifier = trimWS $ do
  name  <- ident
  guard (name `notElem` keywords)
  return $ Ident name NoInfo
