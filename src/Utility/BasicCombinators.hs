module Utility.BasicCombinators where

import           Control.Applicative
import           Control.Monad
import           Data.Maybe

import           Utility.Definitions
import           Utility.Declarations

-- Consumes the first character if the input string is non-empty, fails
-- otherwise
item :: Parser Char
item = Parser $ \inp -> case inp of
                        [] -> []
                        x:xs -> [(x, xs)]

-- Consumes a single character if it satisfies the predicate, fails otherwise
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \x -> if p x then return x else mzero

-- Parser for specific characters
char :: Char -> Parser Char
char c = satisfy (c ==)

oneOf :: String -> Parser Char
oneOf s = satisfy ( `elem` s)

-- Parser for specific single digits
digit :: Parser Char
digit = satisfy (\x -> '0' <= x && x <= '9')

-- Parser for specific lower-case letters
lower :: Parser Char
lower = satisfy (\x -> 'a' <= x && x <= 'z')

-- Parser for specific upper-case letters
upper :: Parser Char
upper = satisfy (\x -> 'A' <= x && x <= 'Z')

-- Parser for letters
letter :: Parser Char
letter =  lower <|> upper

-- Parser for alpha-numeric characters
alphanum :: Parser Char
alphanum = letter <|> digit

-- parser for all characters & escape chars
character :: Parser Char
character = satisfy (\s -> s `notElem` ['\\', '\"', '\'']) <|> escapeChar

escapeChar :: Parser Char
escapeChar = do
  char '\\'
  escaped_char <- item
  return $ fromJust $ lookup escaped_char escapeCharAssoc
  where
    escapeCharAssoc = [('b','\b'), ('n','\n'), ('f','\f')
                      , ('r','\r'), ('t','\t'), ('\\','\\')
                      , ('\"','\"'), ('\'','\''), ('0', '\0')]


-- Parser for words
word :: Parser String
word = many letter

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  char x
  string xs
  return (x:xs)

-- generic combinators
sepby1 :: Parser a -> Parser b -> Parser [a]
sepby1 p sep = do
    x <- p
    xs <- many (do {sep; p;})
    return (x:xs)

sepby :: Parser a -> Parser b -> Parser [a]
sepby p sep = sepby1 p sep <|> return []

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do
  open
  x <- p
  close
  return x
