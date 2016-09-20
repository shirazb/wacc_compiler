module Utility.BasicCombinators where

import           Control.Applicative
import           Control.Monad
import           Data.Maybe

{- LOCAL IMPORTS -}

import           Utility.Declarations

{- GENERIC PREDICATE COMBINATORS -}

-- PRE:  None
-- POST: Consumes the first character if the input string is non-empty, fails
-- otherwise (denoted by empty string)
item :: Parser Char
item  = Parser $ \s -> case s of
                       []   -> []
                       (x:xs) -> [(x, xs)]

-- PRE:  None
-- POST: Consumes a single character if it satisfies the predicate, fails
--       otherwise (denoted by empty string)
satisfy  :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \x -> if p x then return x else mzero

-- PRE: None
-- POST: Does nothing if a single character satisfies the predicate, fails otherwise
check :: (Char -> Bool) -> Parser ()
check predicate
  = Parser check'
  where
    check' []          = []
    check' inp@(c : _) = [ ((), inp) | predicate c ]

{- BASIC ATOMIC COMBINATORS -}

-- PRE:  None
-- POST: Calls 'satisfy' with a predicate for a specific character
char  :: Char -> Parser Char
char c = satisfy (c ==)

-- PRE:  None
-- POST: Calls 'satisfy' with a predicate to determine if it's an element of
--       the list
oneOf  :: String -> Parser Char
oneOf s = satisfy (`elem` s)

-- PRE:  None
-- POST: Calls 'satisfy' with a predicate for single digits
digit :: Parser Char
digit  = satisfy (\x -> '0' <= x && x <= '9')

-- PRE:  None
-- POST: Calls 'satisfy' with predicate for lower-case letters
lower :: Parser Char
lower  = satisfy (\x -> 'a' <= x && x <= 'z')

-- PRE:  None
-- POST: Calls 'satisfy' with predicate for upper-case letters
upper :: Parser Char
upper  = satisfy (\x -> 'A' <= x && x <= 'Z')

-- PRE:  None
-- POST: Parser for letters
letter :: Parser Char
letter  = lower <|> upper

-- PRE:  None
-- POST: Parser for alpha-numeric characters
alphanum :: Parser Char
alphanum  = letter <|> digit

-- PRE:  None
-- POST: Parser for all characters including escape chararacters
character :: Parser Char
character  = satisfy (\s -> s `notElem` ['\\', '\"', '\'']) <|> escapeChar

-- PRE:  None
-- POST: Parser for escape characters
escapeChar :: Parser Char
escapeChar  = do
              char '\\'
              escaped_char <- item
              return $ fromJust $ lookup escaped_char escapeCharAssoc
              where
              escapeCharAssoc =  [('b','\b'), ('n','\n'), ('f','\f'),
                                  ('r','\r'), ('t','\t'), ('\\','\\'),
                                  ('\"','\"'), ('\'','\''), ('0', '\0')]


{- PARSERS FOR SEQUENCES -}

-- PRE:  None
-- POST: Returns the input string if the given string is parsed successfully. Fails otherwise.
string :: String -> Parser String
string []     = return []
string (x:xs) = do
  char x
  string xs
  return (x:xs)

-- PRE:  None
-- POST: Parses zero or more occurences of p seperated by sep. Returns parsed items as a list.
-- Example usage: it can be used to parse an inputstring of the form "1,2,3" where the seperators have no special meaning.
sepby :: Parser a -> Parser b -> Parser [a]
sepby p sep = sepby' p sep <|> return []

-- PRE:  None
-- POST: Similar to sepby but it parses one or more occurences. It will fail if there is not at least one occurence of p.
sepby' :: Parser a -> Parser b -> Parser [a]
sepby' p sep = do
  x <- p
  xs <- many (sep >> p)
  return (x:xs)

-- PRE:  None
-- POST: It will parse one occurence of p, but first remove an opening delimiter and then after parsing p it will remove a closing delimiter. Returns the result of parsing p
-- Example Usage: It can be used to remove brackets and parse the contents inside. parse (char '(') intLiteral (char ')') "(1)" will return IntLiteral 1. It does not take in to
-- account any white space.
bracketNoWS :: Parser a -> Parser b -> Parser c -> Parser b
bracketNoWS open p close = do
  open
  x <- p
  close
  return x

-- PRE:  None
-- POST: Parses zero or more occurences of letters and returns the result as a string.
word :: Parser String
word  = many letter

