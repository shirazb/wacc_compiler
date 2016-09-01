module BasicCombinators where

import Control.Applicative
import Control.Monad
import Data.Maybe

{- LOCAL IMPORTS -}

import Declarations

{- GENERIC PREDICATE COMBINATORS -}

-- PRE:  None
-- POST: Consumes the first character if the input string is non-empty, fails 
-- otherwise (denoted by empty string)
item :: Parser Char 
item  = Parser $ \s -> case s of 
                         []   -> []
			 x:xs -> [(x, xs)]

-- PRE:  None
-- POST: Consumes a single character if it satisfies the predicate, fails 
--       otherwise (denoted by empty string)
satisfy  :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \x -> if p x then return x else mzero

-- PRE:  ?
-- POST: ?
sepby      :: Parser a -> Parser b -> Parser [a]
sepby p sep = sepby' p sep <|> return []

-- PRE:  ?
-- POST: ?
sepby'      :: Parser a -> Parser b -> Parser [a]
sepby' p sep = do
  x <- p
  xs <- many $ do { sep ; p }
  return (x:xs)

-- PRE:  ?
-- POST: ?
bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do
  open
  x <- p
  close
  return x

{- SPECIFIC PREDICATE COMBINATORS -}

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
-- POST: Parser for all characters and escape chararacters
character :: Parser Char
character  = satisfy (\s -> s `notElem` ['\\', '\"', '\'']) <|> escapeChar
           where
	     -- PRE:  None
	     -- POST: Parser for escape characters
             escapeChar :: Parser Char
	     escapeChar  = do
               char '\\'
               escaped_char <- item
               return $ fromJust $ lookup escaped_char escapeCharAssoc
            
             escapeCharAssoc = [('b','\b'), ('n','\n'), ('f','\f'),
	                        ('r','\r'), ('t','\t'), ('\\','\\'),
			        ('\"','\"'), ('\'','\''), ('0', '\0')]

-- PRE:  None
-- POST: Parser for words
word :: Parser String
word  = many letter

-- PRE:  None
-- POST: ?
string :: String -> Parser String
string []     = return []
string (x:xs) = do
  char x
  string xs
  return (x:xs)
