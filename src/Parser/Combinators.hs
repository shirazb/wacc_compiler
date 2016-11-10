{-
  Basic parser combinator definitions which are used to 
  build larger and more complex parsers.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser.Combinators where

import Control.Applicative       (Alternative (..))
import Control.Monad             (MonadPlus (..), guard)
import Control.Monad.Except      (MonadError (..))
import Data.Maybe                (fromJust)
import Control.Monad.State       (MonadState (..), StateT (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))

import Utilities.Definitions

{- Parser type declarations -}
newtype Parser t a
  = Parser { parse :: StateT [t] (StateT Position (MaybeT (Either Err))) a }
  deriving ( Monad
           , Applicative
           , Functor
           , MonadState [t]
           , Alternative
           , MonadError Err
           , MonadPlus
           )
-- POST: Executes a parser over a given input stream.
runParser :: Parser t a -> [t] -> Either Err (Maybe((a,[t]), Position))
runParser p inputString
 = runMaybeT $ runStateT (runStateT (parse p) inputString) (0, 0)

{- Position Utility Functions -}

getPosition :: Parser a Position
getPosition
  = Parser $ lift get

putPosition :: Position -> Parser a ()
putPosition
  = Parser . lift . put

updatePosition :: Char -> Parser Char ()
updatePosition c
  = getPosition >>= (putPosition . updateParserPosition c)

updateParserPosition :: Char -> Position -> Position
updateParserPosition '\n' (ln, c)
  = (ln + 1, 1)
updateParserPosition _ (ln, c)
  = (ln, c + 1)

updateRowPosition :: Position -> Position
updateRowPosition (ln, c)
  = (ln + 1, c)

{- GENERIC PREDICATE COMBINATORS: -}

-- POST: Consumes the first character if the input string is non-empty, fails
--       otherwise (denoted by Nothing). Updates position appropriately.
item :: Parser Char Char
item
  = do
    state <- get
    case state of
      (c:cs) -> do {put cs; updatePosition c; return c}
      []     -> failParser

-- POST: A parser which always fails
failParser :: MonadPlus m => m a
failParser = mzero

-- POST: Generates error and terminates execution if parser fails.
require :: Parser Char a -> String -> Parser Char a
require parser errorMessage = do
  p <- getPosition
  parser <|> throwError ("Syntax Error: " ++ errorMessage, updateRowPosition p)

-- POST: Consumes a single character if it satisfies the predicate, fails
--       otherwise (denoted by Nothing)
satisfy  :: (Char -> Bool) -> Parser Char Char
satisfy p
  = item >>= \char -> if p char then return char else failParser

-- POST: Does nothing if a single character satisfies the predicate, fails
--       otherwise. No input string is consumed.
check :: (a -> Bool) -> Parser a ()
check predicate = do
  inputString <- get
  case inputString of
    inp@(x:xs) -> do {guard (predicate x); put inp;}
    _          -> failParser

{- BASIC ATOMIC COMBINATORS: -}

-- POST: Parses a single specific character.
char  :: Char -> Parser Char Char
char c
  = satisfy (c ==)

-- POST: Parses single digit.
digit :: Parser Char Char
digit
  = satisfy (\x -> '0' <= x && x <= '9')

-- POST: Parses single lower-case letter.
lower :: Parser Char Char
lower
  = satisfy (\x -> 'a' <= x && x <= 'z')

-- POST: Parses single upper-case letter.
upper :: Parser Char Char
upper
  = satisfy (\x -> 'A' <= x && x <= 'Z')

-- POST: Parses single letter.
letter :: Parser Char Char
letter
  = lower <|> upper

-- POST: Parses single alpha-numeric character.
alphanum :: Parser Char Char
alphanum
  = letter <|> digit

-- POST: Parses any single character, including escape characters.
character :: Parser Char Char
character
  = satisfy ( `notElem` ['\\', '\"', '\'']) <|> escapeChar

-- POST: Parses single escape character.
escapeChar :: Parser Char Char
escapeChar = do
  char '\\'
  require (check (`elem` map fst escapeCharList))
                   "Invalid Escape Character"
  escaped_char <- item
  return $ fromJust $ lookup escaped_char escapeCharList

{- PARSERS FOR SEQUENCES: -}

-- POST: Parses either whole string or fails.
string :: String -> Parser Char String
string []
  = return []
string (x:xs) = do
  char x
  string xs
  return (x : xs)

-- POST:    Parses zero or more occurences of p seperated by sep. Returns
--          parsed items as a list.
-- EXAMPLE: sepby intLiteral char x "1,2,3" returns [1, 2, 3].
sepby :: Parser Char a -> Parser Char b -> Parser Char [a]
sepby p sep
  = sepby' p sep <|> return []

-- POST: Parses one or more occurences of p seperated by sep. Returns
--       parsed items as a list.
sepby' :: Parser Char a -> Parser Char b -> Parser Char [a]
sepby' p sep = do
  x  <- p
  xs <- many (sep >> p)
  return (x : xs)

-- POST:    Parses one occurence of p, removing opening and closing
--          delimiters. Expects no whitespace. Returns result of parsing p.
-- EXAMPLE: Remove brackets and parse the contents inside:
--          bracketNoWS (char '(') intLiteral (char ')') "(1)" will return 1.
bracketNoWS :: Parser Char a -> Parser Char b -> Parser Char c -> Parser Char b
bracketNoWS open p close = do
  open
  x <- p
  close
  return x
