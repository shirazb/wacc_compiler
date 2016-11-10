{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DeriveFunctor              #-}

module Parser.Combinators where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Maybe
import Data.List                 (nub)
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

runParser :: Parser t a -> [t] -> Position ->
              Either Err (Maybe((a,[t]), Position))
runParser p inputString initialPos
 = runMaybeT $ runStateT (runStateT (parse p) inputString) initialPos

{- Position Utility Functions -}

getPosition :: Parser Char Position
getPosition
  = Parser $ lift get

putPosition :: Position -> Parser Char ()
putPosition
  = Parser . lift . put

updatePosition :: (Position -> Position) -> Parser Char ()
updatePosition f
  = getPosition >>= (putPosition . f)

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
--       otherwise (denoted by Nothing).
parseChar :: (MonadState [t] m, MonadPlus m) => m t
parseChar
  = do
    state <- get
    case state of
      (x:xs) -> do {put xs; return x}
      []     -> mzero

-- POST: Consumes the first character if the input string is non-empty, fails
--       otherwise (denoted by Nothing). Updates position appropriately.
item :: Parser Char Char
item
  = do
    c <- parseChar
    updatePosition (updateParserPosition c)
    return c
-- POST: Attempts to parse input string using the given Parser.
--       If it fails then it reports an error and terminates execution
tryParser :: Parser Char a -> String -> Parser Char a
tryParser parser errorMessage = do
  p <- getPosition
  parser <|> throwError ("Syntax Error: " ++ errorMessage, updateRowPosition p)

-- POST: Consumes a single character if it satisfies the predicate, fails
--       otherwise (denoted by Nothing)
satisfy  :: (Char -> Bool) -> Parser Char Char
satisfy p
  = item >>= \x -> if p x then return x else mzero

-- POST: Does nothing if a single character satisfies the predicate, fails
--       otherwise
check :: (Char -> Bool) -> Parser Char ()
check predicate
  = do
    inputString <- get
    case inputString of
      inp@(x:xs) -> do {guard (predicate x); put inp;}
      _          -> mzero

{- BASIC ATOMIC COMBINATORS: -}

-- POST: Calls 'satisfy' with a predicate for a specific character
char  :: Char -> Parser Char Char
char c
  = satisfy (c ==)

-- POST: Calls 'satisfy' with a predicate to determine if it's an element of
--       the list
oneOf  :: String -> Parser Char Char
oneOf s
  = satisfy (`elem` s)

-- POST: Calls 'satisfy' with a predicate for single digits
digit :: Parser Char Char
digit
  = satisfy (\x -> '0' <= x && x <= '9')

-- POST: Calls 'satisfy' with predicate for lower-case letters
lower :: Parser Char Char
lower
  = satisfy (\x -> 'a' <= x && x <= 'z')

-- POST: Calls 'satisfy' with predicate for upper-case letters
upper :: Parser Char Char
upper
  = satisfy (\x -> 'A' <= x && x <= 'Z')

-- POST: Parser for letters
letter :: Parser Char Char
letter
  = lower <|> upper

-- POST: Parser for alpha-numeric characters
alphanum :: Parser Char Char
alphanum
  = letter <|> digit

-- POST: Parser for all characters including escape chararacters
character :: Parser Char Char
character
  = satisfy ( `notElem` ['\\', '\"', '\'']) <|> escapeChar

-- POST: Parser for escape characters
escapeChar :: Parser Char Char
escapeChar
  = do
      char '\\'
      tryParser (check (\c -> c `elem` map fst escapeCharAssoc))
                       "Invalid Escape Char"
      escaped_char <- item
      return $ fromJust $ lookup escaped_char escapeCharAssoc
      where
        escapeCharAssoc = [('b','\b'), ('n','\n'), ('f','\f'),
                          ('r','\r'), ('t','\t'), ('\\','\\'),
                          ('\"','\"'), ('\'','\''), ('0', '\0')]

{- PARSERS FOR SEQUENCES: -}

-- POST: Returns the input string if the given string is parsed successfully,
--       fails otherwise
string :: String -> Parser Char String
string []
  = return []
string (x:xs)
  = do
      char x
      string xs
      return (x : xs)

-- POST:    Parses zero or more occurences of p seperated by sep. Returns
--          parsed items as a list
-- EXAMPLE: It can be used to parse an inputstring of the form "1,2,3" where
--          the seperators have no special meaning
sepby :: Parser Char a -> Parser Char b -> Parser Char [a]
sepby p sep
  = sepby' p sep <|> return []

-- POST: Similar to sepby but it parses one or more occurences. It will fail
--       if there is not at least one occurence of p
sepby' :: Parser Char a -> Parser Char b -> Parser Char [a]
sepby' p sep
  = do
      x  <- p
      xs <- many (sep >> p)
      return (x : xs)

-- POST:    It will parse one occurence of p, but first remove an opening
--          delimiter and then after parsing p it will remove a closing
--          delimiter. Returns the result of parsing p
-- EXAMPLE: It can be used to remove brackets and parse the contents inside.
--          parse (char '(') intLiteral (char ')') "(1)" will return
--          IntLiteral 1. It does not take in to account any white space
bracketNoWS :: Parser Char a -> Parser Char b -> Parser Char c -> Parser Char b
bracketNoWS open p close
  = do
      open
      x <- p
      close
      return x
