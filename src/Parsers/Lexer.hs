{-
This module defines a number of combinators to handle lexical issues, such as removing whitespace and comments.
It also handles distinguishing between keywords and identifiers.
-}
module Parsers.Lexer {-(export list?)-} where

import           Control.Applicative
import           Control.Monad
import           Data.Char                (isSpace)
import           Data.Maybe               (fromJust)

import           Utility.BasicCombinators
import           Utility.Declarations
import           Utility.Definitions

commentDelim
  = "#"
-- PRE: None
-- POST: Removes single line comments.
comments :: Parser ()
comments
  = void $ string commentDelim >> many (satisfy (/= '\n')) >> char '\n'

-- PRE: None
-- Post: Removes spaces incl \t,\n etc
spaces :: Parser ()
spaces = void $ some (satisfy isSpace)

-- PRE: None
-- POST: Removes whitespace or comments.
junk :: Parser ()
junk
  = void $ many (spaces <|> comments)

{-
The following three functions remove leading and trailing whitespace.
-}

leadingWS :: Parser a -> Parser a
leadingWS p
  = junk >> p

trailingWS :: Parser a -> Parser a
trailingWS p = do
  parsedValue <- p
  junk
  return parsedValue

trimWS :: Parser a -> Parser a
trimWS
  = trailingWS . leadingWS


-- PRE: None
-- POST: Removes whitespace before and after the result of the string parser.
token :: String -> Parser String
token
  = trimWS . string

-- PRE: None
-- POST: It is used to parse keywords defined in the WACC language.
keyword :: String -> Parser String
keyword k = do
  kword <- leadingWS (string k)
  check isSpace <|> check isPunctuation <|> check isComment
  junk
  return kword

-- List of operators defined in the WACC language
operators :: String
operators = map (head.fst) (lowBinOps ++ highBinOps ++ higherBinOps)

-- PRE : None
-- POST: Returns True if the given input is either a seperator or an operator.
isPunctuation :: Char -> Bool
isPunctuation c =  c `elem` ([';', ',', ']', '[', ')', '('] ++ operators)

-- PRE: None
-- POST: Returns True if you are at the start of a comment.
isComment :: Char -> Bool
isComment c = c == '#'

-- PRE: None
-- POST: Parser for the given input char, it also removes whitespace around the char.
punctuation :: Char -> Parser Char
punctuation
  = trimWS . char

-- List of keywords defined in the wacc language.
keywords = ["while", "if", "fi", "else", "null", "pair", "is", "begin", "skip", "end", "call", "newpair", "fst", "snd"]

-- PRE: None
-- POST: A parser for identifiers used to parse identifers in the wacc language, removes trailing whitespace.
identifiers :: Parser String
identifiers
  = identifier >>= token

{-
Two Utility functions which are used to parse identifiers. An identifier in the wacc language starts with either an alphanum char or
an underscore char, followed by any number of underscore or alphanum chars. The ident function includes a check to ensure that
the parsed identifier is not a keyword.
-}
ident :: Parser String
ident
  = liftM2 (:) (char '_' <|> letter) (many (alphanum <|> char '_'))

identifier :: Parser String
identifier = trimWS $ do
  x <- ident
  guard (x `notElem` keywords)
  return x

-- PRE: The given input string contains a value which is present in the map.
-- POST: It takes as input a map from strings to values of type a. It attempts to parse one of the strings in the map and
-- if it succeeds it will return the corresponding a value. Essentially a parser lookup function. It removes trailing WS.
parseFromMap :: [(String, a)] -> Parser a
parseFromMap assoclist = do
  value <- foldr1 (<|>) (map (token . fst) assoclist)
  return $ fromJust (lookup value assoclist)


-- Similar to bracketNoWS defined in basic combinators, however it takes in to account whitespace.
bracket :: Parser a -> Parser b -> Parser c  -> Parser b
bracket open p close
  = trimWS $ bracketNoWS open p close
