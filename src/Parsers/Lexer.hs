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
import Debug.Trace

commentDelim
  = "#"

-- PRE: None
-- POST: Removes single line comments.
comments :: Parser Char ()
comments
  = void $ string commentDelim >> many (satisfy (/= '\n')) >> char '\n'

-- PRE: None
-- Post: Removes spaces incl \t,\n etc
spaces :: Parser Char ()
spaces = void $ some (satisfy isSpace)

-- PRE: None
-- POST: Removes whitespace or comments.
junk :: Parser Char ()
junk
  = void $ many (spaces <|> comments)

{-
The following three functions remove leading and trailing whitespace.
-}

leadingWS :: Parser Char b -> Parser Char b
leadingWS p
  = junk >> p

trailingWS :: Parser Char b -> Parser Char b
trailingWS p = do
  parsedValue <- p
  junk
  return parsedValue

trimWS :: Parser Char b -> Parser Char b
trimWS
  = trailingWS . leadingWS


-- PRE: None
-- POST: Removes whitespace before and after the result of the string parser.
token :: String -> Parser Char String
token
  = trimWS . string
-- TODO: THe issue is with the keyword AND
-- the end of file terminator.
-- PRE: None
-- POST: It is used to parse keywords defined in the WACC language.
keyword :: String -> Parser Char String
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
punctuation :: Char -> Parser Char Char
punctuation
  = trimWS . char

-- List of keywords defined in the wacc language.
keywords = ["while", "if", "fi", "else", "null", "pair", "is", "begin", "skip", "end", "call",
            "newpair", "fst", "snd", "return", "read", "free", "exit", "println", "print",
            "then", "do", "done", "int", "bool", "char", "string", "len", "chr", "ord",
            "true", "false"]

-- PRE: None
-- POST: A parser for identifiers used to parse identifers in the wacc language, removes trailing whitespace.
identifiers :: Parser Char String
identifiers
  = identifier >>= token

{-
Two Utility functions which are used to parse identifiers. An identifier in the wacc language starts with either an alphanum char or
an underscore char, followed by any number of underscore or alphanum chars. The ident function includes a check to ensure that
the parsed identifier is not a keyword.
-}
ident :: Parser Char String
ident
  = liftM2 (:) (char '_' <|> letter) (many (alphanum <|> char '_'))

identifier :: Parser Char String
identifier = trimWS $ do
  x <- ident
  guard (x `notElem` keywords)
  return x

-- PRE: The given input string contains a value which is present in the map.
-- POST: It takes as input a map from strings to values of type a. It attempts to parse one of the strings in the map and
-- if it succeeds it will return the corresponding a value. Essentially a parser lookup function. It removes trailing WS.
parseFromMap :: (Show a) => [(String, a)] -> Parser Char a
parseFromMap assoclist = do
  value <- foldr1 (<|>) (map (token . fst) assoclist)
  return $ fromJust (lookup value assoclist)


-- Similar to bracketNoWS defined in basic combinators, however it takes in to account whitespace.
bracket :: Parser Char a -> Parser Char b -> Parser Char c  -> Parser Char b
bracket open p close
  = trimWS $ bracketNoWS open p close
