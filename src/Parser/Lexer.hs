{-
  This module defines a number of combinators to handle lexical issues, such as
  removing whitespace and comments. It also handles distinguishing between
  keywords and identifiers.
-}

module Parser.Lexer where

import Control.Applicative (many, some, (<|>))
import Control.Monad       (void)
import Data.Char           (isSpace)
import Data.Maybe          (fromJust)

import Parser.Combinators
import Utilities.Definitions

commentDelim :: String
commentDelim
  = "#"

-- POST: Removes single line comments.
comments :: Parser Char ()
comments = do
    string commentDelim
    many (satisfy (/= '\n'))
    require (char '\n') "Comment not terminated with newline."
    return ()

-- Post: Removes spaces incl \t,\n etc
spaces :: Parser Char ()
spaces
  = void $ some (satisfy isSpace)

-- POST: Removes whitespace or comments.
junk :: Parser Char ()
junk
  = void $ many (spaces <|> comments)

-- The following three functions remove leading and trailing whitespace:

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

-- POST: Removes whitespace before and after the result of the string parser
token :: String -> Parser Char String
token
  = trimWS . string

-- POST: It is used to parse keywords defined in the WACC language
keyword :: String -> Parser Char String
keyword k = do
  kword <- leadingWS (string k)
  check isSpace <|> check isPunctuation <|> check isComment
  junk
  return kword

-- POST: List of operators defined in the WACC language
operators :: String
operators
  = map (head . fst) (binOpPrec1 ++ binOpPrec2 ++ binOpPrec3 ++
                      binOpPrec4 ++ binOpPrec5 ++ binOpPrec6)

-- POST: Returns True if the given input is either a seperator or an operator
isPunctuation :: Char -> Bool
isPunctuation
  =  flip elem ([';', ',', ']', '[', ')', '('] ++ operators)

-- POST: Returns True if you are at the start of a comment
isComment :: Char -> Bool
isComment
  = (==) '#'

-- POST: Parser for the given input char, it also removes whitespace around
--       the char
punctuation :: Char -> Parser Char Char
punctuation
  = trimWS . char

-- PRE:  The given input string contains a value which is present in the map
-- POST: It takes as input a map from strings to values of type a. It attempts
--       to parse one of the strings in the map and if it succeeds it will
--       return the corresponding a value. Essentially a parser lookup
--       function. It removes trailing WS.
parseFromMap :: (Show a) => [(String, a)] -> Parser Char a
parseFromMap assoclist = do
  value  <- foldr1 (<|>) (map (token . fst) assoclist)
  return $ fromJust (lookup value assoclist)

-- POST: Similar to bracketNoWS defined in basic combinators, however it takes
--       whitespaces into account.
bracket :: Parser Char a -> Parser Char b -> Parser Char c  -> Parser Char b
bracket open p close
  = trimWS $ bracketNoWS open p close

-- List of keywords defined in the wacc language
keywords = ["while", "if", "fi", "else", "null", "pair", "is", "begin", "skip",
            "end", "call", "newpair", "fst", "snd", "return", "read", "free",
            "exit", "println", "print", "then", "do", "done", "int", "bool",
            "char", "string", "len", "chr", "ord", "true", "false"]
