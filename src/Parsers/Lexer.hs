---
-- Lexical Issues
---
module Parsers.Lexer {-(export list?)-} where

import Control.Applicative
import Control.Monad
import Data.Char (isSpace)
import Data.Maybe (fromJust)

import Utility.BasicCombinators
import Utility.Declarations
import Utility.Definitions

commentDelim
  = "#"

comments :: Parser ()
comments
  = void $ string commentDelim >> many (satisfy (/= '\n')) >> char '\n'

-- PRE: None
-- Post: Removes spaces incl \t,\n etc
spaces :: Parser ()
spaces = void $ some (satisfy isSpace)

junk :: Parser ()
junk
  = void $ many (spaces <|> comments)

leadingWS :: Parser a -> Parser a
leadingWS p
  = junk >> p

trailingWS :: Parser a -> Parser a
trailingWS p = do
  parsedValue <- p
  junk
  return parsedValue

-- Could try solve duplicate trimWS calls hackily by somehow having trimWS (trimWS ..) short circuit to trimWS
trimWS :: Parser a -> Parser a
trimWS
  = trailingWS . leadingWS

token :: String -> Parser String
token
  = trimWS . string

-- don't understand why there is no semicolon problem.
-- might still be problematic, or can be shortened
-- THIS IS A POTENTIAL ISSUE IS THIS A QUICK FIX OR NAH????
-- WHAT WE HAVE ESSENTAILLY DONE IS JUST CHECK FOR THE semi-colon
-- so what we could do is check for a list of seperators?
-- after anykeywords
keyword :: String -> Parser String
keyword k = do
  kword <- leadingWS (string k)
  check isSpace <|> check isPunctuation <|> check isComment
  junk
  return kword

operators :: String
operators = map (head.fst) (lowBinOps ++ highBinOps ++ higherBinOps)

isPunctuation :: Char -> Bool
isPunctuation c =  c `elem` ([';', ',', ']', '[', ')', '('] ++ operators)

isComment :: Char -> Bool
isComment c = c == '#'

punctuation :: Char -> Parser Char
punctuation
  = trimWS . char

-- TODO: Finish list of keywords
-- do we add new pair? there isnt a space after it
-- think about keywords like fst n snd , do we actually have them as keywords
keywords = ["while", "if", "fi", "else", "null", "pair", "is", "begin", "skip", "end", "call", "newpair", "fst", "snd"]

identifiers :: Parser String
identifiers
  = identifier >>= token

ident :: Parser String
ident
  = liftM2 (:) (char '_' <|> letter) (many (alphanum <|> char '_'))

identifier :: Parser String
identifier = trimWS $ do
  x <- ident
  guard (x `notElem` keywords)
  return x

-- TODO: Put this in a more appropriate file!
-- we could use an actual MAP from Data.Map
parseFromMap :: [(String, a)] -> Parser a
parseFromMap assoclist = do
  value <- foldr1 (<|>) (map (token . fst) assoclist)
  return $ fromJust (lookup value assoclist)


bracket :: Parser a -> Parser b -> Parser c  -> Parser b
bracket open p close
  = trimWS $ bracketNoWS open p close
