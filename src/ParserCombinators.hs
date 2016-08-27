module ParserCombinators where

import Definitions
import Control.Monad
import Control.Applicative
import Debug.Trace

item :: Parser Char
item = Parser $ \inp -> case inp of
                        [] -> []
                        x:xs -> [(x, xs)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \x -> if p x then return x else mzero

char :: Char -> Parser Char
char c = satisfy (c ==)

oneOf :: String -> Parser Char
oneOf s = satisfy ( `elem` s)

digit :: Parser Char
digit = satisfy (\x -> '0' <= x && x <= '9')

lower :: Parser Char
lower = satisfy (\x -> 'a' <= x && x <= 'z')

upper :: Parser Char
upper = satisfy (\x -> 'A' <= x && x <= 'Z')

letter :: Parser Char
letter =  lower <|> upper

alphanum :: Parser Char
alphanum = letter <|> digit

word :: Parser String
word = many letter

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  char x
  string xs
  return (x:xs)

intLiter :: Parser Expr
intLiter = do
  sign <- string "-" <|> return []
  digits <- many digit
  return $ IntLit (read (sign ++ digits))

boolLiter :: Parser Expr
boolLiter = do
  boolean <- string "true" <|> string "false"
  if boolean == "true"
    then return $ BoolLit True
    else return $ BoolLit False

character :: Parser Char
character = do
  char <- satisfy (\s -> notElem s ['\\', '\"', '\'', '\'']) <|> escapeChar
  return char

escapeChar :: Parser Char
escapeChar = do
  traceM "we are in the escape char function"
  x <- char '\\' ---
  traceM $ "Show the value of x:" ++ show x
  escaped_char <- item
  return $ read $ "\\" ++ [escaped_char]

pairLiter :: Parser Expr
pairLiter = do
  string "null"
  return PairLiteral

ident :: Parser Ident
ident = do
  first <- char '_' <|> letter
  rest <- many letter
  return $ Ident $ first:rest
