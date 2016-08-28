module ParserCombinators where

import           Control.Applicative
import           Control.Monad
import           Debug.Trace
import           Definitions

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

-- Parser for words
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
character = satisfy (\s -> s `notElem` ['\\', '\"', '\'']) <|> escapeChar

{-}
escapeChar :: Parser Char
escapeChar = do
  traceM "we are in the escape char function"
  x <- char '\\' -- '\\'
  traceM $ "Show the value of x:" ++ show x
  escaped_char <- item
  --traceM $ "Show the value of escaped_char:" ++ show escaped_char
  return $ read $ x:[escaped_char]
-}

{-
escapeChar :: Parser Char
escapeChar = do
  escaped_char <- item
  char <- lookup escaped_char list
  case char of
    Nothing -> return ''
    Just x  -> return x
  where
    list = [('b','\b'), ('n','\n'), ('f','\f')
           , ('r','\r'), ('t','\t'), ('\\','\\')
           , ('\"','\"'), ('\'','\''), ('0', '\0')]
-}

pairLiter :: Parser Expr
pairLiter = do
  string "null"
  return PairLiteral

ident :: Parser Ident
ident = do
  first <- char '_' <|> letter
  rest <- many letter
  return $ Ident $ first:rest
