module ParserCombinators where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Maybe
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

-- generic combinators
sepby1 :: Parser a -> Parser b -> Parser [a]
sepby1 p sep = do
    x <- p
    xs <- many (do {sep; p;})
    return (x:xs)

sepby :: Parser a -> Parser b -> Parser [a]
sepby p sep = sepby1 p sep <|> return []

chainl1 :: Parser Expr -> Parser BinOp -> Parser Expr
chainl1 p op = do { x <- p; rest x}
  where
    rest x = (do
      f <- op
      y <- p
      rest (BinaryOp f x y)) <|> return x

chainr1 ::Parser Expr -> Parser BinOp -> Parser Expr
chainr1 p op = (do
  x <- p
  f <- op
  acc <- chainr1 p op
  return (BinaryOp f x acc)) <|> p

--chainr :: (Show a) => Parser a -> Parser (a -> a -> a) -> a -> Parser a
--chainr p op v = chainr1 p op <|> return v

--chainl :: (Show a) => Parser a -> Parser (a -> a -> a) -> a -> Parser a
--chainl p op v = chainl1 p op <|> return v

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do { open; x <- p; close; return x }

intLiter :: Parser Expr
intLiter = do
  traceM $ "INT LITER FUNCTION" ++ "dfs"
  sign <- string "-" <|> return []
  traceM $ "Retrieving Sign in intLiter, the value of sign is: " ++ show sign
  digits <- many digit
  traceM $ "The value of digits is: " ++ show digits
  return $ IntLit (read (sign ++ digits))


boolLiter :: Parser Expr
boolLiter = do
  traceM $ "HITS THE BOOL LITER FUNCTION" ++ "Dfds"
  boolean <- string "true" <|> string "false"
  traceM $ "THE VALUE OF BOOLEAN is: " ++ show boolean
  if boolean == "true"
    then return $ BoolLit True
    else return $ BoolLit False

charLit :: Parser Expr
charLit = do
  chr <- bracket (char '\'') chararcter (char '\'')
  return $ CharLit chr

chararcter :: Parser Char
chararcter = satisfy (\s -> s `notElem` ['\\', '\"', '\'']) <|> escapeChar


escapeChar :: Parser Char
escapeChar = do
  char '\\'
  escaped_char <- item
  return $ fromJust $ lookup escaped_char escapeCharAssoc
  where
    escapeCharAssoc = [('b','\b'), ('n','\n'), ('f','\f')
                      , ('r','\r'), ('t','\t'), ('\\','\\')
                      , ('\"','\"'), ('\'','\''), ('0', '\0')]
-- We need to do error handling

pairLiter :: Parser Expr
pairLiter = do
  string "null"
  return PairLiteral

ident :: Parser String
ident = do
  first <- char '_' <|> letter
  rest  <- many letter
  return $ first:rest

-- this is slyly broken
identifier :: Parser String
identifier = do
  x <- ident
  guard $ notElem x ks
  return x
-- implement token func
ks = ["this","is","filler"]

exprIdent :: Parser Expr
exprIdent = do
  traceM $ "exprIDENT"
  var <- identifier
  return $ ExprI var


spaces :: Parser ()
spaces = do
  many $ satisfy isSpace
  return ()

stringLiter :: Parser Expr
stringLiter = do
  string <- bracket (char '\"') (many chararcter) (char '\"')
  return $ StringLit string
-- data UnOp    = Exclatation | Neg | Len | Ord | Chr  deriving (Show, Eq)

parseUnaryOp :: Parser UnOp
parseUnaryOp = do
  un_op <- string "!" <|> string "-" <|> string "len" <|> string "ord" <|> string "chr"
  let astOp = fromJust $ lookup un_op un_op_assoc
  return astOp
  where
    un_op_assoc = [("!", Not), ("-", Neg), ("len", Len), ("ord", Ord), ("chr", Chr)]

unaryExpr:: Parser Expr
unaryExpr = do
  un_op <- parseUnaryOp
  expr <- parseExpr
  return $ UnaryOp un_op expr

parseBinaryOp :: Parser BinOp
parseBinaryOp = do
  bin_op <- string "*" <|> string "/" <|> string "%" <|> string "+" <|> string "-"
            <|> string ">" <|> string ">=" <|> string "<" <|> string "<="
            <|> string "==" <|> string "!=" <|> string "&&" <|> string "||"
  let astOp = fromJust $ lookup bin_op bin_op_assoc
  return astOp
  where
    bin_op_assoc = [("*", Mul), ("/", Div), ("%", Mod), ("+", Add),
                    ("-", Sub), (">", Definitions.GT), (">=", GTE), ("<", Definitions.LT),
                    ("<=", LTE), ("==", Definitions.EQ), ("!=", NEQ), ("&&", AND),
                    ("||", OR)]
binaryExpr :: Parser Expr
binaryExpr = do
  traceM $ "Hello we are in binaryExpr"
  expr <- parseExpr
  traceM $ "The value of the first expr is: " ++ show expr
  bin_op <- parseBinaryOp
  traceM $ "The value of the op is: " ++ show bin_op
  expr' <- parseExpr
  traceM $ "The value of expr' is: " ++ show expr'
  let x = BinaryOp bin_op expr expr'
  traceM $ "This is the end result of the function: " ++ show x
  return x

bracketedExpr :: Parser Expr
bracketedExpr = do
  traceM $ "ENTER BRACKETING FUNCTION"
  x <- bracket (char '(') parseExpr (char ')')
  traceM $ "LEAVING BRACKETING FUNCTION, having parsed " ++ show x
  return x

parseTest :: Parser Expr
parseTest = Parser $ \s -> do
                           let xs = parse parseExpr s
                           return (last xs)


factor = boolLiter `mplus` charLit `mplus` stringLiter `mplus` pairLiter `mplus` bracketedExpr

parseExpr :: Parser Expr
parseExpr =
   intLiter
  `mplus` boolLiter
  `mplus` charLit
  `mplus` stringLiter
  `mplus` pairLiter
  `mplus` exprIdent
  `mplus` unaryExpr
  `mplus` bracketedExpr
  `mplus` chainl1 factor parseBinaryOp
