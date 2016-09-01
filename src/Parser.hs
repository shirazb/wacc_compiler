module Parser where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Debug.Trace
import           Utility.Definitions
import           Utility.BasicCombinators

intLiteral :: Parser Expr
intLiteral = do
  digits <- some digit
  return $ IntLit $ read digits

boolLiteral :: Parser Expr
boolLiteral = do
  boolean <- string "true" <|> string "false"
  if boolean == "true"
    then return $ BoolLit True
    else return $ BoolLit False

charLiteral :: Parser Expr
charLiteral = do
  chr <- bracket (char '\'') character (char '\'')
  return $ CharLit chr

pairLiteral :: Parser Expr
pairLiteral = do
  string "null"
  return PairLiteral

ident :: Parser String
ident = do
  first <- char '_' <|> letter
  rest  <- many (alphanum <|> char '_')
  return $ first:rest

identifier :: Parser String
identifier = do
  x <- ident
  guard $ notElem x keywords
  return x

exprIdent :: Parser Expr
exprIdent = do
  var <- identifier
  return $ ExprI var

spaces :: Parser ()
spaces = do
  many $ satisfy isSpace
  return ()

stringLiter :: Parser Expr
stringLiter = do
  string <- bracket (char '\"') (many character) (char '\"')
  return $ StringLit string

parseUnaryOp :: Parser UnOp
parseUnaryOp = do
  unOp <- string "!" <|> string "-" <|> string "len" <|> string "ord" <|> string "chr"
  let astOp = fromJust $ lookup unOp unOpAssoc
  return astOp
  where
    unOpAssoc = [("!", Not), ("-", Neg), ("len", Len), ("ord", Ord), ("chr", Chr)]

parseBinaryOp :: Parser BinOp
parseBinaryOp = do
  binOp <- string "*" <|> string "/" <|> string "%" <|> string "+" <|> string "-"
            <|> string ">=" <|> string ">" <|> string "<=" <|> string "<"
            <|> string "==" <|> string "!=" <|> string "&&" <|> string "||"
  let astOp = fromJust $ lookup binOp binOps
  return astOp
  where
    binOps        = [("*", Mul), ("/", Div), ("%", Mod), ("+", Add),
                    ("-", Sub), (">", Utility.Definitions.GT), (">=", GTE), ("<", Utility.Definitions.LT),
                    ("<=", LTE), ("==", Utility.Definitions.EQ), ("!=", NEQ), ("&&", AND),
                    ("||", OR)]


-- from binary expressions onwards, we are not sure if they work

-- DEBUGGING ---

unaryExpr:: Parser Expr
unaryExpr = do
  un_op <- parseUnaryOp
  expr <- parseExpr
  return $ UnaryApp un_op expr

chainl1 :: Parser Expr -> Parser BinOp -> Parser Expr
chainl1 p op = do { x <- p; rest x}
  where
    rest x = (do
      f <- op
      y <- p
      rest $ BinaryApp f x y) <|> return x

-- -- how do you do operator precedence????
-- parseAdd = chainl1 intLiter parseBinaryOp
--
binaryExpr :: Parser Expr
binaryExpr = chainl1 parseExpr parseBinaryOp

 -- binaryExpr :: Parser Expr
-- binaryExpr = do
--   expr <- parseExpr
--   binOp <- parseBinaryOp
--   expr' <- parseExpr
--   return $ BinaryApp binOp expr expr'

-- the concept of bracketedExpr works
bracketedExpr :: Parser Expr
bracketedExpr = bracket (char '(') parseExpr (char ')')
--
-- parseTest :: Parser Expr
-- parseTest = Parser $ \s -> do
--                            let xs = parse parseExpr s
--                            return (last xs)
--
--
-- factor = boolLiter `mplus` charLit `mplus` stringLiter `mplus` pairLiter `mplus` bracketedExpr


-- this is broken because we need to remove left recursion from the grammar
-- parseExpr' = return [] <|>  parseBinaryOp `mplus` parseExpr
--
parseExpr :: Parser Expr
parseExpr =
   intLiteral
  <|> boolLiteral
  <|> charLiteral
  <|> stringLiter
  <|> pairLiteral
  <|> exprIdent
  <|> unaryExpr
  <|> bracketedExpr
  <|> binaryExpr
