module ParserCombinators where
import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Debug.Trace
import           DefinitionsFactor
import           BasicCombinators


intLiteral :: Parser Expr
intLiteral = do
  digits <- some digit
  return $ Factor $ IntLit $ read digits

boolLiteral :: Parser Factor
boolLiteral = do
  boolean <- string "true" <|> string "false"
  if boolean == "true"
    then return $ BoolLit True
    else return $ BoolLit False

charLiteral :: Parser Factor
charLiteral = do
  chr <- bracket (char '\'') character (char '\'')
  return $ CharLit chr

pairLiteral :: Parser Factor
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

factorIdent :: Parser Factor
factorIdent = do
  var <- identifier
  return $ ExprI var

spaces :: Parser ()
spaces = do
  many $ satisfy isSpace
  return ()

stringLiter :: Parser Factor
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

parseBinaryOpLow :: Parser BinOp
parseBinaryOpLow = do
  binOp <- string "+" <|> string "-"
  let astOp = fromJust $ lookup binOp binOps
  return astOp

parseBinaryOpHigh :: Parser BinOp
parseBinaryOpHigh = do
  binOp <- string "*" <|> string "/" <|> string "%"
  let astOp = fromJust $ lookup binOp binOps
  return astOp
 
-- NOT IMPLEMENTED

chainl1 :: Parser Expr -> Parser BinOp -> Parser Expr
chainl1 p op = do {x <- p; rest x}
  where
    rest x = (do
      operator <- op
      y        <- p
      rest (BinApp operator x y)) <|> return x

lowBinaryExpr :: Parser Expr
lowBinaryExpr = highBinaryExpr `chainl1` parseBinaryOpLow

highBinaryExpr :: Parser Expr
highBinaryExpr = intLiteral `chainl1` parseBinaryOpHigh

unaryExpr :: Parser Factor
unaryExpr = mzero
arrayElem :: Parser Factor
arrayElem = mzero
binaryExpr :: Parser Expr
binaryExpr = lowBinaryExpr

--parseExpr :: Parser Expr
--parseExpr =
--  parseFactorWrapInExpr
--  <|> binaryExpr

--parseFactorWrapInExpr :: Parser Expr
--parseFactorWrapInExpr = do
--  p <- parseFactor
--  return $ Factor p

--parseFactor =
--  intLiteral
--  <|> charLiteral
--  <|> boolLiteral
--  <|> stringLiter
--  <|> pairLiteral
--  <|> factorIdent
--  <|> arrayElem
--  <|> unaryExpr
