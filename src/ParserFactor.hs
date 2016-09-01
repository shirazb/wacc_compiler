module ParserCombinators where
import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Debug.Trace
import           Utility.DefinitionsFactor
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

factorIdent :: Parser Expr
factorIdent = do
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

parseBinaryOpLow :: Parser BinOp
parseBinaryOpLow = do
  binOp <- string "+" <|> string "-" <|> string ">=" <|> string ">" <|> string "<=" <|> string "<" 
            <|> string "==" <|> string "!=" <|> string "&&" <|> string "||"
  let astOp = fromJust $ lookup binOp binOps
  return astOp

parseBinaryOpHigh :: Parser BinOp
parseBinaryOpHigh = do
  binOp <- string "*" <|> string "/" <|> string "%"
  let astOp = fromJust $ lookup binOp binOps
  return astOp
 
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
highBinaryExpr = parseExpr' `chainl1` parseBinaryOpHigh

unaryExpr :: Parser Expr
unaryExpr = do
  op <- parseUnaryOp
  expr <- parseExpr
  return $ UnaryApp op expr

bracketedExpr :: Parser Expr
bracketedExpr = bracket (char '(') parseExpr (char ')')

arrayElem :: Parser Expr
arrayElem = do
  array_name <- identifier
  arraynotation <- some $ bracket (char '[') parseExpr (char ']')
  return $ ExprArray $ ArrayElem array_name arraynotation

binaryExpr :: Parser Expr
binaryExpr = lowBinaryExpr

parseExpr' =
      arrayElem   
  <|> unaryExpr
  <|> bracketedExpr
  <|> charLiteral
  <|> boolLiteral
  <|> stringLiter
  <|> pairLiteral
  <|> factorIdent
  <|> intLiteral


parseExpr = binaryExpr <|> parseExpr'