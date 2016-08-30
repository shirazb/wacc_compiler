module ParserCombinators where
import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Debug.Trace
import           DefinitionsFactor
import           BasicCombinators


intLiteral :: Parser Factor
intLiteral = do
  digits <- some digit
  return $ IntLit $ read digits

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
  guard $ notElem x ks
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

parseBinaryOp :: Parser BinOp
parseBinaryOp = do
  binOp <- string "*" <|> string "/" <|> string "%" <|> string "+" <|> string "-"
            <|> string ">=" <|> string ">" <|> string "<=" <|> string "<"
            <|> string "==" <|> string "!=" <|> string "&&" <|> string "||"
  let astOp = fromJust $ lookup binOp binOps
  return astOp
  where
    binOps        = [("*", Mul), ("/", Div), ("%", Mod), ("+", Add),
                    ("-", Sub), (">", DefinitionsFactor.GT), (">=", GTE), ("<", DefinitionsFactor.LT),
                    ("<=", LTE), ("==", DefinitionsFactor.EQ), ("!=", NEQ), ("&&", AND),
                    ("||", OR)]


-- from binary expressions onwards, we are not sure if they work

-- DEBUGGING ---

unaryExpr :: Parser Factor
unaryExpr = mzero
-- unaryExpr:: Parser Factor
-- unaryExpr = do
--   un_op <- parseUnaryOp
--   expr <- parseExpr
--   return $ UnaryApp un_op expr
--
-- chainl1 :: Parser Factor -> Parser BinOp -> Parser Factor
-- chainl1 = mzero
-- 1 + 2 + 3 + 4 + 5 + 6
-- chainl1 p op = do { x <- p; rest x}
--   where
--     rest x = (do
--       f <- op
--       y <- p
--       rest $ BinApp f x y) <|> return x

-- -- how do you do operator precedence????
-- parseAdd = chainl1 intLiter parseBinaryOp
--
-- binaryExpr :: Parser Expr
-- binaryExpr = chainl1 parseExpr parseBinaryOp

 -- binaryExpr :: Parser Expr
-- binaryExpr = do
--   expr <- parseExpr
--   binOp <- parseBinaryOp
--   expr' <- parseExpr
--   return $ BinaryApp binOp expr expr'

-- -- the concept of bracketedExpr works
-- bracketedExpr :: Parser Expr
-- bracketedExpr = bracket (char '(') parseExpr (char ')')
arrayElem :: Parser Factor
arrayElem = mzero
binaryExpr :: Parser Expr
binaryExpr = mzero

parseExpr :: Parser Expr
parseExpr =
  parseFactorWrapInExpr
  <|> binaryExpr

parseFactorWrapInExpr :: Parser Expr
parseFactorWrapInExpr = do
  p <- parseFactor
  return $ Factor p


parseFactor =
  intLiteral
  <|> charLiteral
  <|> boolLiteral
  <|> stringLiter
  <|> pairLiteral
  <|> factorIdent
  <|> arrayElem
  <|> unaryExpr
