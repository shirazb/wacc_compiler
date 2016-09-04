---
-- Expression Parsing.
---
module Parsers.Expression (parseExpr, parseExprList, arrayElem) where

import Control.Applicative
import Control.Monad

import Parsers.Lexer
import Utility.BasicCombinators
import Utility.Declarations
import Utility.Definitions

intLiteral :: Parser Expr
intLiteral
  = (IntLit . read) <$> some digit

boolLiteral :: Parser Expr
boolLiteral = do
  boolean <- string "true" <|> string "false"
  if boolean == "true"
    then return (BoolLit True)
    else return (BoolLit False)

charLiteral :: Parser Expr
charLiteral
   = CharLit <$> bracket (char '\'') character (char '\'')

pairLiteral :: Parser Expr
pairLiteral
  = string "null" >> return PairLiteral

exprIdent :: Parser Expr
exprIdent
  = IdentE <$> identifier

stringLiter :: Parser Expr
stringLiter
  = StringLit <$> bracket (char '\"') (many character) (char '\"')

parseUnaryOp :: Parser UnOp
parseUnaryOp
  = parseFromMap unOpAssoc

parseBinaryOpLow :: Parser BinOp
parseBinaryOpLow
  = parseFromMap lowBinOps

parseBinaryOpHigh :: Parser BinOp
parseBinaryOpHigh
  = parseFromMap highBinOps

parseBinaryOpHigher :: Parser BinOp
parseBinaryOpHigher
  = parseFromMap higherBinOps

chainl1 :: Parser Expr -> Parser BinOp -> Parser Expr
chainl1 p op
  = p >>= rest
  where
    rest x = (do
      f <- op
      y <- p
      rest $ BinaryApp f x y) <|> return x

lowBinaryExpr :: Parser Expr
lowBinaryExpr
  = highBinaryExpr `chainl1` parseBinaryOpLow

highBinaryExpr :: Parser Expr
highBinaryExpr
  = higherBinaryExpr `chainl1` parseBinaryOpHigh

higherBinaryExpr :: Parser Expr
higherBinaryExpr
  = parseExpr' `chainl1` parseBinaryOpHigher

binaryExpr :: Parser Expr
binaryExpr
  = lowBinaryExpr

unaryExpr :: Parser Expr
unaryExpr
  = UnaryApp <$> parseUnaryOp <*> parseExpr

bracketedExpr :: Parser Expr
bracketedExpr
  = bracket (char '(') parseExpr (char ')')

arrayElem :: Parser ArrayElem
arrayElem
  = ArrayElem <$> identifier <*> some (bracket (char '[') parseExpr (char ']'))

arrayElemExpr :: Parser Expr
arrayElemExpr
  = ExprArray <$> arrayElem

parseExpr' :: Parser Expr
parseExpr'
  =   arrayElemExpr
  <|> unaryExpr
  <|> bracketedExpr
  <|> charLiteral
  <|> boolLiteral
  <|> stringLiter
  <|> pairLiteral
  <|> exprIdent
  <|> intLiteral

parseExpr :: Parser Expr
parseExpr
  = binaryExpr <|> parseExpr'

parseExprList :: Char -> Char -> Parser [Expr]
parseExprList open close
  = bracket (char open) (sepby parseExpr (char ',')) (char close)
