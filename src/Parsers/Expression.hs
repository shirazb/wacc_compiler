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
  = token $ leadWSC $ (IntLit . read) <$> some digit

boolLiteral :: Parser Expr
boolLiteral = token $ leadWSC (do
  boolean <- string "true" <|> string "false"
  if boolean == "true"
    then return (BoolLit True)
    else return (BoolLit False))

charLiteral :: Parser Expr
charLiteral
   = token $ leadWSC $ CharLit <$> bracket (char '\'') character (char '\'')

pairLiteral :: Parser Expr
pairLiteral
  = token $ leadWSC $ string "null" >> return PairLiteral

exprIdent :: Parser Expr
exprIdent
  = token $ leadWSC $ IdentE <$> identifier

stringLiter :: Parser Expr
stringLiter
  = token $ leadWSC $ StringLit <$> bracket (char '\"') (many character) (char '\"')

parseUnaryOp :: Parser UnOp
parseUnaryOp
  = token $ leadWSC $ parseFromMap unOpAssoc

parseBinaryOpLow :: Parser BinOp
parseBinaryOpLow
  = token $ leadWSC $ parseFromMap lowBinOps

parseBinaryOpHigh :: Parser BinOp
parseBinaryOpHigh
  = token $ leadWSC $ parseFromMap highBinOps

parseBinaryOpHigher :: Parser BinOp
parseBinaryOpHigher
  = token $ leadWSC $ parseFromMap higherBinOps

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
  = token $ leadWSC $ highBinaryExpr `chainl1` parseBinaryOpLow

highBinaryExpr :: Parser Expr
highBinaryExpr
  = token $ leadWSC $ higherBinaryExpr `chainl1` parseBinaryOpHigh

higherBinaryExpr :: Parser Expr
higherBinaryExpr
  = token $ leadWSC $ parseExpr' `chainl1` parseBinaryOpHigher

binaryExpr :: Parser Expr
binaryExpr
  = token $ leadWSC lowBinaryExpr

unaryExpr :: Parser Expr
unaryExpr
  = token $ leadWSC $ UnaryApp <$> parseUnaryOp <*> parseExpr

bracketedExpr :: Parser Expr
bracketedExpr
  = token $ leadWSC $ bracket (char '(') parseExpr (char ')')

arrayElem :: Parser ArrayElem
arrayElem
  = token $ leadWSC $ ArrayElem <$> identifier <*> some (bracket (char '[') parseExpr (char ']'))

arrayElemExpr :: Parser Expr
arrayElemExpr
  = token $ leadWSC $ ExprArray <$> arrayElem

parseExpr' :: Parser Expr
parseExpr'
  =  token $ leadWSC (arrayElemExpr
  <|> unaryExpr
  <|> bracketedExpr
  <|> charLiteral
  <|> boolLiteral
  <|> stringLiter
  <|> pairLiteral
  <|> exprIdent
  <|> intLiteral)

parseExpr :: Parser Expr
parseExpr
  = token $ leadWSC $ binaryExpr <|> parseExpr'

parseExprList :: Char -> Char -> Parser [Expr]
parseExprList open close
  = token $ leadWSC $ bracket (char open) (sepby parseExpr (char ',')) (char close)
