{-
This module consists of parser combinators which are used to parse expressions in the WACC language.
A number of basic expression combinators have been defined which are then used as building blocks to build
more complex parsers of expressions.
-}
module Parsers.Expression (parseExpr, parseExprList, arrayElem) where

import           Control.Applicative
import           Control.Monad

import           Debug.Trace
import           Parsers.Lexer
import           Utility.BasicCombinators
import           Utility.Declarations
import           Utility.Definitions

{- Basic combinators which are used to parse atomic expressions -}

intLiteral :: Parser Expr
intLiteral
  = trimWS $ (IntLit . read) <$> some digit

boolLiteral :: Parser Expr
boolLiteral = do
  boolean <- keyword "true" <|> keyword "false"
  if boolean == "true"
    then return (BoolLit True)
    else return (BoolLit False)

charLiteral :: Parser Expr
charLiteral
   = CharLit <$> bracket (char '\'') character (char '\'')

pairLiteral :: Parser Expr
pairLiteral
  = keyword "null" >> return PairLiteral

exprIdent :: Parser Expr
exprIdent
  = IdentE <$> identifier

stringLiter :: Parser Expr
stringLiter
  = StringLit <$> bracket (char '\"') (many character) (char '\"')


{- Complex combinators used to parse larger & complex expressions. They are built using the basic combinators defined above and a few generic
   combinators defined in the BasicCombinators module.
 -}

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
  = trimWS $ p >>= rest
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
binaryExpr = lowBinaryExpr

unaryExpr :: Parser Expr
unaryExpr
  = UnaryApp <$> parseUnaryOp <*> parseExpr'

bracketedExpr :: Parser Expr
bracketedExpr
  = bracket (punctuation '(') parseExpr (punctuation ')')

arrayElem :: Parser ArrayElem
arrayElem
  = ArrayElem <$> identifier <*> some (bracket (punctuation '[') parseExpr (punctuation ']'))

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
  <|> exprIdent
  <|> pairLiteral
  <|> intLiteral

parseExpr :: Parser Expr
parseExpr
  = binaryExpr <|> parseExpr'

parseExprList :: Char -> Char -> Parser [Expr]
parseExprList open close
  = bracket (punctuation open) (sepby parseExpr (punctuation ',')) (punctuation close)
