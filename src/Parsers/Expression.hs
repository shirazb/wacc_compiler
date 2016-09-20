{-
This module consists of parser combinators which are used to parse expressions in the WACC language.
A number of basic expression combinators have been defined which are then used as building blocks to build
more complex parsers of expressions. Refer to BNF spec of WACC language to see exactly what an expression is
in the WACC language
-}
module Parsers.Expression (parseExpr, parseExprList, arrayElem, binaryExpr) where

import           Control.Applicative
import           Control.Monad

import           Debug.Trace
import           Parsers.Lexer
import           Utility.BasicCombinators
import           Utility.Declarations
import           Utility.Definitions


-- PRE:  None
-- POST: Parses all valid expressions in the WACC language, it is factored out like
-- this to prevent the parser going in to an infinite loop due to left recursion.
parseExpr :: Parser Expr
parseExpr
  = binaryExpr <|> parseExpr'

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


-- PRE: None
-- POST: Parses all valid application of unary operators expressions.
unaryExpr :: Parser Expr
unaryExpr
  = UnaryApp <$> parseUnaryOp <*> parseExpr'

-- PRE: None
-- POST: Parser of unary operators.
parseUnaryOp :: Parser UnOp
parseUnaryOp
  = parseFromMap unOpAssoc


{-
A number of parsers used to parse valid binary expressions in the WACC language.
The design of the parser combinators take in to acccount the precdence of binary operators.
-}

-- PRE: None
-- POST: Parses all valid binary expressions
-- Example Usage: parse  binaryExpr "1 + 2" will return BinaryApp Mul (IntLit 1) (IntLit 2)
binaryExpr :: Parser Expr
binaryExpr = lowBinaryExpr

parseBinaryOpLow :: Parser BinOp
parseBinaryOpLow
  = parseFromMap lowBinOps

parseBinaryOpHigh :: Parser BinOp
parseBinaryOpHigh
  = parseFromMap highBinOps

parseBinaryOpHigher :: Parser BinOp
parseBinaryOpHigher
  = parseFromMap higherBinOps

lowBinaryExpr :: Parser Expr
lowBinaryExpr
  = highBinaryExpr `chainl1` parseBinaryOpLow

highBinaryExpr :: Parser Expr
highBinaryExpr
  = higherBinaryExpr `chainl1` parseBinaryOpHigh

higherBinaryExpr :: Parser Expr
higherBinaryExpr
  = parseExpr' `chainl1` parseBinaryOpHigher

-- PRE: None
-- POST: Returns a parser which parses a sequence of expressions seperated by a meaningful seperator
-- e.g the operator (+).
chainl1 :: Parser Expr -> Parser BinOp -> Parser Expr
chainl1 p op
  = trimWS $ p >>= rest
  where
    rest x = (do
      f <- op
      y <- p
      rest $ BinaryApp f x y) <|> return x


-- PRE: None
-- POST: Parser of bracketed expressions. Parser removes whitespace and throws away brackets.
bracketedExpr :: Parser Expr
bracketedExpr
  = bracket (punctuation '(') parseExpr (punctuation ')')

-- PRE: None
-- POST: Parses references to array elements.
-- Example usage: parse arrayElem "abc[1][2]" will return ArrayElem "abc" [IntLit 1, IntLit 2]
arrayElem :: Parser ArrayElem
arrayElem
  = ArrayElem <$> identifier <*> some (bracket (punctuation '[') parseExpr (punctuation ']'))

-- PRE: None
-- POST: Wraps parsed array elements in appropriate data constructor.
arrayElemExpr :: Parser Expr
arrayElemExpr
  = ExprArray <$> arrayElem

-- PRE: None
-- POST: Parses a list of expressions.
parseExprList :: Char -> Char -> Parser [Expr]
parseExprList open close
  = bracket (punctuation open) (sepby parseExpr (punctuation ',')) (punctuation close)
