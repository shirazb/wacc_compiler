{-
This module consists of parser combinators which are used to parse
expressions in the WACC language. A number of basic expression
combinators have been defined which are then used as building blocks
to build more complex parsers of expressions. Refer to BNF spec of WACC
language to see exactly what an expression is in the WACC language.
-}

module Parser.Expression (parseExpr, parseExpr' , parseExprList, arrayElem, binaryExpr, chainr, parseBinaryOpLow) where

import Control.Applicative
import Control.Monad
import Data.Maybe

import Parser.Lexer
import Parser.Combinators
import Utilities.Declarations
import Utilities.Definitions

-- PRE:  None
-- POST: Parses all valid expressions in the WACC language, it is factored out like
-- this to prevent the parser going in to an infinite loop due to left recursion.
parseExpr :: Parser Char Expr
parseExpr
  = binaryExpr <|> parseExpr'


parseExpr' :: Parser Char Expr
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

{- BASIC COMBINATORS: -}
-- Used to parse atomic expressions

intLiteral :: Parser Char Expr
intLiteral
  = trimWS $ (IntLit . read) <$> some digit

boolLiteral :: Parser Char Expr
boolLiteral
  = do
      boolean <- keyword "true" <|> keyword "false"
      if boolean == "true"
        then return (BoolLit True)
        else return (BoolLit False)

quoted c p
  = bracket (char c) p (char c)

charLiteral :: Parser Char Expr
charLiteral
  = CharLit <$> quoted '\''
      (tryParser character "Invalid character found")

pairLiteral :: Parser Char Expr
pairLiteral
  = keyword "null" >> return PairLiteral

exprIdent :: Parser Char Expr
exprIdent
  = IdentE <$> identifier

stringLiter :: Parser Char Expr
stringLiter
  = StringLit <$> quoted '\"'
      (tryParser (many character) "Invalid char found in string")

{-
Complex combinators used to parse larger and more complex expressions.
They are built using the basic combinators defined above and a few
generic combinators defined in the BasicCombinators module.
-}

-- PRE: None
-- POST: Parses all valid application of unary operators expressions.
unaryExpr :: Parser Char Expr
unaryExpr
  = parseUnaryAppHigh <|> parseUnaryAppLow

parseUnaryAppLow :: Parser Char Expr
parseUnaryAppLow = do
  op      <- foldr1 (<|>) (map (keyword . fst) unOpAssoc)
  let op = fromJust $ lookup op unOpAssoc
  expr    <- tryParser parseExpr "Invalid argument to unary operator"
  return $ UnaryApp op expr

parseUnaryAppHigh :: Parser Char Expr
parseUnaryAppHigh = do
  op   <- parseFromMap unOpAssocHigher
  expr <- tryParser parseExpr' "Invalid argument to unary operator"
  return $ UnaryApp op expr

{-
A number of parsers used to parse valid binary expressions in the WACC
language. The design of the parser combinators take in to acccount the
precdence of binary operators.
-}

-- PRE: None
-- POST: Parses all valid binary expressions
-- Example: parse  binaryExpr "1 + 2" will return
-- BinaryApp Mul (IntLit 1) (IntLit 2)
binaryExpr :: Parser Char Expr
binaryExpr
  = highBinaryExpr `chainl1` parseBinaryOpLow

highBinaryExpr :: Parser Char Expr
highBinaryExpr
  = higherBinaryExpr `chainl1` parseBinaryOpHigh

higherBinaryExpr :: Parser Char Expr
higherBinaryExpr
  = parseExpr' `chainl1` parseBinaryOpHigher

parseBinaryOpLow :: Parser Char BinOp
parseBinaryOpLow
  = parseFromMap lowBinOps

parseBinaryOpHigh :: Parser Char BinOp
parseBinaryOpHigh
  = parseFromMap highBinOps

parseBinaryOpHigher :: Parser Char BinOp
parseBinaryOpHigher
  = parseFromMap higherBinOps

-- PRE: None
-- POST: Returns a parser which parses a sequence of expressions seperated by
-- a meaningful seperator, for example, the operator (+). The parser returns
-- the expression wrapped up in the appropriate data constructors. Assume
-- existence of parser which returns the Add data constructor as its result,
-- call it parseAdd.
-- Example: parse (chainl1 intLiteral parseAdd) "1 + 2 + 3" will return
-- Add (Add (IntLit 1) (IntLit 2)) (IntLit 3)
chainl1 :: Parser Char Expr -> Parser Char BinOp -> Parser Char Expr
chainl1 p op
  = trimWS $ p >>= rest
  where
    rest x = (do
      f <- op
      y <- tryParser p "Invalid argument to binary expression"
      rest $ BinaryApp f x y) <|> return x

chainr :: Parser Char Expr -> Parser Char BinOp -> Parser Char Expr
chainr p op
  = p >>= rest
  where
    rest x = (do
      f <- op
      xs <- chainr p op
      rest $ BinaryApp f x xs
      ) <|> return x

-- PRE: None
-- POST: Parser of bracketed expressions. Parser removes whitespace and throws
-- away brackets.
bracketedExpr :: Parser Char Expr
bracketedExpr
  = bracket (punctuation '(') (tryParser parseExpr "Invalid Expression in brackets") (tryParser (punctuation ')') "Missing closing parenthesis to bracketed expression")

-- PRE: None
-- POST: Parses references to array elements.
-- Example: parse arrayElem "abc[1][2]" will return
-- ArrayElem "abc" [IntLit 1, IntLit 2]
arrayElem :: Parser Char ArrayElem
arrayElem
  = ArrayElem <$> identifier <*> some (bracket (punctuation '[') parseExpr (punctuation ']'))

-- PRE: None
-- POST: Wraps parsed array elements in appropriate data constructor.
arrayElemExpr :: Parser Char Expr
arrayElemExpr
  = ExprArray <$> arrayElem

-- PRE: None
-- POST: Parses a list of expressions.
parseExprList :: Char -> Char -> Parser Char [Expr]
parseExprList open close
  = bracket (punctuation open) (sepby parseExpr (punctuation ',')) (punctuation close)
