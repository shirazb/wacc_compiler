{-
This module consists of parser combinators which are used to parse
expressions in the WACC language. A number of basic expression
combinators have been defined which are then used as building blocks
to build more complex parsers of expressions. Refer to BNF spec of WACC
language to see exactly what an expression is in the WACC language.
-}

{-# LANGUAGE MultiWayIf #-}

module Parser.Expression (
  parseExpr,
  parseExprList,
  arrayElem
) where

import Control.Applicative ((<*>), (<|>), some, many, liftA2, liftA3)
import Control.Monad.Except (throwError)
import Data.Maybe (fromJust)
import Parser.Lexer
import Parser.Identifier
import Parser.Combinators
import Utilities.Definitions

-- NB:   parseExpr avoids left recursion in binary expressions by separating
--       binary expression parser from the other forms of expression parsers.
-- POST: Parses valid expressions, generates an error upon failing.
parseExpr :: Parser Char Expr
parseExpr
  = binaryExpr <|> parseExpr'

parseExpr' :: Parser Char Expr
parseExpr'
  =   arrayElemExpr
  <|> intLiteral
  <|> unaryExpr
  <|> bracketedExpr
  <|> charLiteral
  <|> boolLiteral
  <|> stringLiter
  <|> exprIdent
  <|> pairLiteral

{- LITERAL COMBINATORS: -}

-- can be optionally signed.
intLiteral :: Parser Char Expr
intLiteral = trimWS $ do
  sign <- string "-" <|> string "+" <|> return []
  num  <- some digit
  pos  <- getPosition
  let n = if sign == "-" then negate (read num) else read num
  if | (n > maxInt || n < minInt) ->
          throwError ("Syntax: Int Overflow", updateRowPosition pos)
     | otherwise -> return $ IntLit n pos

boolLiteral :: Parser Char Expr
boolLiteral = do
  boolean <- keyword "true" <|> keyword "false"
  pos     <- getPosition
  if | boolean == "true" -> return (BoolLit True pos)
     | otherwise -> return (BoolLit False pos)

quoted :: Char -> Parser Char b -> Parser Char b
quoted c parser
  = bracket (char c) parser (char c)

charLiteral :: Parser Char Expr
charLiteral = do
  cLit <- quoted '\''
      (require character "Invalid character found")
  pos <- getPosition
  return (CharLit cLit pos)

pairLiteral :: Parser Char Expr
pairLiteral
  = keyword "null" >> PairLiteral <$> getPosition

exprIdent :: Parser Char Expr
exprIdent
  = liftA2 IdentE identifier getPosition

stringLiter :: Parser Char Expr
stringLiter
  = liftA2 StringLit
      (quoted '\"' (require (many character) "Invalid char found in string"))
      getPosition

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
  op      <- foldr1 (<|>) (map (keyword . fst) unOpPrec2)
  let op' = fromJust $ lookup op unOpPrec2
  expr    <- require parseExpr "Invalid argument to unary operator"
  pos     <- getPosition
  return $ UnaryApp op' expr pos

parseUnaryAppHigh :: Parser Char Expr
parseUnaryAppHigh = do
  op   <- parseFromMap unOpPrec1
  expr <- require parseExpr "Invalid argument to unary operator"
  pos  <- getPosition
  return $ UnaryApp op expr pos

{-
  A number of parsers used to parse valid binary expressions in the WACC
  language. The design of the parser combinators take in to acccount the
  precdence of binary operators.
-}

-- POST:    Parses all valid binary expressions
-- EXAMPLE: parse  binaryExpr "1 + 2" will return
--          BinaryApp Mul (IntLit 1) (IntLit 2)
binaryExpr  :: Parser Char Expr
binaryExpr
  = prec5Binary `chainl1` parseBinOpPrec6

prec5Binary :: Parser Char Expr
prec5Binary
  = prec4Binary `chainl1` parseBinOpPrec5

prec4Binary :: Parser Char Expr
prec4Binary
  = prec3Binary `chainl1` parseBinOpPrec4

prec3Binary :: Parser Char Expr
prec3Binary
  = prec2Binary `chainl1` parseBinOpPrec3

prec2Binary :: Parser Char Expr
prec2Binary
  = prec1Binary `chainl1` parseBinOpPrec2

prec1Binary :: Parser Char Expr
prec1Binary
  = parseExpr' `chainl1` parseBinOpPrec1

parseBinOpPrec1 :: Parser Char BinOp
parseBinOpPrec1
  = parseFromMap binOpPrec1

parseBinOpPrec2 :: Parser Char BinOp
parseBinOpPrec2
  = parseFromMap binOpPrec2

parseBinOpPrec3 :: Parser Char BinOp
parseBinOpPrec3
  = parseFromMap binOpPrec3

parseBinOpPrec4 :: Parser Char BinOp
parseBinOpPrec4
  = parseFromMap binOpPrec4

parseBinOpPrec5 :: Parser Char BinOp
parseBinOpPrec5
  = parseFromMap binOpPrec5

parseBinOpPrec6 :: Parser Char BinOp
parseBinOpPrec6
  = parseFromMap binOpPrec6

-- POST:    Returns a parser which parses a sequence of expressions seperated
--          by a meaningful seperator, for example, the operator (+). The
--          parser returns the expression wrapped up in the appropriate data
--          constructors. Assume existence of parser which returns the Add data
--          constructor as its result, call it parseAdd.
-- EXAMPLE: parse (chainl1 intLiteral parseAdd) "1 + 2 + 3" will return
--          Add (Add (IntLit 1) (IntLit 2)) (IntLit 3)
chainl1 :: Parser Char Expr -> Parser Char BinOp -> Parser Char Expr
chainl1 p op
  = trimWS (p >>= rest)
  where
    rest x = (do
      f    <- op
      y    <- require p "Invalid argument to binary expression"
      pos  <- getPosition
      rest $ BinaryApp f x y pos) <|> return x

chainr :: Parser Char Expr -> Parser Char BinOp -> Parser Char Expr
chainr p op
  = p >>= rest
  where
    rest x = (do
      f <- op
      xs <- chainr p op
      pos <- getPosition
      rest $ BinaryApp f x xs pos
      ) <|> return x

-- POST: Parser of bracketed expressions. Parser removes whitespace and throws
--       away brackets.
bracketedExpr :: Parser Char Expr
bracketedExpr
  = bracket
      (punctuation '(')
      (require parseExpr "Invalid Expression in brackets")
      (require (punctuation ')')
          "Missing closing parenthesis to bracketed expression")

-- POST:    Parses references to array elements.
-- EXAMPLE: parse arrayElem "abc[1][2]" will return
--          ArrayElem "abc" [IntLit 1, IntLit 2]
arrayElem :: Parser Char ArrayElem
arrayElem
  = liftA3 ArrayElem
      identifier
      arrayIndexes
      getPosition

-- POST: Parses the array indexers e.g. [1][2]
arrayIndexes :: Parser Char [Expr]
arrayIndexes
  = some (bracket (punctuation '[') parseExpr (punctuation ']'))

-- POST: Wraps parsed array elements in appropriate data constructor.
arrayElemExpr :: Parser Char Expr
arrayElemExpr
  = liftA2 ExprArray arrayElem getPosition

-- POST: Parses a list of expressions.
parseExprList :: Char -> Char -> Parser Char [Expr]
parseExprList open close
  = bracket
      (punctuation open)
      (sepby parseExpr (punctuation ','))
      (punctuation close)
