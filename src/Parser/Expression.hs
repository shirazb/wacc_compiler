{- This module defines parser combinators which are used to parse expressions.
A number of basic expression combinators have been defined which are then used
as building blocks to build more complex parsers of expressions -}

{-# LANGUAGE MultiWayIf #-}

module Parser.Expression (
  parseExpr,
  parseExprList,
  arrayElem,
  parseMemberAccess,
  parseFuncCall
) where

import Control.Applicative  ((<*>), (<|>), some, many)
import Control.Monad.Except (throwError)
import Data.Maybe           (fromJust)
import Data.List

{- LOCAL IMPORTS -}
import Parser.LexicalResolver
import Parser.Identifier (identifier)
import Parser.BasicCombinators
import Utilities.Definitions

-- POST: Parses valid expressions and generates an error upon failing
-- NOTE: parseExpr avoids left recursion in binary expressions by separating
--       binary expression parser from the other forms of expression parsers
parseExpr :: Parser Char Expr
parseExpr
  = binaryExpr <|> parseExpr'

-- POST: Parses valid expressions
parseExpr' :: Parser Char Expr
parseExpr'
  =   arrayElemExpr
  <|> intLiteral
  <|> unaryExpr
  <|> bracketedExpr
  <|> charLiteral
  <|> boolLiteral
  <|> parseExprMemberAccess
  <|> stringLiter
  <|> exprIdent
  <|> pairLiteral

{- LITERAL COMBINATORS -}

-- POST: Parser for integers
-- NOTE: Can be optionally signed
intLiteral :: Parser Char Expr
intLiteral = trimWS $ do
  pos  <- getPosition
  sign <- string "-" <|> string "+" <|> return []
  num  <- some digit
  let n = if sign == "-" then negate (read num) else read num
  if | (n > maxInt || n < minInt) ->
          throwError ("Syntax: Int Overflow", updateRowPosition pos)
     | otherwise -> return $ IntLit n pos

-- POST: Parser for booleans
boolLiteral :: Parser Char Expr
boolLiteral = do
  pos     <- getPosition
  boolean <- keyword "true" <|> keyword "false"
  if | boolean == "true" -> return (BoolLit True pos)
     | otherwise -> return (BoolLit False pos)

-- POST: Parses input string after removing opening and closing delimiters
quoted :: Char -> Parser Char b -> Parser Char b
quoted c parser
  = bracket (char c) parser (char c)

-- POST: Parser for chars
charLiteral :: Parser Char Expr
charLiteral = do
  pos  <- getPosition
  cLit <- quoted '\''
      (require character "Invalid character found")
  return (CharLit cLit pos)

-- POST: Parser for pairs
pairLiteral :: Parser Char Expr
pairLiteral = do
  pos <- getPosition
  keyword "null"
  return $ PairLiteral pos

-- POST: Parses an expression identifier
exprIdent :: Parser Char Expr
exprIdent = do
  pos    <- getPosition
  ident  <- identifier
  return $ IdentE ident pos

-- POST: Parses a string literal
stringLiter :: Parser Char Expr
stringLiter = do
  pos    <- getPosition
  string <- quoted '\"' (require (many character)
            "Invalid char found in string")
  return $ StringLit string pos

{- UNARY & BINARY EXPRESSION COMBINATORS -}

-- POST: Parses all valid application of unary operators expressions
unaryExpr :: Parser Char Expr
unaryExpr
  = parseUnaryAppHigh <|> parseUnaryAppLow

-- POST: Parses unary operators with low precedence
parseUnaryAppLow :: Parser Char Expr
parseUnaryAppLow = do
  pos     <- getPosition
  op      <- foldr1 (<|>) (map (keyword . fst) unOpPrec2)
  let op' = fromJust $ lookup op unOpPrec2
  expr    <- require parseExpr "Invalid argument to unary operator"
  return $ UnaryApp op' expr pos

-- POST: Parses unary operators with high precedence
parseUnaryAppHigh :: Parser Char Expr
parseUnaryAppHigh = do
  pos  <- getPosition
  op   <- parseFromMap unOpPrec1
  expr <- require parseExpr "Invalid argument to unary operator"
  return $ UnaryApp op expr pos

-- POST:    Parses all valid binary expressions
-- EXAMPLE: parse  binaryExpr "1 + 2" will return
--          BinaryApp Mul (IntLit 1) (IntLit 2)
binaryExpr  :: Parser Char Expr
binaryExpr
  = prec5Binary `chainl1` parseBinOpPrec6

-- POST: Parses the binary expressions with precedence level 5 left
--       associatively
prec5Binary :: Parser Char Expr
prec5Binary
  = prec4Binary `chainl1` parseBinOpPrec5

-- POST: Parses the binary expressions with precedence level 4 left
--       associatively
prec4Binary :: Parser Char Expr
prec4Binary
  = prec3Binary `chainl1` parseBinOpPrec4

-- POST: Parses the binary expressions with precedence level 3 left
--       associatively
prec3Binary :: Parser Char Expr
prec3Binary
  = prec2Binary `chainl1` parseBinOpPrec3

-- POST: Parses the binary expressions with precedence level 2 left
--       associatively
prec2Binary :: Parser Char Expr
prec2Binary
  = prec1Binary `chainl1` parseBinOpPrec2

-- POST: Parses the binary expressions with precedence level 1 left
--       associatively
prec1Binary :: Parser Char Expr
prec1Binary
  = parseExpr' `chainl1` parseBinOpPrec1

-- POST: Parses binary operators of precedence level 1
parseBinOpPrec1 :: Parser Char BinOp
parseBinOpPrec1
  = parseFromMap binOpPrec1

-- POST: Parses binary operators of precedence level 2
parseBinOpPrec2 :: Parser Char BinOp
parseBinOpPrec2
  = parseFromMap binOpPrec2

-- POST: Parses binary operators of precedence level 3
parseBinOpPrec3 :: Parser Char BinOp
parseBinOpPrec3
  = parseFromMap binOpPrec3

-- POST: Parses binary operators of precedence level 4
parseBinOpPrec4 :: Parser Char BinOp
parseBinOpPrec4
  = parseFromMap binOpPrec4

-- POST: Parses binary operators of precedence level 5
parseBinOpPrec5 :: Parser Char BinOp
parseBinOpPrec5
  = parseFromMap binOpPrec5

-- POST: Parses binary operators of precedence level 6
parseBinOpPrec6 :: Parser Char BinOp
parseBinOpPrec6
  = parseFromMap binOpPrec6

{- HELPER FUNCTIONS -}

-- POST:    Takes a parser of expressions and a parser of binary opterators and
--          parsers them in a left associative fashion
-- EXAMPLE: parse (chainl1 intLiteral parseAdd) "1 + 2 + 3" will return
--          Add (Add (IntLit 1) (IntLit 2)) (IntLit 3)
chainl1 :: Parser Char Expr -> Parser Char BinOp -> Parser Char Expr
chainl1 p op
  = trimWS (p >>= rest)
  where
    rest x = (do
      pos  <- getPosition
      f    <- op
      y    <- require p "Invalid argument to binary expression"
      rest $ BinaryApp f x y pos) <|> return x

-- POST: Takes a parser of expressions and a parser of binary opterators and
--       parsers them in a right associative fashion
chainr :: Parser Char Expr -> Parser Char BinOp -> Parser Char Expr
chainr p op
  = p >>= rest
  where
    rest x = (do
      pos <- getPosition
      f   <- op
      xs  <- chainr p op
      rest $ BinaryApp f x xs pos
      ) <|> return x

-- POST: Parser of bracketed expressions. Parser removes whitespace and throws
--       away brackets
bracketedExpr :: Parser Char Expr
bracketedExpr
  = bracket
      (punctuation '(')
      (require parseExpr "Invalid Expression in brackets")
      (require (punctuation ')')
          "Missing closing parenthesis to bracketed expression")

-- POST:    Parses references to array elements
-- EXAMPLE: parse arrayElem "abc[1][2]" will return
--          ArrayElem "abc" [IntLit 1, IntLit 2]
arrayElem :: Parser Char ArrayElem
arrayElem = do
  pos     <- getPosition
  ident   <- identifier
  indexes <- arrayIndexes
  return $ ArrayElem ident indexes pos

-- POST: Parses the array indexers e.g. [1][2]
arrayIndexes :: Parser Char [Expr]
arrayIndexes
  = some (bracket (punctuation '[') parseExpr (punctuation ']'))

-- POST: Wraps parsed array elements in appropriate data constructor
arrayElemExpr :: Parser Char Expr
arrayElemExpr = do
  pos      <- getPosition
  element  <- arrayElem
  return $ ExprArray element pos

-- POST: Parses a list of expressions
parseExprList :: Char -> Char -> Parser Char [Expr]
parseExprList open close
  = bracket
      (punctuation open)
      (sepby parseExpr (punctuation ','))
      (punctuation close)

-- POST: Parses a member access as an expression
parseExprMemberAccess :: Parser Char Expr
parseExprMemberAccess = do
  pos       <- getPosition
  memAccess <- parseMemberAccess
  return $ ExprMemberAccess memAccess pos

{- The following functions help us with parsing member accesses -}

-- POST: Parses a member access, differentiating whether the obj whose member
--       is a variable or the result of a method call.
parseMemberAccess :: Parser Char MemberAccess
parseMemberAccess = do
  pos        <- getPosition
  object     <- parseInstance
  punctuation '.'
  firstMem   <- parseMember
  remMembers <- many $ punctuation '.' *> parseMember
  return $ MemList object (firstMem : remMembers) pos

-- POST: Parses the object on which a member is accessed
parseInstance :: Parser Char Instance
parseInstance = do
  pos  <- getPosition
  inst <- (FuncReturnsObj <$> parseFuncCall) <|> (VarObj <$> identifier)
  return $ inst pos

-- POST: Parses a list of member accesses
-- Example Usage: Parse something of the form x.z.y
parseMemList :: Parser Char [Member]
parseMemList
  = many $ punctuation '.' *> parseMember

--POST: Parses either a method call or a field access
parseMember :: Parser Char Member
parseMember
  =   parseClassMethod
  <|> parseFieldMember

-- POST: Parses a field
parseFieldMember :: Parser Char Member
parseFieldMember = do
  pos       <- getPosition
  fieldName <- identifier
  return $ FieldAccess fieldName pos

-- POST: Parses a method call
parseClassMethod :: Parser Char Member
parseClassMethod = do
  pos <- getPosition
  fc  <- parseFuncCall
  return $ MethodCall fc pos

-- POST: Parses a function call
parseFuncCall :: Parser Char FuncCall
parseFuncCall = do
  ident <- identifier
  es    <- parseExprList '(' ')'
  return $ FuncCall ident es
