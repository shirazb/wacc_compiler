module Parser where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Debug.Trace

{- Local Imports -}

import Utility.Definitions
import Utility.BasicCombinators
import Utility.Declarations
intLiteral :: Parser Expr
intLiteral = IntLit . read <$> some digit

boolLiteral :: Parser Expr
boolLiteral = do
  boolean <- string "true" <|> string "false"
  if boolean == "true"
    then return $ BoolLit True
    else return $ BoolLit False

charLiteral :: Parser Expr
charLiteral = CharLit <$> bracket (char '\'') character (char '\'')

pairLiteral :: Parser Expr
pairLiteral = string "null" >> return PairLiteral

ident :: Parser String
ident = liftA2 (:) (char '_' <|> letter) (many (alphanum <|> char '_'))

identifier :: Parser String
identifier = do
  x <- ident
  guard $ notElem x keywords
  return x

exprIdent :: Parser Expr
exprIdent = ExprI <$> identifier

spaces :: Parser ()
spaces = void $ many (satisfy isSpace)

stringLiter :: Parser Expr
stringLiter = StringLit <$> bracket (char '\"') (many character) (char '\"')

-- parseOps mindfuck
parseOps :: [String] -> Parser String
parseOps = foldr1 (<|>) . map string

parseUnaryOp :: Parser UnOp
parseUnaryOp = do
  unOp <- parseOps ["!", "-", "len", "ord", "chr"]
  let astOp = fromJust $ lookup unOp unOpAssoc
  return astOp

parseBinaryOpLow :: Parser BinOp
parseBinaryOpLow = do
  binOp <- parseOps ["+", "-", ">=", ">", "<=", "<", "==", "!=", "&&", "||"]
  let astOp = fromJust $ lookup binOp binOps
  return astOp

parseBinaryOpHigh :: Parser BinOp
parseBinaryOpHigh = do
  binOp <- string "*" <|> string "/" <|> string "%"
  let astOp = fromJust $ lookup binOp binOps
  return astOp

parseBinaryOpHigher :: Parser BinOp
parseBinaryOpHigher = do
  binOp <- string "/" <|> string "%"
  let astOp = fromJust $ lookup binOp binOps
  return astOp

chainl1 :: Parser Expr -> Parser BinOp -> Parser Expr
chainl1 p op = do { x <- p; rest x}
  where
    rest x = (do
      f <- op
      y <- p
      rest $ BinaryApp f x y) <|> return x

lowBinaryExpr :: Parser Expr
lowBinaryExpr = highBinaryExpr `chainl1` parseBinaryOpLow

highBinaryExpr :: Parser Expr
highBinaryExpr = higherBinaryExpr `chainl1` parseBinaryOpHigh

higherBinaryExpr :: Parser Expr
higherBinaryExpr = parseExpr' `chainl1` parseBinaryOpHigher

unaryExpr :: Parser Expr
unaryExpr = UnaryApp <$> parseUnaryOp <*> parseExpr

bracketedExpr :: Parser Expr
bracketedExpr = bracket (char '(') parseExpr (char ')')

-- arrayElem :: Parser Expr
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
  <|> exprIdent
  <|> intLiteral

parseExpr = binaryExpr <|> parseExpr'

-- PRE:  None
-- POST: Source code is a valid statement <-> parse parseStatement parses statement into AST
parseStatement = error "TODO"

-- PRE:  None
-- POST: Consumes a "skip" string, returning the Skip statment
parseSkip :: Parser Stat
parseSkip = string "skip" >> return Skip

parseType :: Parser Type
parseType =
  baseToType parseBaseType
  <|> pairToType  parsePairType
  <|> arrayToType parseArrayType

parseBaseType :: Parser BaseType
parseBaseType = do
  baseType <- string "int" <|> string "bool" <|> string "char" <|> string "string"
  let baseT = fromJust $ lookup baseType baseTypes
  return baseT

-- Wraps the parsed BaseType into a Type
baseToType :: Parser BaseType -> Parser Type
baseToType parserBaseType = BaseT <$> parserBaseType

-- Wraps the parsed ArrayType into a Type
arrayToType :: Parser ArrayType -> Parser Type
arrayToType parserArrayType = ArrayT <$> parserArrayType

-- Wraps the parsed ArrayType into a Type
pairToType :: Parser PairType -> Parser Type
pairToType parserPairType = PairT <$> parserPairType

parseArrayType :: Parser ArrayType
parseArrayType
  = bracket (char '[') parseType (char ']')

parsePairType :: Parser PairType
parsePairType = do
  string "pair"
  char '('
  t1 <- parsePairElemType
  char ','   -- TODO: FIX WHITESPACE AROUND COMMA
  t2 <- parsePairElemType
  char ')'
  return $ PairType t1 t2

parseNestedPairType :: Parser PairElemType
parseNestedPairType = do
  string "pair"
  return Pair

parsePairElemType :: Parser PairElemType
parsePairElemType
  = parseNestedPairType <|>
  do { baseType  <- parseBaseType;  return $ BaseP  baseType  } <|>
  do { arrayType <- parseArrayType; return $ ArrayP arrayType }

parseDeclaration :: Parser Stat
parseDeclaration = do
  varType <- parseType
  ident   <- identifier
  char '=' --TODO: FIX WHITESPACE
  assignRHS     <- parseRHS
  return $ Declaration varType ident assignRHS

parseRHS :: Parser AssignRHS
parseRHS
  = assignToExpr
  <|> assignToNewPair
--  <|> assignToPairElem
--  <|> assignToFuncCall
--  <|> assignToArrayLitg

assignToExpr :: Parser AssignRHS
assignToExpr = ExprAssign <$> parseExpr

  {- do
  expr <- parseExpr
  return $ ExprAssign expr -}

-- THIS PATTERN OF USING ANOTHER PARSER THEN WRAPPING ITS RESULT IN A
-- CONSTRUCTOR IS VERY COMMON. CAN WE ABSTRACT IT OUT?
-- POSSIBLY:


{-
assignToArrayLit :: Parser AssignRHS
assignToArrayLit = do
  arrayLit <- parseArrayLit
  return $ ArrayLitAssign arrayLit -}

assignToNewPair :: Parser AssignRHS
assignToNewPair = do
  assignToNewPair
