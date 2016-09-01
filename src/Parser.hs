module Parser where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import Debug.Trace

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

parseFromMap :: [(String, a)] -> Parser a
parseFromMap ops = do
  op <- foldr1 (<|>) $ map (string.fst) ops
  return $ fromJust $ lookup op ops

parseUnaryOp :: Parser UnOp
parseUnaryOp = parseFromMap unOpAssoc

parseBinaryOpLow :: Parser BinOp
parseBinaryOpLow = parseFromMap lowBinOps

parseBinaryOpHigh :: Parser BinOp
parseBinaryOpHigh = parseFromMap highBinOps

parseBinaryOpHigher :: Parser BinOp
parseBinaryOpHigher = parseFromMap higherBinOps

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

arrayElem :: Parser Expr
arrayElem = ((ExprArray.) . ArrayElem) <$> identifier <*> some (bracket (char '[') parseExpr (char ']'))

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
parseBaseType = parseFromMap baseTypes

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
parseNestedPairType = string "pair" >> return Pair

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
  <|> assignToPairElem
  <|> assignToFuncCall
  <|> assignToArrayLit

assignToExpr :: Parser AssignRHS
assignToExpr = ExprAssign <$> parseExpr

assignToPairElem :: Parser AssignRHS
assignToPairElem = do
  fstOrSnd <- string "fst" <|> string "snd"
  expr    <- parseExpr
  if fstOrSnd == "fst"
    then return $ PairElemAssign $ First expr
    else return $ PairElemAssign $ Second expr


assignToFuncCall :: Parser AssignRHS
assignToFuncCall = do
  string "call"
  name <- identifier
  arglist <- parseArglist
  return $ FuncCallAssign name arglist

parseArglist :: Parser ArgList
parseArglist = ArgList <$> parseExprList '(' ')'

assignToArrayLit :: Parser AssignRHS
assignToArrayLit = ArrayLitAssign <$> parseToArrayLit

assignToNewPair :: Parser AssignRHS
assignToNewPair = assignToNewPair

parseToArrayLit :: Parser ArrayLit
parseToArrayLit = ArrayLit <$> parseExprList '[' ']'

parseExprList :: Char -> Char -> Parser [Expr]
parseExprList open close = bracket (char open) (sepby parseExpr (char ',')) (char close)
