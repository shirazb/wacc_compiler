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

arrayElem :: Parser ArrayElem
arrayElem =  ArrayElem <$> identifier <*> some (bracket (char '[') parseExpr (char ']'))

arrayElemExpr :: Parser Expr
arrayElemExpr
  = ExprArray <$> arrayElem

binaryExpr :: Parser Expr
binaryExpr = lowBinaryExpr

parseExpr' :: Parser Expr
parseExpr' =
      arrayElemExpr
  <|> unaryExpr
  <|> bracketedExpr
  <|> charLiteral
  <|> boolLiteral
  <|> stringLiter
  <|> pairLiteral
  <|> exprIdent
  <|> intLiteral

parseExpr :: Parser Expr
parseExpr = binaryExpr <|> parseExpr'

-- PRE:  None
-- POST: Source code is a valid statement <-> parse parseStatement parses statement into AST
parseStatement :: Parser Stat
parseStatement = error "TODO"

-- PRE:  None
-- POST: Consumes a "skip" string, returning the Skip statment
parseSkip :: Parser Stat
parseSkip = string "skip" >> return Skip


parseType :: Parser Type
parseType =
      PairT <$>  parsePairType
  <|> BaseT <$> parseBaseType
--   <|> ArrayT <$> parseArrayType

parseBaseType :: Parser BaseType
parseBaseType = parseFromMap baseTypes

parseArrayType :: Parser ArrayType
parseArrayType = do
  t <- parseType  -- TODO: FIX LEFT RECURSION HERE
  string "[]"
  return t

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
  = parseNestedPairType <|> (BaseP <$> parseBaseType) <|> (ArrayP <$> parseArrayType)


parseDeclaration :: Parser Stat
parseDeclaration = do
  varType <- parseType
  ident   <- identifier
  char '=' --TODO: FIX WHITESPACE
  assignRHS     <- parseRHS
  return $ Declaration varType ident assignRHS

parseRHS :: Parser AssignRHS
parseRHS
  =   assignToNewPair
  <|> assignToPairElem
  <|> assignToFuncCall
  <|> assignToArrayLit
  <|> assignToExpr

assignToExpr :: Parser AssignRHS
assignToExpr = ExprAssign <$> parseExpr

pairElem :: Parser PairElem
pairElem = do
  fstOrSnd <- string "fst" <|> string "snd"
  expr    <- parseExpr
  if fstOrSnd == "fst"
    then return $ First expr
    else return $ Second expr

assignToPairElem :: Parser AssignRHS
assignToPairElem
  = PairElemAssign <$> pairElem

assignToFuncCall :: Parser AssignRHS
assignToFuncCall = do
  string "call"
  name <- identifier
  arglist <- parseExprList '(' ')'
  return $ FuncCallAssign name arglist

assignToArrayLit :: Parser AssignRHS
assignToArrayLit = ArrayLitAssign <$> parseExprList '[' ']'

assignToNewPair :: Parser AssignRHS
assignToNewPair = do
  string "newpair"
  char '('
  expr1 <- parseExpr
  char ','
  expr2 <- parseExpr
  char ')'
  return $ NewPairAssign expr1 expr2

parseExprList :: Char -> Char -> Parser [Expr]
parseExprList open close
  = bracket (char open) (sepby parseExpr (char ',')) (char close)

parseAssignment :: Parser Stat
parseAssignment = do
    lhs <- parseLHS
    char '='
    rhs <- parseRHS
    return $ Assignment lhs rhs

parseLHS :: Parser AssignLHS
parseLHS
  =   ArrayDeref <$> arrayElem
  <|> PairDeref  <$> pairElem
  <|> Var        <$> identifier
