module Parser (parseProgram) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import Debug.Trace

{- Local Imports -}

import Utility.Definitions
import Utility.BasicCombinators
import Utility.Declarations

parseProgram :: Parser Program
parseProgram = do
  string "begin"
  funcs <- many parseFunc
  main  <- parseStatement
  string "end"
  return $ Program funcs main

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Expression Parsing

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

ident :: Parser String
ident
  = liftA2 (:) (char '_' <|> letter) (many (alphanum <|> char '_'))

identifier :: Parser String
identifier = do
  x <- ident
  guard (x `notElem` keywords)
  return x

exprIdent :: Parser Expr
exprIdent
  = IdentE <$> identifier

stringLiter :: Parser Expr
stringLiter
  = StringLit <$> bracket (char '\"') (many character) (char '\"')

-- we could use an actual MAP from Data.Map
parseFromMap :: [(String, a)] -> Parser a
parseFromMap assoclist = do
  value <- foldr1 (<|>) (map (string.fst) assoclist)
  return $ fromJust (lookup value assoclist)

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

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Type Parsing

parseType :: Parser Type
parseType
  =   ArrayT <$> parseArrayType
  <|> PairT  <$>  parsePairType
  <|> BaseT  <$> parseBaseType

parseBaseType :: Parser BaseType
parseBaseType
  = parseFromMap baseTypes

multiDimArray :: Parser (ArrayType -> Type)
multiDimArray
  = string "[]" >> rest ArrayT
  where
    rest x = (do
      string "[]"
      rest (ArrayT . x)) <|> return x

parseArrayType :: Parser ArrayType
parseArrayType = do
  t          <- (BaseT <$> parseBaseType) <|> (PairT <$> parsePairType)
  dimension  <- multiDimArray
  return (dimension t)

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
parseNestedPairType
  = string "pair" >> return Pair

parsePairElemType :: Parser PairElemType
parsePairElemType
  =   parseNestedPairType
  <|> ArrayP <$> parseArrayType
  <|> BaseP  <$> parseBaseType

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Statement Parsing

-- PRE:  None
-- POST: Source code is a valid statement <-> parses source code into Stat type
parseStatement :: Parser Stat
parseStatement
  = parseSeq <|> parseStatement'

parseStatement' :: Parser Stat
parseStatement'
  =   parseDeclaration
  <|> parseAssignment
  <|> parseRead
  <|> parseBuiltInFunc "free"    Free
  <|> parseBuiltInFunc "return"  Return
  <|> parseBuiltInFunc "exit"    Exit
  <|> parseBuiltInFunc "print"   Print
  <|> parseBuiltInFunc "println" Println
  <|> parseIfStat
  <|> parseWhileStat
  <|> parseBlock
  <|> parseSkip

parseRead :: Parser Stat
parseRead
  = string "read" >> (Read <$> parseLHS)

parseBuiltInFunc :: String -> (Expr -> Stat) -> Parser Stat
parseBuiltInFunc funcName func
  = string funcName >> (func <$> parseExpr)

parseIfStat :: Parser Stat
parseIfStat = do
  string "if"
  cond       <- parseExpr
  string "then"
  thenStat   <- parseStatement
  string "else"
  elseStat   <- parseStatement
  string "fi"
  return $ If cond thenStat elseStat

parseWhileStat :: Parser Stat
parseWhileStat = do
  string "while"
  cond       <- parseExpr
  string "do"
  loopBody   <- parseStatement
  string "done"
  return $ While cond loopBody

parseBlock :: Parser Stat
parseBlock
  = Begin <$> bracket (string "begin") parseStatement (string "end")

parseSeq :: Parser Stat
parseSeq = parseStatement' >>= rest
  where
    rest s = (do
      char ';'
      s' <- parseStatement
      rest $ Seq s s') <|> return s

-- PRE:  None
-- POST: Consumes a "skip" string, returning the Skip statment
parseSkip :: Parser Stat
parseSkip
  = string "skip" >> return Skip

parseDeclaration :: Parser Stat
parseDeclaration = do
  varType    <- parseType
  ident      <- identifier
  char '=' --TODO: FIX WHITESPACE
  assignRHS  <- parseRHS
  return $ Declaration varType ident assignRHS

parseRHS :: Parser AssignRHS
parseRHS
  =   assignToNewPair
  <|> assignToPairElem
  <|> assignToFuncCall
  <|> assignToArrayLit
  <|> assignToExpr

assignToExpr :: Parser AssignRHS
assignToExpr
  = ExprAssign <$> parseExpr

pairElem :: Parser PairElem
pairElem = do
  fstOrSnd  <- string "fst" <|> string "snd"
  expr      <- parseExpr
  if fstOrSnd == "fst"
    then return (First  expr)
    else return (Second expr)

assignToPairElem :: Parser AssignRHS
assignToPairElem
  = PairElemAssign <$> pairElem

assignToFuncCall :: Parser AssignRHS
assignToFuncCall = do
  string "call"
  name     <- identifier
  arglist  <- parseExprList '(' ')'
  return $ FuncCallAssign name arglist

assignToArrayLit :: Parser AssignRHS
assignToArrayLit
  = ArrayLitAssign <$> parseExprList '[' ']'

assignToNewPair :: Parser AssignRHS
assignToNewPair = do
  string "newpair"
  char '('
  expr1 <- parseExpr
  char ','
  expr2 <- parseExpr
  char ')'
  return $ NewPairAssign expr1 expr2

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

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Parsing Functions

parseFunc :: Parser Func
parseFunc = do
  returnType <- parseType
  name       <- identifier
  paramList  <- bracket (char '(') parseParamList (char ')')
  string "is"
  funcBody   <- parseStatement
  string "end"
  return $ Func returnType name paramList funcBody

-- ISSUE: Can have comma in pair types!
parseParamList :: Parser ParamList
parseParamList
  = ParamList <$> sepby' parseParam (char ',')

parseParam :: Parser Param
parseParam
  = liftM2 Param parseType identifier

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Lexical Issues

comments :: Parser ()
comments
  = void $ char '#' >> many (satisfy (/= '\n')) >> char '\n'

spacesAndComments :: Parser ()
spacesAndComments
  = void $ many (spaces <|> comments)

leadWSC :: Parser a -> Parser a
leadWSC p
  = spacesAndComments >> p

token :: Parser a -> Parser a
token p = do
  parsedValue <- p
  spacesAndComments
  return parsedValue

keyword :: String -> Parser String
keyword xs
  = token (string xs)

identifiers :: Parser String
identifiers
  = token identifier
