---
-- Statement parsing
---
module Parsers.Statement (parseStatement) where

import Control.Applicative

import Parsers.Expression
import Parsers.Lexer
import Parsers.Type
import Utility.BasicCombinators
import Utility.Declarations
import Utility.Definitions

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
