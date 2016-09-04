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
  = token $ leadWSC $ parseSeq <|> parseStatement'

parseStatement' :: Parser Stat
parseStatement'
  =  token $ leadWSC (parseDeclaration
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
  <|> parseSkip)

parseRead :: Parser Stat
parseRead
  = token $ leadWSC $ string "read" >> (Read <$> parseLHS)

parseBuiltInFunc :: String -> (Expr -> Stat) -> Parser Stat
parseBuiltInFunc funcName func
  = token $ leadWSC $ string funcName >> (func <$> parseExpr)

parseIfStat :: Parser Stat
parseIfStat = token $ leadWSC (do
  string "if"
  cond       <- parseExpr
  string "then"
  thenStat   <- parseStatement
  string "else"
  elseStat   <- parseStatement
  string "fi"
  return $ If cond thenStat elseStat)

parseWhileStat :: Parser Stat
parseWhileStat = token $ leadWSC (do
  string "while"
  cond       <- parseExpr
  string "do"
  loopBody   <- parseStatement
  string "done"
  return $ While cond loopBody)

parseBlock :: Parser Stat
parseBlock
  = token $ leadWSC $ Begin <$> bracket (string "begin") parseStatement (string "end")

parseSeq :: Parser Stat
parseSeq = token $ leadWSC $ parseStatement' >>= rest
  where
    rest s = (do
      char ';'
      s' <- parseStatement
      rest $ Seq s s') <|> return s

-- PRE:  None
-- POST: Consumes a "skip" string, returning the Skip statment
parseSkip :: Parser Stat
parseSkip
  = token $ leadWSC $ string "skip" >> return Skip

parseDeclaration :: Parser Stat
parseDeclaration = token $ leadWSC (do
  varType    <- parseType
  ident      <- identifier
  char '=' --TODO: FIX WHITESPACE
  assignRHS  <- parseRHS
  return $ Declaration varType ident assignRHS)

parseRHS :: Parser AssignRHS
parseRHS
  =  token $ leadWSC (assignToNewPair
  <|> assignToPairElem
  <|> assignToFuncCall
  <|> assignToArrayLit
  <|> assignToExpr)

assignToExpr :: Parser AssignRHS
assignToExpr
  =  token $ leadWSC $ ExprAssign <$> parseExpr

pairElem :: Parser PairElem
pairElem = token $ leadWSC $ do
  fstOrSnd  <- string "fst" <|> string "snd"
  expr      <- parseExpr
  if fstOrSnd == "fst"
    then return (First  expr)
    else return (Second expr)

assignToPairElem :: Parser AssignRHS
assignToPairElem
  = token $ leadWSC $ PairElemAssign <$> pairElem

assignToFuncCall :: Parser AssignRHS
assignToFuncCall = token $ leadWSC $ do
  string "call"
  name     <- identifier
  arglist  <- parseExprList '(' ')'
  return $ FuncCallAssign name arglist

assignToArrayLit :: Parser AssignRHS
assignToArrayLit
  = token $ leadWSC $ ArrayLitAssign <$> parseExprList '[' ']'

assignToNewPair :: Parser AssignRHS
assignToNewPair = token $ leadWSC (do
  string "newpair"
  char '('
  expr1 <- parseExpr
  char ','
  expr2 <- parseExpr
  char ')'
  return $ NewPairAssign expr1 expr2)

parseAssignment :: Parser Stat
parseAssignment = token $ leadWSC (do
  lhs <- parseLHS
  char '='
  rhs <- parseRHS
  return $ Assignment lhs rhs)

parseLHS :: Parser AssignLHS
parseLHS
  = token $ leadWSC (ArrayDeref <$> arrayElem
  <|> PairDeref  <$> pairElem
  <|> Var        <$> identifier)
