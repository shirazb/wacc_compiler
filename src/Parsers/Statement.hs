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
import Debug.Trace

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
  <|> parseBuiltInFunc "println" Println
  <|> parseBuiltInFunc "print"   Print
  <|> parseIfStat
  <|> parseWhileStat
  <|> parseBlock
  <|> parseSkip

parseRead :: Parser Stat
parseRead
  = keyword "read" >> (Read <$> parseLHS)

parseBuiltInFunc :: String -> (Expr -> Stat) -> Parser Stat
parseBuiltInFunc funcName func
  = token funcName >> (func <$> parseExpr)

parseIfStat :: Parser Stat
parseIfStat = do
  traceM "We are parsing and if statement"
  keyword "if"
  cond <- parseExpr
  traceM ("The boolean condition is: " ++ show cond)
  keyword "then"
  traceM "We have parsed the then of the if statement"
  thenStat <- parseStatement
  traceM ("The then branch is: " ++ show thenStat)
  keyword "else"
  traceM "we have parsed the else of the if statement"
  elseStat <- parseStatement
  traceM ("the else branch is: " ++ show elseStat)
  keyword "fi"
  return $ If cond thenStat elseStat

parseWhileStat :: Parser Stat
parseWhileStat = do
  keyword "while"
  cond       <- parseExpr
  keyword "do"
  loopBody   <- parseStatement
  keyword "done"
  return $ While cond loopBody

parseBlock :: Parser Stat
parseBlock
  = Begin <$> bracket (keyword "begin") parseStatement (keyword "end")

parseSeq :: Parser Stat
parseSeq = parseStatement' >>= rest
  where
    rest s = (do
      traceM "We are parsing a sequence"
      traceM ("The first thing in the sequence is: " ++ show s)
      punctuation ';'
      traceM "We are about to get the second element in the seq"
      s' <- parseStatement
      traceM ("In the sequence the second statement is:" ++ show s')
      rest $ Seq s s') <|> return s

-- PRE:  None
-- POST: Consumes a "skip" string, returning the Skip statment
parseSkip :: Parser Stat
parseSkip
  = keyword "skip" >> return Skip

parseDeclaration :: Parser Stat
parseDeclaration = do
  varType    <- parseType
  ident      <- identifier
  punctuation '='
  assignRHS  <- parseRHS
  return $ Declaration varType ident assignRHS

parseRHS :: Parser AssignRHS
parseRHS
  =   assignToExpr
  <|> assignToPairElem
  <|> assignToFuncCall
  <|> assignToArrayLit
  <|> assignToNewPair
  


assignToExpr :: Parser AssignRHS
assignToExpr
  = ExprAssign <$> parseExpr

pairElem :: Parser PairElem
pairElem = do
  fstOrSnd  <- keyword "fst" <|> keyword "snd"
  expr      <- parseExpr
  if fstOrSnd == "fst"
    then return (First  expr)
    else return (Second expr)

assignToPairElem :: Parser AssignRHS
assignToPairElem
  = PairElemAssign <$> pairElem

assignToFuncCall :: Parser AssignRHS
assignToFuncCall = do
  keyword "call"
  name     <- identifier
  arglist  <- parseExprList '(' ')'
  return $ FuncCallAssign name arglist

assignToArrayLit :: Parser AssignRHS
assignToArrayLit
  = ArrayLitAssign <$> parseExprList '[' ']'

assignToNewPair :: Parser AssignRHS
assignToNewPair = do
  token "newpair"
  punctuation '('
  expr1 <- parseExpr
  punctuation ','
  expr2 <- parseExpr
  punctuation ')'
  return $ NewPairAssign expr1 expr2

parseAssignment :: Parser Stat
parseAssignment = do
  traceM "We are parsing the assignment"
  lhs <- parseLHS
  traceM "we are succesfully continuuing to parse the assignment"
  traceM ("The lhs in the assignment is: " ++ show lhs)
  punctuation '='
  rhs <- parseRHS
  traceM ("The rhs in the assignment is: " ++ show rhs)
  return $ Assignment lhs rhs

parseLHS :: Parser AssignLHS
parseLHS
  =   ArrayDeref <$> arrayElem
  <|> PairDeref  <$> pairElem
  <|> Var        <$> identifier
