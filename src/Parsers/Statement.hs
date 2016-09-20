{-
This module defines a number of parser combinators used to parse statements in the WACC language. Refer to the BNF specification of the
WACC language to see exactly what a statement is in the WACC language.
-}
module Parsers.Statement (parseStatement) where

import           Control.Applicative

import           Debug.Trace
import           Parsers.Expression
import           Parsers.Lexer
import           Parsers.Type
import           Utility.BasicCombinators
import           Utility.Declarations
import           Utility.Definitions

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
  keyword "if"
  cond <- parseExpr
  keyword "then"
  thenStat <- parseStatement
  keyword "else"
  elseStat <- parseStatement
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
  -- = Block <$> bracket (keyword "begin") parseStatement (keyword "end")
  = Block <$> do
    keyword "begin"
    traceM "------ parsed begin of block ------"
    s <- parseStatement
    traceM $ "parseStatement returned (" ++ show s ++ ")"
    keyword "end"
    traceM "------ parsed end of block ------"
    return s

parseSeq :: Parser Stat
parseSeq = traceM "parsing initial statement of sequence..." >> parseStatement' >>= \s -> rest s
  where
    rest s = (do
      punctuation ';'
      s' <- parseStatement
      rest $ Seq s s') <|> return s

-- PRE:  None
-- POST: Consumes a "skip" string, returning the Skip statment
parseSkip :: Parser Stat
parseSkip
  = keyword "skip" >> return Skip

parseDeclaration :: Parser Stat
parseDeclaration = do
  traceM  "------ Entered parseDeclaration  ------"
  varType    <- parseType
  traceM $ "parseType returned " ++ show varType
  ident      <- identifier
  traceM $ "identifier returned " ++ show ident
  punctuation '='
  assignRHS  <- parseRHS
  traceM $ "parseRHS returned " ++ show assignRHS
  traceM  "------ Leaving parsedDeclratation ------"
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
    then return (Fst  expr)
    else return (Snd expr)

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
  lhs <- parseLHS
  punctuation '='
  rhs <- parseRHS
  return $ Assignment lhs rhs

parseLHS :: Parser AssignLHS
parseLHS
  =   ArrayDeref <$> arrayElem
  <|> PairDeref  <$> pairElem
  <|> Var        <$> identifier
