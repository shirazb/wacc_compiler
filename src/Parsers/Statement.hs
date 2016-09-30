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
-- POST: Parses all valid statements in the WACC language, it is factored out like
-- this to prevent the parser going in to an infinite loop due to left recursion.
parseStatement :: Parser Char Stat
parseStatement
  = parseSeq <|> parseStatement'

parseStatement' :: Parser Char Stat
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


{-
Parser combinators for statements in the WACC language, refer to BNF specification to see
what a statement is.
-}


{-
The following two parsers, parse the built in functions of the WACC language. We have a generic parser for all built in funcs except Read.
This is because the argument of read differs from the other built in funcs.
-}
parseRead :: Parser Char Stat
parseRead
  = keyword "read" >> (Read <$> parseLHS)

parseBuiltInFunc :: String -> (Expr -> Stat) -> Parser Char Stat
parseBuiltInFunc funcName func
  = keyword funcName >> (func <$> parseExpr)


{-
Parsers for all the synctactic structures in the WACC language that make up a statement.
-}


-- PRE: None
-- POST: Parses a conditional
parseIfStat :: Parser Char Stat
parseIfStat = do
  keyword "if"
  cond <- parseExpr
  keyword "then"
  thenStat <- parseStatement
  keyword "else"
  elseStat <- parseStatement
  keyword "fi"
  return $ If cond thenStat elseStat


-- PRE: None
-- POST: Parses a while loop
parseWhileStat :: Parser Char Stat
parseWhileStat = do
  keyword "while"
  cond       <- parseExpr
  keyword "do"
  loopBody   <- parseStatement
  keyword "done"
  return $ While cond loopBody


-- PRE: None
-- POST: Parses a new block of statements.
parseBlock :: Parser Char Stat
parseBlock
  = Block <$> do
    keyword "begin"
    s <- parseStatement
    keyword "end"
    return s

-- PRE: None
-- POST: Parses a sequence of statements seperated by semi-colons.
parseSeq :: Parser Char Stat
parseSeq = parseStatement' >>= \s -> rest s
  where
    rest s = (do
      punctuation ';'
      s' <- parseStatement
      rest $ Seq s s') <|> return s


-- PRE: None
-- POST: Parses the skip keyword.
parseSkip :: Parser Char Stat
parseSkip
  = keyword "skip" >> return Skip

-- PRE: None
-- POST: Parses a declaration of the form type name = rhs.
parseDeclaration :: Parser Char Stat
parseDeclaration = do
  varType    <- parseType
  ident      <- identifier
  punctuation '='
  assignRHS  <- parseRHS
  return $ Declaration varType ident assignRHS

-- PRE: None
-- POST: Parses an assignment of the form lhs = rhs.
parseAssignment :: Parser Char Stat
parseAssignment = do
  lhs <- parseLHS
  punctuation '='
  rhs <- parseRHS
  return $ Assignment lhs rhs

{-
Defines a number of parser combinators which can parse all valid lhs and rhs of
a declaration or assignment. These combinators are used to build parseAssignment
& parseDeclaration
-}

-- PRE: None
-- POST: Parses all valid RHS of an assignment or declaration.
parseRHS :: Parser Char AssignRHS
parseRHS
  =   assignToExpr
  <|> assignToPairElem
  <|> assignToFuncCall
  <|> assignToArrayLit
  <|> assignToNewPair

-- PRE: None
-- POST: Parses all valid LHS of an assignment or the argument of the function read.
parseLHS :: Parser Char AssignLHS
parseLHS
  =   ArrayDeref <$> arrayElem
  <|> PairDeref  <$> pairElem
  <|> Var        <$> identifier


-- PRE: None
-- POST: Parses an expr (rhs)
assignToExpr :: Parser Char AssignRHS
assignToExpr
  = ExprAssign <$> parseExpr

-- PRE: None
-- POST: Parses a pair elem which can be either a lhs or rhs.
pairElem :: Parser Char PairElem
pairElem = do
  fstOrSnd  <- keyword "fst" <|> keyword "snd"
  expr      <- parseExpr
  if fstOrSnd == "fst"
    then return (Fst  expr)
    else return (Snd expr)

-- PRE: None
-- POST: Wraps the result of parsing a pairElem in the appropriate data constructor.
assignToPairElem :: Parser Char AssignRHS
assignToPairElem
  = PairElemAssign <$> pairElem

-- PRE: None
-- POST: Parses functions calls (rhs)
assignToFuncCall :: Parser Char AssignRHS
assignToFuncCall = do
  keyword "call"
  name     <- identifier
  arglist  <- parseExprList '(' ')'
  return $ FuncCallAssign name arglist

-- PRE: None
-- POST: Parses array literals (rhs)
assignToArrayLit :: Parser Char AssignRHS
assignToArrayLit
  = ArrayLitAssign <$> parseExprList '[' ']'

-- PRE: None
-- POST: Parses a newpair declaration (rhs)
assignToNewPair :: Parser Char AssignRHS
assignToNewPair = do
  token "newpair"
  punctuation '('
  expr1 <- parseExpr
  punctuation ','
  expr2 <- parseExpr
  punctuation ')'
  return $ NewPairAssign expr1 expr2
