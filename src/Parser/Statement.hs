{-
This module defines a number of parser combinators used to parse statements in
the WACC language. Refer to the BNF specification of the WACC language to see
exactly what a statement is in the WACC language.
-}

module Parser.Statement (parseStatement) where

import Control.Applicative
import Data.Char

import Parser.Expression
import Parser.Lexer
import Parser.Type
import Parser.Combinators
import Utilities.Declarations
import Utilities.Definitions

-- POST: Parses all valid statements in the WACC language, it is factored out
--       like this to prevent the parser going in to an infinite loop due to
--       left recursion
parseStatement :: Parser Char Stat
parseStatement
  = parseSeq <|> parseStatement'

parseStatement' :: Parser Char Stat
parseStatement'
  =   parseDeclaration
  <|> parseRead
  <|> parseAssignment
  <|> parseBuiltInFunc "free"    Free
  <|> parseBuiltInFunc "return"  Return
  <|> parseBuiltInFunc "exit"    Exit
  <|> parseBuiltInFunc "println" Println
  <|> parseBuiltInFunc "print"   Print
  <|> parseIfStat
  <|> parseWhileStat
  <|> parseBlock
  <|> parseSkip

-- The following two parsers parse the built in functions of the WACC language.
-- We have a generic parser for all built in funcs except Read. This is
-- because the argument of read differs from the other built in functions

parseRead :: Parser Char Stat
parseRead
  = keyword "read" >> (Read <$> parseLHS)

parseBuiltInFunc :: String -> (Expr -> Stat) -> Parser Char Stat
parseBuiltInFunc funcName func
  = do
      keyword funcName
      expr1 <- locationReporter parseExpr ("Invalid arguments to " ++
                 funcName ++ " function")
      return $ func expr1

-- Parsers for all the synctactic structures in the WACC language that make up
-- a statement:

-- POST: Parses a conditional
parseIfStat :: Parser Char Stat
parseIfStat = do
  keyword "if"
  cond          <- locationReporter parseExpr
                     "Invalid expression for if condition"
  locationReporter (keyword "then") "Missing 'then' keyword"
  thenStat      <- locationReporter parseStatement
                     "Invalid statement for then branch"
  posBeforeElse <- getPosition
  locationReporter (keyword "else") "Missing 'else' keyword"
  posAfterElse  <- getPosition
  elseStat      <- locationReporter parseStatement
                     "Invalid statement for else branch"
  pos           <- getPosition
  locationReporter (keyword "fi") "Missing 'fi' keyword"
  pos1          <- getPosition
  return         $ If cond thenStat elseStat

-- POST: Parses a while loop
parseWhileStat :: Parser Char Stat
parseWhileStat = do
  keyword "while"
  cond      <- locationReporter parseExpr
                 "Invalid expression in while condition"
  locationReporter (keyword "do") "Missing 'do' keyword"
  loopBody  <- locationReporter parseStatement
                 "Invalid statement for while condition"
  locationReporter (keyword "done") "Missing 'done' keyword"
  return $ While cond loopBody

-- POST: Parses a new block of statements.
parseBlock :: Parser Char Stat
parseBlock = Block <$> do
  keyword "begin"
  s <- locationReporter parseStatement "Invalid statement in block"
  locationReporter (keyword "end") "Missing 'end' keyword in block"
  return s

-- POST: Parses a sequence of statements seperated by semi-colons.
parseSeq :: Parser Char Stat
parseSeq = parseStatement' >>= rest
  where
    rest s = (do
      punctuation ';'
      s' <-  locationReporter parseStatement "Invalid statement in sequence"
      rest $ Seq s s') <|> return s

-- POST: Parses the skip keyword.
parseSkip :: Parser Char Stat
parseSkip
  = keyword "skip" >> return Skip

-- POST: Parses a declaration of the form type name = rhs.
parseDeclaration :: Parser Char Stat
parseDeclaration = do
  varType    <- parseType
  ident      <- identifier
  punctuation '='
  assignRHS  <- locationReporter parseRHS "Invalid RHS in declaration"
  return $ Declaration varType ident assignRHS

-- POST: Parses an assignment of the form lhs = rhs.
parseAssignment :: Parser Char Stat
parseAssignment = do
  lhs <- parseLHS
  locationReporter (punctuation '=')
    "Missing equal sign in assignment. Did you misspell or forget a keyword?"
  rhs <- locationReporter parseRHS "Invalid RHS in assignment"
  return $ Assignment lhs rhs

{-
Defines a number of parser combinators which can parse all valid lhs and rhs of
a declaration assignment. These combinators are used to build parseAssignment
& parseDeclaration
-}

-- POST: Parses all valid RHS of an assignment or declaration
parseRHS :: Parser Char AssignRHS
parseRHS
  =   assignToExpr
  <|> assignToPairElem
  <|> assignToFuncCall
  <|> assignToArrayLit
  <|> assignToNewPair

-- POST: Parses all valid LHS of an assignment or the argument of the function
--       read
parseLHS :: Parser Char AssignLHS
parseLHS
  =   ArrayDeref <$> arrayElem
  <|> PairDeref  <$> pairElem
  <|> Var        <$> identifier

-- POST: Parses an expr (rhs)
assignToExpr :: Parser Char AssignRHS
assignToExpr
  = ExprAssign <$> parseExpr

-- POST: Parses a pair elem which can be either a lhs or rhs.
pairElem :: Parser Char PairElem
pairElem = do
  fstOrSnd  <- keyword "fst" <|> keyword "snd"
  expr      <- locationReporter parseExpr "Invalid Expr for pairElem"
  if fstOrSnd == "fst"
    then return (Fst  expr)
    else return (Snd expr)

-- POST: Wraps the result of parsing a pairElem in the appropriate data
--       constructor
assignToPairElem :: Parser Char AssignRHS
assignToPairElem
  = PairElemAssign <$> pairElem

-- POST: Parses functions calls (rhs)
assignToFuncCall :: Parser Char AssignRHS
assignToFuncCall = do
  keyword "call"
  name     <- locationReporter identifier "Invalid Function Name"
  arglist  <- locationReporter (parseExprList '(' ')') "Invalid parameter list"
  return $ FuncCallAssign name arglist

-- POST: Parses array literals (rhs)
assignToArrayLit :: Parser Char AssignRHS
assignToArrayLit = do
  punctuation '['
  exprList <- sepby parseExpr (punctuation ',')
  locationReporter (punctuation ']') "No closing bracket in array literal"
  return $ ArrayLitAssign exprList

-- POST: Parses a newpair declaration (rhs)
assignToNewPair :: Parser Char AssignRHS
assignToNewPair = do
  token "newpair"
  locationReporter (punctuation '(') "Missing opening parenthesis for newpair"
  expr1 <- locationReporter parseExpr
             "Invalid Expr for first expression in newpair"
  locationReporter (punctuation ',') "No comma in new pair declaration"
  expr2 <- locationReporter parseExpr
             "Invalid Expr for second expression in newpair"
  locationReporter (punctuation ')') "Missing closing parenthesis for newpair"
  return $ NewPairAssign expr1 expr2
