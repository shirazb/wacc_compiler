{-
This module defines a number of parser combinators used to parse statements in
the WACC language. Refer to the BNF specification of the WACC language to see
exactly what a statement is in the WACC language.
-}

module Parser.Statement (parseStatement) where

import Control.Applicative ((<|>), (<$>), liftA3)
import Control.Monad.State (liftM2)

import Parser.Expression
import Parser.Identifier
import Parser.Lexer
import Parser.Type
import Parser.Combinators
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

{-
Parsers for statements in the WACC language.
-}

-- POST: Parses an if statement.
parseIfStat :: Parser Char Stat
parseIfStat = do
  keyword "if"
  cond <- require parseExpr "Invalid expression in if condition"
  require (keyword "then") "Missing 'then' keyword"
  thenStat <- require parseStatement "Invalid statement in 'then' branch"
  require (keyword "else") "Missing 'else' keyword"
  elseStat <- require parseStatement "Invalid statement in 'else' branch"
  require (keyword "fi") "Missing 'fi' keyword"
  pos <- getPosition
  return $ If cond thenStat elseStat pos

-- POST: Parses a while loop.
parseWhileStat :: Parser Char Stat
parseWhileStat = do
  keyword "while"
  cond      <- require parseExpr "Invalid expression in while condition"
  require (keyword "do") "Missing 'do' keyword"
  loopBody  <- require parseStatement "Invalid statement in while body"
  require (keyword "done") "Missing 'done' keyword"
  pos <- getPosition
  return $ While cond loopBody pos

-- POST: Parses a new block of statements.
parseBlock :: Parser Char Stat
parseBlock = do
  keyword "begin"
  stat <- require parseStatement "Invalid statement in block"
  require (keyword "end") "Missing 'end' keyword in block"
  pos <- getPosition
  return $ Block stat pos


-- POST: Parses a sequence of statements seperated by the semi-colon operator.
parseSeq :: Parser Char Stat
parseSeq = parseStatement' >>= rest
  where
    rest stat = (do
      punctuation ';'
      stat' <-  require parseStatement "Invalid statement in sequence"
      pos   <- getPosition
      rest $ Seq stat stat' pos) <|> return stat

-- POST: Parses the skip keyword.
parseSkip :: Parser Char Stat
parseSkip
  = keyword "skip" >> Skip <$> getPosition

-- POST: Parses a declaration of the form type name = rhs.
parseDeclaration :: Parser Char Stat
parseDeclaration = do
  varType    <- parseType
  ident      <- identifier
  punctuation '='
  assignRHS  <- require parseRHS "Invalid RHS in declaration"
  pos <- getPosition
  return $ Declaration varType ident assignRHS pos

-- POST: Parses an assignment of the form lhs = rhs.
parseAssignment :: Parser Char Stat
parseAssignment = do
  lhs <- parseLHS
  require (punctuation '=')
      "Missing equal sign in assignment. Did you misspell or forget a keyword?"
  rhs <- require parseRHS "Invalid RHS in assignment"
  pos <- getPosition
  return $ Assignment lhs rhs pos


{-
  The following two Parsers parse the built in functions of the WACC language.
  We have a generic parser for all built in functions except Read. This is
  because the argument of read differs from the other built in functions.
-}

parseRead :: Parser Char Stat
parseRead = do
  keyword "read"
  lhs <- parseLHS
  pos <- getPosition
  return $ Read lhs pos


parseBuiltInFunc :: String -> (Expr -> Position -> Stat) -> Parser Char Stat
parseBuiltInFunc funcName func = do
  keyword funcName
  expr1 <- require parseExpr ("Invalid arguments to " ++ funcName ++
             " function")
  pos <- getPosition
  return $ func expr1 pos

{-
  Parsers for the left hand sides and right hand sides of the WACC language.
-}

-- POST: Parses all valid right hand sides in WACC.
parseRHS :: Parser Char AssignRHS
parseRHS
  =   assignToExpr
  <|> assignToPairElem
  <|> assignToFuncCall
  <|> assignToArrayLit
  <|> assignToNewPair

-- POST: Parses all valid left hand sides in WACC.
parseLHS :: Parser Char AssignLHS
parseLHS
  =   liftM2 ArrayDeref arrayElem getPosition
  <|> liftM2 PairDeref pairElem getPosition
  <|> liftM2 Var identifier getPosition


{-
  Parsers used in the definitions of parseLHS & parseRHS, they parse
  various LHS's or RHS's in the WACC language.
-}

-- POST: Parses an expression (RHS).
assignToExpr :: Parser Char AssignRHS
assignToExpr
  = liftM2 ExprAssign parseExpr getPosition

-- POST: Parses a newpair declaration (RHS).
assignToNewPair :: Parser Char AssignRHS
assignToNewPair = do
  token "newpair"
  require (punctuation '(') "Missing opening parenthesis for newpair"
  expr1 <- require parseExpr "Invalid expression in first expression in newpair"
  require (punctuation ',') "Expecting comma in new pair declaration"
  expr2 <- require parseExpr "Invalid expression in second expression in newpair"
  require (punctuation ')') "Missing closing parenthesis for newpair"
  pos <- getPosition
  return $ NewPairAssign expr1 expr2 pos

-- POST: Parses functions calls (RHS).
assignToFuncCall :: Parser Char AssignRHS
assignToFuncCall = do
  keyword "call"
  name     <- require identifier "Invalid Function Name"
  arglist  <- require (parseExprList '(' ')') "Invalid parameter list"
  pos      <- getPosition
  return $ FuncCallAssign name arglist pos

-- POST: Parses array literals (RHS).
assignToArrayLit :: Parser Char AssignRHS
assignToArrayLit = do
  punctuation '['
  exprList <- sepby parseExpr (punctuation ',')
  require (punctuation ']') "No closing bracket in array literal"
  pos <- getPosition
  return $ ArrayLitAssign exprList pos


{-
  The following parsers are used to parse pair elements.
-}

-- POST: Parses pair elements in which can appear in both lhs & rhs.
pairElem :: Parser Char PairElem
pairElem
  = liftA3 PairElem (pairFst <|> pairSnd) pairElemExpr getPosition

-- POST: Parses a pair elem expression.
pairElemExpr :: Parser Char Expr
pairElemExpr
  = require parseExpr "Invalid expression in pairElem"

pairFst :: Parser Char PairElemSelector
pairFst = do
  keyword "fst"
  return Fst

pairSnd :: Parser Char PairElemSelector
pairSnd = do
  keyword "snd"
  return Snd

-- POST: Wraps the result of parsing a pairElem in the appropriate data
--       constructor
assignToPairElem :: Parser Char AssignRHS
assignToPairElem
  = PairElemAssign <$> pairElem <*> getPosition
