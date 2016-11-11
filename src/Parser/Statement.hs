{- This module defines a number of parser combinators used to parse
statements -}

module Parser.Statement (parseStatement) where

import Control.Applicative ((<|>), (<$>), liftA3)
import Control.Monad.State (liftM2)

{- LOCAL IMPORTS -}
import Parser.BasicCombinators
import Parser.Expression
import Parser.Identifier
import Parser.LexicalResolver
import Parser.Type
import Utilities.Definitions

-- POST: Parses all valid statements in the WACC language
-- NOTE: It is factored out like this to prevent the parser going in to an
--       infinite loop due to left recursion
parseStatement :: Parser Char Stat
parseStatement
  = parseSeq <|> parseStatement'

-- POST: Parses all valid statements in the WACC language
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

-- POST: Parses an if statement
parseIfStat :: Parser Char Stat
parseIfStat = do
  pos <- getPosition
  keyword "if"
  cond <- require parseExpr "Invalid expression in if condition"
  require (keyword "then") "Missing 'then' keyword"
  thenStat <- require parseStatement "Invalid statement in 'then' branch"
  require (keyword "else") "Missing 'else' keyword"
  elseStat <- require parseStatement "Invalid statement in 'else' branch"
  require (keyword "fi") "Missing 'fi' keyword"
  return $ If cond thenStat elseStat pos

-- POST: Parses a while loop
parseWhileStat :: Parser Char Stat
parseWhileStat = do
  pos <- getPosition
  keyword "while"
  cond      <- require parseExpr "Invalid expression in while condition"
  require (keyword "do") "Missing 'do' keyword"
  loopBody  <- require parseStatement "Invalid statement in while body"
  require (keyword "done") "Missing 'done' keyword"
  return $ While cond loopBody pos

-- POST: Parses a new block of statements
parseBlock :: Parser Char Stat
parseBlock = do
  pos <- getPosition
  keyword "begin"
  stat <- require parseStatement "Invalid statement in block"
  require (keyword "end") "Missing 'end' keyword in block"
  return $ Block stat pos


-- POST: Parses a sequence of statements seperated by the semi-colon operator
parseSeq :: Parser Char Stat
parseSeq = parseStatement' >>= rest
  where
    rest stat = (do
      pos   <-  getPosition
      punctuation ';'
      stat' <-  require parseStatement "Invalid statement in sequence"
      rest $ Seq stat stat' pos) <|> return stat

-- POST: Parses the skip keyword
parseSkip :: Parser Char Stat
parseSkip
  = keyword "skip" >> Skip <$> getPosition

-- POST: Parses a declaration of the form type name = rhs
parseDeclaration :: Parser Char Stat
parseDeclaration = do
  pos        <- getPosition
  varType    <- parseType
  ident      <- identifier
  punctuation '='
  assignRHS  <- require parseRHS "Invalid RHS in declaration"
  return $ Declaration varType ident assignRHS pos

-- POST: Parses an assignment of the form lhs = rhs
parseAssignment :: Parser Char Stat
parseAssignment = do
  pos <- getPosition
  lhs <- parseLHS
  require (punctuation '=')
      "Missing equal sign in assignment. Did you misspell or forget a keyword?"
  rhs <- require parseRHS "Invalid RHS in assignment"
  return $ Assignment lhs rhs pos

-- POST: Parses the built in functions of the WACC language
parseBuiltInFunc :: String -> (Expr -> Position -> Stat) -> Parser Char Stat
parseBuiltInFunc funcName func = do
  pos <- getPosition
  keyword funcName
  expr1 <- require parseExpr ("Invalid arguments to " ++ funcName ++
             " function")
  return $ func expr1 pos

-- POST: Parses the 'read' function of the WACC language
-- NOTE: This method is separate from the general parseBuiltInFunc as the
--       argument of 'read' differs from the other built in functions
parseRead :: Parser Char Stat
parseRead = do
  keyword "read"
  lhs <- parseLHS
  pos <- getPosition
  return $ Read lhs pos

-- POST: Parses all valid right hand sides in WACC
parseRHS :: Parser Char AssignRHS
parseRHS
  =   assignToExpr
  <|> assignToPairElem
  <|> assignToFuncCall
  <|> assignToArrayLit
  <|> assignToNewPair

-- POST: Parses all valid left hand sides in WACC
parseLHS :: Parser Char AssignLHS
parseLHS
  =   do {pos <- getPosition; element <- arrayElem; return $ ArrayDeref element pos}
  <|> do {pos <- getPosition; pairE <- pairElem; return $ PairDeref pairE pos}
  <|> do {pos <- getPosition; ident <- identifier; return $ Var ident pos}

{- HELPER FUNCTIONS -}

-- POST: Parses an expression (RHS)
assignToExpr :: Parser Char AssignRHS
assignToExpr = do
  pos <- getPosition
  expr <- parseExpr
  return $ ExprAssign expr pos

-- POST: Parses a newpair declaration (RHS)
assignToNewPair :: Parser Char AssignRHS
assignToNewPair = do
  pos <- getPosition
  token "newpair"
  require (punctuation '(') "Missing opening parenthesis for newpair"
  expr1 <- require parseExpr "Invalid expression in first expression in newpair"
  require (punctuation ',') "Expecting comma in new pair declaration"
  expr2 <- require parseExpr "Invalid expression in second expression in newpair"
  require (punctuation ')') "Missing closing parenthesis for newpair"
  return $ NewPairAssign expr1 expr2 pos

-- POST: Parses functions calls (RHS)
assignToFuncCall :: Parser Char AssignRHS
assignToFuncCall = do
  pos      <- getPosition
  keyword "call"
  name     <- require identifier "Invalid Function Name"
  arglist  <- require (parseExprList '(' ')') "Invalid parameter list"
  return $ FuncCallAssign name arglist pos

-- POST: Parses array literals (RHS)
assignToArrayLit :: Parser Char AssignRHS
assignToArrayLit = do
  pos <- getPosition
  punctuation '['
  exprList <- sepby parseExpr (punctuation ',')
  require (punctuation ']') "No closing bracket in array literal"
  return $ ArrayLitAssign exprList pos

-- POST: Parses pair elements in which can appear in both the LHS and the RHS
pairElem :: Parser Char PairElem
pairElem = do
  pos <- getPosition
  selector <- pairFst <|> pairSnd
  expr <- pairElemExpr
  return $ PairElem selector expr pos

-- POST: Parses a pair elem expression
pairElemExpr :: Parser Char Expr
pairElemExpr
  = require parseExpr "Invalid expression in pairElem"

-- POST: Parses the 'fst' keyword in a pair
pairFst :: Parser Char PairElemSelector
pairFst = do
  keyword "fst"
  return Fst

-- POST: Parses the 'snd' keyword in a pair
pairSnd :: Parser Char PairElemSelector
pairSnd = do
  keyword "snd"
  return Snd

-- POST: Wraps the result of parsing a pairElem in the appropriate data
--       constructor
assignToPairElem :: Parser Char AssignRHS
assignToPairElem = do
  pos <- getPosition
  pairE <- pairElem
  return $ PairElemAssign pairE pos
