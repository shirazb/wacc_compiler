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

{-The following two parsers parse the built in functions of the WACC language.
  We have a generic parser for all built in funcs except Read. This is
  because the argument of read differs from the other built in functions -}

parseRead :: Parser Char Stat
parseRead = do
  keyword "read"
  lhs <- parseLHS
  pos <- getPosition
  return $ Read lhs pos


parseBuiltInFunc :: String -> (Expr -> Position -> Stat) -> Parser Char Stat
parseBuiltInFunc funcName func = do
  keyword funcName
  expr1 <- tryParser parseExpr ("Invalid arguments to " ++ funcName ++ " function")
  pos <- getPosition
  return $ func expr1 pos

{-
Parsers for all the synctactic structures in the WACC language that make up a statement.
-}

-- POST: Parses a conditional
parseIfStat :: Parser Char Stat
parseIfStat = do
  keyword "if"
  cond <- tryParser parseExpr "Invalid expression for if condition"
  tryParser (keyword "then") "Missing 'then' keyword"
  thenStat <- tryParser parseStatement "Invalid statement for then branch"
  tryParser (keyword "else") "Missing 'else' keyword"
  elseStat <- tryParser parseStatement "Invalid statement for else branch"
  tryParser (keyword "fi") "Missing 'fi' keyword"
  pos <- getPosition
  return $ If cond thenStat elseStat pos

-- POST: Parses a while loop
parseWhileStat :: Parser Char Stat
parseWhileStat = do
  keyword "while"
  cond      <- tryParser parseExpr "Invalid expression in while condition"
  tryParser (keyword "do") "Missing 'do' keyword"
  loopBody  <- tryParser parseStatement "Invalid statement for while condition"
  tryParser (keyword "done") "Missing 'done' keyword"
  pos <- getPosition
  return $ While cond loopBody pos

-- POST: Parses a new block of statements.
parseBlock :: Parser Char Stat
parseBlock = Block <$> (do
  keyword "begin"
  s <- tryParser parseStatement "Invalid statement in block"
  tryParser (keyword "end") "Missing 'end' keyword in block"
  return s) <*> getPosition

-- POST: Parses a sequence of statements seperated by semi-colons.
parseSeq :: Parser Char Stat
parseSeq = parseStatement' >>= rest
  where
    rest s = (do
      punctuation ';'
      s' <-  tryParser parseStatement "Invalid statement in sequence"
      pos <- getPosition
      rest $ Seq s s' pos) <|> return s

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
  assignRHS  <- tryParser parseRHS "Invalid RHS in declaration"
  pos <- getPosition
  return $ Declaration varType ident assignRHS pos

-- POST: Parses an assignment of the form lhs = rhs.
parseAssignment :: Parser Char Stat
parseAssignment = do
  lhs <- parseLHS
  tryParser (punctuation '=')
      "Missing equal sign in assignment. Did you misspell or forget a keyword?"
  rhs <- tryParser parseRHS "Invalid RHS in assignment"
  pos <- getPosition
  return $ Assignment lhs rhs pos

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
  =   ArrayDeref <$> arrayElem  <*> getPosition
  <|> PairDeref  <$> pairElem   <*> getPosition
  <|> Var        <$> identifier <*> getPosition

-- POST: Parses an expr (rhs)
assignToExpr :: Parser Char AssignRHS
assignToExpr
  = ExprAssign <$> parseExpr <*> getPosition

-- POST: Parses a pair elem which can be either a lhs or rhs.
pairElemExpr :: Parser Char Expr
pairElemExpr
  = tryParser parseExpr "Invalid Expr for pairElem"

pairFst :: Parser Char PairElemSelector
pairFst = do
  keyword "fst"
  return Fst

pairSnd :: Parser Char PairElemSelector
pairSnd = do
  keyword "snd"
  return Snd

pairElem :: Parser Char PairElem
pairElem
  = liftA3 PairElem (pairFst <|> pairSnd) pairElemExpr getPosition

-- POST: Wraps the result of parsing a pairElem in the appropriate data
--       constructor
assignToPairElem :: Parser Char AssignRHS
assignToPairElem
  = PairElemAssign <$> pairElem <*> getPosition

-- POST: Parses functions calls (rhs)
assignToFuncCall :: Parser Char AssignRHS
assignToFuncCall = do
  keyword "call"
  name     <- tryParser identifier "Invalid Function Name"
  arglist  <- tryParser (parseExprList '(' ')') "Invalid parameter list"
  pos      <- getPosition
  return $ FuncCallAssign name arglist pos

-- POST: Parses array literals (rhs)
assignToArrayLit :: Parser Char AssignRHS
assignToArrayLit = do
  punctuation '['
  exprList <- sepby parseExpr (punctuation ',')
  tryParser (punctuation ']') "No closing bracket in array literal"
  pos <- getPosition
  return $ ArrayLitAssign exprList pos

-- POST: Parses a newpair declaration (rhs)
assignToNewPair :: Parser Char AssignRHS
assignToNewPair = do
  token "newpair"
  tryParser (punctuation '(') "Missing opening parenthesis for newpair"
  expr1 <- tryParser parseExpr "Invalid Expr for first expression in newpair"
  tryParser (punctuation ',') "No comma in new pair declaration"
  expr2 <- tryParser parseExpr "Invalid Expr for second expression in newpair"
  tryParser (punctuation ')') "Missing closing parenthesis for newpair"
  pos <- getPosition
  return $ NewPairAssign expr1 expr2 pos
