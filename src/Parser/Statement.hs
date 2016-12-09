{- This module defines a number of parser combinators used to parse
statements -}

module Parser.Statement (parseStatement, checkNoReturnStat) where

import Control.Applicative ((<|>), (<$>), liftA3)
import Control.Monad.State (liftM2, unless, get)
import Control.Monad.Except

{- LOCAL IMPORTS -}
import Parser.BasicCombinators
import Parser.Expression (parseExpr, arrayElem, parseMemberAccess, parseFuncCall)
import Parser.Identifier (identifier)
import Parser.LexicalResolver
import Parser.Type (parseType)
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
  = parseDeclaration
  <|> parseCallFunc
  <|> parseRead
  <|> parseAssignment
  <|> parseCallMethod
  <|> parseBuiltInFunc "free"    Free
  <|> parseBuiltInFunc "return"  Return
  <|> parseBuiltInFunc "exit"    Exit
  <|> parseBuiltInFunc "println" Println
  <|> parseBuiltInFunc "print"   Print
  <|> parseIfStat
  <|> parseWhileStat
  <|> parseForStat
  <|> parseBlock
  <|> parseKeywordStat "return" ReturnVoid
  <|> parseKeywordStat "skip" Skip
  <|> parseKeywordStat "break" Break
  <|> parseKeywordStat "continue" Continue


-- POST: Parses an if statement
parseIfStat :: Parser Char Stat
parseIfStat = do
  pos      <- getPosition
  keyword "if"
  cond     <- require parseExpr "Invalid expression in if condition"
  require (keyword "then") "Missing 'then' keyword"
  thenStat <- require parseStatement "Invalid statement in 'then' branch"
  require (keyword "else") "Missing 'else' keyword"
  elseStat <- require parseStatement "Invalid statement in 'else' branch"
  require (keyword "fi") "Missing 'fi' keyword"
  return $ If cond thenStat elseStat pos

-- POST: Parses a while loop
parseWhileStat :: Parser Char Stat
parseWhileStat = do
  pos      <- getPosition
  keyword "while"
  cond     <- require parseExpr "Invalid expression in while condition"
  require (keyword "do") "Missing 'do' keyword"
  loopBody <- require parseStatement "Invalid statement in while body"
  require (keyword "done") "Missing 'done' keyword"
  return $ While cond loopBody pos

-- POST: Parses a for loop
parseForStat :: Parser Char Stat
parseForStat = do
  pos      <- getPosition
  keyword "for"
  punctuation '('
  decl     <- require parseDeclaration
              "Invalid declaration in for loop argument"
  require (punctuation ';') "Missing semicolon in for loop declaration"
  cond     <- require parseExpr "Invalid condition in for loop argument"
  require (punctuation ';') "Missing semicolon in for loop declaration"
  assign   <- require parseAssignment "Invalid assignment in for loop argument"
  require (punctuation ')') "Missing closing parenthesis in for loop declaration"
  require (keyword "do") "Missing 'do' keyword"
  loopBody <- require parseStatement "Invalid statement in for loop body"
  require (keyword "done") "Missing 'done' keyword"
  return $ For decl cond assign loopBody pos

-- POST: Parses a new block of statements
parseBlock :: Parser Char Stat
parseBlock = do
  pos  <- getPosition
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

-- POST: Parses a statement consisting of just a keyword
parseKeywordStat :: String -> (Position -> Stat) -> Parser Char Stat
parseKeywordStat string statConstructor = do
  pos <- getPosition
  keyword string
  return $ statConstructor pos

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
  punctuation '='
  rhs <- require parseRHS "Invalid RHS in assignment"
  return $ Assignment lhs rhs pos

-- POST: Parses the built in functions of the WACC language
parseBuiltInFunc :: String -> (Expr -> Position -> Stat) -> Parser Char Stat
parseBuiltInFunc funcName func = do
  pos   <- getPosition
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

parseCallMethod :: Parser Char Stat
parseCallMethod = do
  pos <- getPosition
  memberAccess <- parseMemberAccess
  guard (checkIsMethodCall memberAccess)
  return $ CallMethod memberAccess pos

checkIsMethodCall (MemList inst mems _)
  = case last mems of
      MethodCall _ _ -> True
      _              -> False

parseCallFunc :: Parser Char Stat
parseCallFunc = do
  pos <- getPosition
  keyword "call"
  fc <- parseFuncCall
  return $ CallFunc fc pos

-- POST: Parses all valid right hand sides in WACC
parseRHS :: Parser Char AssignRHS
parseRHS
  =   assignToExpr
  <|> assignToPairElem
  <|> assignToFuncCall
  <|> assignToArrayLit
  <|> assignToNewPair
  <|> assignToConstructor

-- POST: Parses all valid left hand sides in WACC
parseLHS :: Parser Char AssignLHS
parseLHS
  =   do {pos <- getPosition; element <- arrayElem; return $ ArrayDeref element pos}
  <|> do {pos <- getPosition; pairE <- pairElem; return $ PairDeref pairE pos}
  <|> do {pos <- getPosition; memAccess <- parseFieldLHS; return $ MemberDeref memAccess pos}
  <|> do {pos <- getPosition; ident <- identifier; return $ Var ident pos}

parseFieldLHS :: Parser Char MemberAccess
parseFieldLHS = do
  pos <- getPosition
  memAccess <- parseMemberAccess
  guard (checkIsFieldAccess memAccess)
  return memAccess

checkIsFieldAccess :: MemberAccess -> Bool
checkIsFieldAccess (MemList inst mems _)
  = case last mems of
      FieldAccess _ _ -> True
      _               -> False

{- HELPER FUNCTIONS -}

-- POST: Parses a constructor call (RHS)
assignToConstructor :: Parser Char AssignRHS
assignToConstructor = do
  pos              <- getPosition
  keyword "new"
  constructorCall  <- parseFuncCall
  return $ ConstructAssign constructorCall pos

-- POST: Parses an expression (RHS)
assignToExpr :: Parser Char AssignRHS
assignToExpr = do
  pos  <- getPosition
  expr <- parseExpr
  return $ ExprAssign expr pos

-- POST: Parses a newpair declaration (RHS)
assignToNewPair :: Parser Char AssignRHS
assignToNewPair = do
  pos   <- getPosition
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
  funcCall <- require parseFuncCall "Invalid function call"
  return $ FuncCallAssign funcCall pos

-- POST: Parses array literals (RHS)
assignToArrayLit :: Parser Char AssignRHS
assignToArrayLit = do
  pos      <- getPosition
  punctuation '['
  exprList <- sepby parseExpr (punctuation ',')
  require (punctuation ']') "No closing bracket in array literal"
  return $ ArrayLitAssign exprList pos

-- POST: Parses pair elements in which can appear in both the LHS and the RHS
pairElem :: Parser Char PairElem
pairElem = do
  pos      <- getPosition
  selector <- pairFst <|> pairSnd
  expr     <- pairElemExpr
  return $ PairElem selector expr pos

-- POST: Parses a pair elem expression
pairElemExpr :: Parser Char Expr
pairElemExpr
  = require parseExpr "Invalid expression in pairElem"

-- POST: Parses the 'fst' keyword in a pair
pairFst :: Parser Char PairElemSelector
pairFst
  = keyword "fst" >> return Fst

-- POST: Parses the 'snd' keyword in a pair
pairSnd :: Parser Char PairElemSelector
pairSnd =
  keyword "snd" >> return Snd

-- POST: Wraps the result of parsing a pairElem in the appropriate data
--       constructor
assignToPairElem :: Parser Char AssignRHS
assignToPairElem = do
  pos   <- getPosition
  pairE <- pairElem
  return $ PairElemAssign pairE pos

-- POST: Returns true if a return statement is present in the main body
checkNoReturnStat :: Stat -> Parser Char ()
checkNoReturnStat (Return _ _) = do
  pos <- getPosition
  throwError ("Syntax Error: Not in a function. Cannot return.", pos)
checkNoReturnStat (ReturnVoid _) = do
  pos <- getPosition
  throwError ("Syntax Error: Not in a function. Cannot return.", pos)
checkNoReturnStat (Seq s1 s2 _)
  = checkNoReturnStat s1 >> checkNoReturnStat s2
checkNoReturnStat (If _ s1 s2 _)
  = checkNoReturnStat s1 >> checkNoReturnStat s2
checkNoReturnStat (While _ s _)
  = checkNoReturnStat s
checkNoReturnStat (For _ _ _ s _)
  = checkNoReturnStat s
checkNoReturnStat (Block s _)
  = checkNoReturnStat s
checkNoReturnStat _
  = return ()
