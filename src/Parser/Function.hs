{- This module defines parser combinators for functions -}

module Parser.Function where

import Control.Monad           (liftM3, when)
import Control.Monad.Except    (throwError)

{- LOCAL IMPORTS -}
import Parser.BasicCombinators (Parser (..), getPosition, require, sepby)
import Parser.Identifier       (identifier)
import Parser.LexicalResolver
import Parser.Statement        (parseStatement)
import Parser.Type             (parseType)
import Utilities.Definitions

-- POST: Parses a function defintion
parseFunction :: Parser Char Func
parseFunction = do
  pos          <- getPosition
  returnType   <- parseType
  name         <- identifier
  paramList    <- bracket (punctuation '(') parseParamList (require
                    (punctuation ')') "Invalid parameter list")
  require (keyword "is") "Missing 'is' keyword"
  funcBody     <- parseFunctionBody returnType
  require (keyword "end") "Missing 'end' keyword"
  let parameterTypes = map parameterType (parameters paramList)
  let functionType = FuncT returnType parameterTypes
  return $ Func functionType name paramList funcBody pos
  where
    parameterType (Param t _ _) = t
    parameters (ParamList list _) = list

-- POST: Parses a function body
parseFunctionBody :: Type -> Parser Char Stat
parseFunctionBody t = do
  body  <- parseStatement
  if t /= Void
    then checkExecutionPathsReturnNonVoid body
    else checkVoidReturnOnly body
  return body

{- HELPER FUNCTIONS -}

-- POST: Produces an error if there is a return statement
checkVoidReturnOnly :: Stat -> Parser Char ()

checkVoidReturnOnly Return{} = do
  pos <- getPosition
  throwError ("Syntax Error: Cannot return from void function", pos)

checkVoidReturnOnly ReturnVoid{}
  = return ()

checkVoidReturnOnly Exit{}
  = return ()

checkVoidReturnOnly (If _ s1 s2 _)
  = checkVoidReturnOnly s1 >> checkVoidReturnOnly s2

checkVoidReturnOnly (While _ s _)
  = checkVoidReturnOnly s

checkVoidReturnOnly (Block s _)
  = checkVoidReturnOnly s

checkVoidReturnOnly (For _ _ _ s _)
  = checkVoidReturnOnly s

checkVoidReturnOnly (Seq ReturnVoid{} _ _) = do
  pos <- getPosition
  throwError ("Syntax Error: Unreachable statement after return", pos)

checkVoidReturnOnly (Seq Exit{} _ _) = do
  pos <- getPosition
  throwError ("Syntax Error: Unreachable statement after exit", pos)

checkVoidReturnOnly (Seq s s' _)
  = checkVoidReturnOnly s >> checkVoidReturnOnly s'

checkVoidReturnOnly _
  = return ()

-- POST: Produces an error if an execution path does not end with exit or
--       return, or if there are trailing statements after an exit or return
--       statement. Checks for patterns (Seq Return{} _ _) and (Seq Exit{} _ _)
checkExecutionPathsReturnNonVoid :: Stat -> Parser Char ()

checkExecutionPathsReturnNonVoid Return{}
  = return ()

checkExecutionPathsReturnNonVoid Exit{}
  = return ()

checkExecutionPathsReturnNonVoid ReturnVoid{} = do
  pos <- getPosition
  throwError ("Syntax Error: Cannot return nothing from non-void function", pos)

checkExecutionPathsReturnNonVoid (If _ s1 s2 _)
  = checkExecutionPathsReturnNonVoid s1 >> checkExecutionPathsReturnNonVoid s2

checkExecutionPathsReturnNonVoid (While _ s1 _)
  = checkExecutionPathsReturnNonVoid s1

checkExecutionPathsReturnNonVoid (Block s1 _)
  = checkExecutionPathsReturnNonVoid s1

checkExecutionPathsReturnNonVoid (For _ _ _ s _)
  = checkExecutionPathsReturnNonVoid s

checkExecutionPathsReturnNonVoid (Seq Return{} _ _) = do
  pos <- getPosition
  throwError ("Syntax Error: Unreachable statement after return", pos)

checkExecutionPathsReturnNonVoid (Seq Exit{} _ _) = do
  pos <- getPosition
  throwError ("Syntax Error: Unreachable statement after exit", pos)

checkExecutionPathsReturnNonVoid (Seq s1 s2 _)
  = checkExecutionPathsReturnNonVoid s2

checkExecutionPathsReturnNonVoid s = do
  pos <- getPosition
  throwError
    ("Syntax Error: Mising return or exit statement in function" ++
     " body ending at: " ++ pretty s, pos)

-- POST: Parses comma-delimited list of parameters
parseParamList :: Parser Char ParamList
parseParamList = do
  pos    <- getPosition
  params <- sepby parseParam (punctuation ',')
  return $ ParamList params pos

-- POST:    Parses a single parameter
-- EXAMPLE: (parse parseParam "int name") produces (Param Int "name")
parseParam :: Parser Char Param
parseParam = do
  pos    <- getPosition
  t      <- parseType
  ident  <- identifier
  return $ Param t ident pos
