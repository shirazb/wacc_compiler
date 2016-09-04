module Parser (parseProgram) where

import Control.Applicative
import Control.Monad
import Debug.Trace
import System.Environment

{- Local Imports -}

import Parsers.Function
import Parsers.Statement
import Utility.Definitions
import Utility.BasicCombinators
import Utility.Declarations
import Parsers.Expression
import Parsers.Lexer


--- I HAVE SOLVED THE LEXICAL ISSUES
--  BUT IN THE MOST IN-EFFICEINT WAY POSSIBLE
--  BUT MORE TO TEST THE PARSER
--  WE NEED TO TAKE IN AN ACTUAL FILE AND GENERATE THE assignToExpr

main = do
  args <- getArgs
  let filename = head args
  contents <- readFile filename
  let c = parse parseProgram contents
  traceM ("The generated AST is: " ++ show c)
  return ()

parseProgram :: Parser Program
parseProgram
  = token $ leadWSC $ bracket (string "begin") parseProgram' (string "end")
  where
    parseProgram' = liftM2 Program (many parseFunction) parseStatement
