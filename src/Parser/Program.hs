module Parser.Program (parseProgram) where

import Control.Applicative  (many, liftA2)
import Control.Monad.State  (MonadState (..), StateT (..))
import Control.Monad.Except (throwError)

import Parser.Function
import Parser.Lexer
import Parser.Statement
import Parser.Combinators
import Utilities.Definitions
import Semantics.Annotators.AST

-- Parses a WACC program.
parseProgram :: Parser Char Program
parseProgram
  = bracket
      (require (keyword "begin") "Invalid Program start")
      parseProgram'
      endingParse
  where
    parseProgram'
      = liftA2 Program
          (many parseFunction)
          (require parseStatement "Invalid or missing program body")

-- Parses the end token of a WACC program and ensures there is nothing after.
endingParse :: Parser Char String
endingParse = do
  require (string "end") "Unexpected Symbol"
  junk
  unusedInputString <- get
  pos               <- getPosition
  if null unusedInputString
    then return "Valid Program"
    else throwError ("Syntax Error: Unexpected Symbol", updateRowPosition pos)
