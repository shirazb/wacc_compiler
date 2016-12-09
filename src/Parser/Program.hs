{- This module defines a parser for the overall program-}

module Parser.Program (parseProgram) where

import Control.Applicative      (many, liftA3)
import Control.Monad.State      (MonadState (..), StateT (..))
import Control.Monad.Except     (throwError)

{- LOCAL IMPORTS -}
import Parser.Function          (parseFunction)
import Parser.LexicalResolver
import Parser.Statement         (parseStatement, checkNoReturnStat)
import Parser.Class             (parseClass)
import Parser.BasicCombinators
import Semantics.Annotators.AST
import Utilities.Definitions

-- POST: Parses a WACC program
parseProgram :: Parser Char Program
parseProgram
  = bracket
      (require (keyword "begin") "Invalid Program start")
      parseProgram'
      endingParse
  where
    parseProgram'
      = liftA3 Program
          (many parseClass)
          (many parseFunction)
          parseMain

-- POST: Parses the main program body, checking for returns too.
parseMain :: Parser Char Stat
parseMain = do
  body <- require parseStatement "Invalid or missing program body"
  checkNoReturnStat body
  return body

-- POST: Parses the end token of a WACC program and ensures there is nothing
--       afterwards
endingParse :: Parser Char String
endingParse = do
  require (string "end") "Unexpected Symbol"
  junk
  unusedInputString <- get
  pos               <- getPosition
  if null unusedInputString
    then return "Valid Program"
    else throwError ("Syntax Error: Unexpected Symbol", updateRowPosition pos)
