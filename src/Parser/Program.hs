{-# LANGUAGE MultiWayIf #-}
module Parser.Program (parseProgram) where

import Control.Applicative
import Control.Monad
import Control.Monad.State  (MonadState (..), StateT (..))
import Control.Monad.Except

import Parser.Expression
import Parser.Function
import Parser.Lexer
import Parser.Statement
import Parser.Combinators
import Utilities.Definitions
import Semantics.Annotators.AST

parseProgram :: Parser Char Program
parseProgram
  = bracket
      (tryParser (keyword "begin") "Invalid Program start")
      parseProgram'
      endingParse
  where
    parseProgram'
      = liftM2 Program (many parseFunction)
          (tryParser parseStatement "Invalid or missing program body")

endingParse :: Parser Char String
endingParse = do
  tryParser (string "end") "Unexpected Symbol"
  junk
  unusedInputString <- get
  pos               <- getPosition
  if | null unusedInputString -> return "Valid Program"
     | otherwise -> throwError ("Syntax Error: Invalid Program", updateRowPosition pos)
