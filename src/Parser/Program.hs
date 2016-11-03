module Parser.Program (makeAST, parseProgram) where

import Control.Applicative
import Control.Monad
import Control.Monad.State  (MonadState (..), StateT (..))
import Control.Monad.Except

import Parser.Expression
import Parser.Function
import Parser.Lexer
import Parser.Statement
import Parser.Combinators
import Utilities.Declarations
import Utilities.Def2
import Semantics.Annotators.AST

makeAST :: String -> (AST, AST)
makeAST source
  = (a , annotateAST a)
  where
    Right (Just ((a, _), _)) = runParser parseProgram source (0, 0)

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
endingParse
  = do
      tryParser (string "end") "Unexpected Symbol"
      junk
      unusedInputString <- get
      pos               <- getPosition
      if null unusedInputString
        then return "Valid Program"
        else throwError
               ("Syntax Error: Invalid Program", updateRowPosition pos)
