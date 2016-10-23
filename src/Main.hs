{-
A parser built using parser combinators for the WACC language. Parser
combinators are used because of the flexibility and modularity that they
offer. Building a parser combinator in Haskell also serves as a learning
experience to learn the more advanced features of Haskell. The parser
currently has no error handling.
-}

module Parser (runParser) where

import Control.Applicative
import Control.Monad
import Debug.Trace
import System.Environment
import Control.Monad.Except
import Control.Monad.State  (MonadState (..), StateT (..))

import Parser.Expression
import Parser.Function
import Parser.Lexer
import Parser.Statement
import Parser.Combinators
import Utilities.Declarations
import Utilities.Definitions

main 
  = do
      args        <- getArgs
      let filename = head args
      contents    <- readFile filename
      putStrLn "------------------------------------------------"
      putStrLn "           THE PROGRAM WE HAVE PARSED           "
      putStrLn "------------------------------------------------"
      case runParser parseProgram contents (0,0) of
        Right (Just ((a,b), _)) -> print a
        Left err                -> print err
        Right Nothing           -> print "Program Failure"
      return ()

parseProgram :: Parser Char Program
parseProgram
  = bracket (locationReporter (keyword "begin") "Invalid Program start") 
      parseProgram' endingParse
  where
    parseProgram' 
      = liftM2 Program (many parseFunction) 
          (locationReporter parseStatement "Invalid or missing program body")

endingParse :: Parser Char String
endingParse 
  = do
      locationReporter (string "end") "Unexpected Symbol"
      junk
      unusedInputString <- get
      pos               <- getPosition
      if null unusedInputString
        then return "Valid Program"
        else throwError 
               ("Syntax Error: Invalid Program", updateRowPosition pos)
