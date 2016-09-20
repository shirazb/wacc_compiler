{-
A parser built using parser combinators for the WACC language. Parser
combinators are used because of the flexibility and modularity that they
offer. Building a parser combinator in Haskell also serves as a learning
experience to learn the more advanced features of Haskell. The parser
currently has no error handling.
-}

module Parser (parseProgram) where

import           Control.Applicative
import           Control.Monad
import           Debug.Trace
import           System.Environment

{- Local Imports -}

import           Parsers.Expression
import           Parsers.Function
import           Parsers.Lexer
import           Parsers.Statement
import           Utility.BasicCombinators
import           Utility.Declarations
import           Utility.Definitions

main = do
  args <- getArgs
  let filename = head args
  contents <- readFile filename
  let c = parse parseProgram contents
  putStrLn "------------------------------------------------"
  putStrLn "THE PROGRAM WE HAVE PARSED"
  putStrLn "------------------------------------------------"
  print ((fst . head) c)
  return ()

parseProgram :: Parser Program
parseProgram
  = bracket (keyword "begin") parseProgram' (string "end")
  where
    parseProgram' = liftM2 Program (many parseFunction) parseStatement
