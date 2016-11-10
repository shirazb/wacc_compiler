{-
A parser built using parser combinators for the WACC language. Parser
combinators are used because of the flexibility and modularity that they
offer. Building a parser combinator in Haskell also serves as a learning
experience to learn the more advanced features of Haskell. The parser
currently has no error handling.
-}

import Control.Monad.Writer.Strict
import System.Environment
import System.Exit

import Parser.Program
import Semantics.Annotators.AST
import Semantics.ScopeChecker
import Semantics.TypeChecker.Program
import Utilities.Definitions
import Parser.Combinators

main = do
  args         <- getArgs
  let filename = head args
  contents     <- readFile filename

  let ast      = runParser parseProgram contents (0,0)
  a <- case ast of
         Right (Just ((a,b),_))  -> return a
         Left err                -> do {print err; exitWith (ExitFailure 100)}

  let annotatedAST = annotateAST a
  case scopeCheckProgram annotatedAST of
    [] -> return ()
    errors -> do {mapM_ putStrLn errors; exitWith (ExitFailure 200)}

  case generateTypeErrorMessages annotatedAST of
    [] -> return ()
    errors ->  do {mapM_ putStrLn errors; exitWith (ExitFailure 200)}

  exitSuccess
