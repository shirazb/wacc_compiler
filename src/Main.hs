import Control.Monad.Writer.Strict
import System.Environment
import System.Exit
import System.IO
import qualified Data.Map as Map
import Data.List

{- LOCAL IMPORTS -}
import CodeGen.AssemblyGenerator
import Parser.Statement
import Parser.Program
import Parser.BasicCombinators
import Semantics.Annotators.AST
import Semantics.ScopeErrorGenerator
import Semantics.TypeChecker.Program
import Utilities.Definitions
import Optimisations.Optimiser

parseOp :: String -> AST
parseOp s = a
  where
    Right (Just ((a,b),c)) = runParser parseProgram s

main = do
  -- Read file from command line
  args         <- getArgs
  let filename  = head args
  contents     <- readFile filename

  -- Parses the source file and either generates an AST or exits with a syntax
  -- error
  let ast      = runParser parseProgram contents
  a <- case ast of
         Right (Just ((a,b),_))  -> return a
         Left err                -> do {print err; exitWith (ExitFailure 100)}

  -- Annotation enriches the AST with type and scope error information
  let annotatedAST = annotateAST a
  -- Traverses the AST, prints any scope errors found and exits the program if
  -- any errors are found
  case scopeCheckProgram annotatedAST of
    [] -> return ()
    errors -> do {mapM_ putStrLn errors; exitWith (ExitFailure 200)}

  -- Traverses the AST and type checks the program. If any type errors are
  -- found they are printed and the program terminates
  case generateTypeErrorMessages annotatedAST of
    [] -> return ()
    errors ->  do {mapM_ putStrLn errors; exitWith (ExitFailure 200)}

  -- Generates ARM11 Assembly code from the AST and prints to stdout
  -- Compile script pipes this in to a file and generates the assembly file.
  putStrLn (makeInstr annotatedAST)

  exitSuccess
