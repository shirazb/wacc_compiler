import Control.Monad.Writer.Strict
import System.Environment
import System.Exit
import System.IO
import qualified Data.Map as Map
import Data.List

{- LOCAL IMPORTS -}
import Optimisations.Optimiser
import Parser.Statement
import Parser.Program
import Parser.BasicCombinators
import Parser.BasicCombinators
import Semantics.Annotators.AST
import Semantics.ScopeErrorGenerator
import Semantics.TypeChecker.Program
import Utilities.Definitions

-- POST: Prints the given errors then exits with code 200
semanticFailure :: [String] -> IO ()
semanticFailure errors
  = mapM_ putStrLn errors >> exitWith (ExitFailure 200)

-- POST: Prints the given err then exits with code 100
syntacticFailure :: Err -> IO AST
syntacticFailure err
  = print err >> exitWith (ExitFailure 100)

main = do
  -- Reads file from command line
  args         <- getArgs
  let filename  = head args
  contents     <- readFile filename

  -- Parses the source file and either generates an AST or exits with a syntax
  -- error
  let ast       = runParser parseProgram contents
  a <- case ast of
         Right (Just ((a,b),_))  -> return a
         Left err                -> syntacticFailure err

  -- Annotation enriches the AST with type and scope error information
  let annotatedAST = annotateAST a

  -- Traverses the AST, prints any scope errors found and exits the program if
  -- any errors are found
  case scopeCheckProgram annotatedAST of
    []     -> return ()
    errors -> semanticFailure errors

  -- Traverses the AST and type checks the program. If any type errors are
  -- found they are printed and the program terminates
  case generateTypeErrorMessages annotatedAST of
    []     -> return ()
    errors ->  semanticFailure errors

  -- Performs constant evaluation and control flow analysis optimisations
  optimisedAST <- case optimiser annotatedAST of
                    Right optAST -> return optAST
                    Left errors  -> do
                        mapM_ putStrLn errors
                        exitWith (ExitFailure 200)

  -- Generates ARM11 Assembly code from the AST and prints to stdout
  -- Compile script pipes this in to a file and generates the assembly file.
  putStrLn (makeInstr optimisedAST)

  exitSuccess
