import Control.Monad.Writer.Strict
import System.Environment
import System.Exit
import System.IO
import qualified Data.Map as Map
import Data.List

{- LOCAL IMPORTS -}
import Parser.Program
import Parser.BasicCombinators
import Semantics.Annotators.AST
import Semantics.ScopeErrorGenerator
import Semantics.TypeChecker.Program
import Utilities.Definitions
import CodeGen.Assembly
import CodeGen.Statement
import Parser.Statement
import CodeGen.Program

makeInstr :: String -> IO ()
makeInstr s
  = putStrLn assemblyCode
  where
  (Right (Just ((a,b),_))) =  runParser parseProgram s
  annotation = annotateAST a
  assemblyCode = intercalate "\n" (map show $ fst . fst $ genInstruction (genInstrFromAST annotation))

main = do
  args         <- getArgs
  let filename = head args
  contents     <- readFile filename

  let ast      = runParser parseProgram contents
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
  -- makeInstr contents
  exitSuccess
