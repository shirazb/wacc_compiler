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

printInstrs :: String -> IO ()
printInstrs
  = putStrLn . makeInstr

makeInstr :: String -> String
makeInstr s
  = space ++ dataSegment ++ "\n"   ++
    text                 ++ "\n\n" ++
    global               ++ "\n"   ++
    textInstrs           ++ "\n"   ++
    funcInstrs
  where
    (Right (Just ((a,b),_))) =  runParser parseProgram s
    annotated  = annotateAST a
    ((((textSeg, functions), DataSeg dataSeg _), _), _) = genInstruction (genInstrFromAST annotated)
    textInstrs = showInstrs textSeg
    dataInstrs = intercalate "\n" (map show dataSeg)
    funcInstrs = space ++ concatMap show functions
    showInstrs = intercalate "\n" . map showInstr
    dataSegment = case dataInstrs of
                   [] -> ""
                   _ -> dataLabel ++ "\n\n" ++ dataInstrs ++ "\n"


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

  printInstrs contents
  exitSuccess
