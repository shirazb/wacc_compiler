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

boolAssignment = "begin bool b = false ; b = true end"

printInstrs :: AST -> IO ()
printInstrs
  = putStrLn . makeInstr

debug s = printInstrs ast
  where
  ast                       = annotateAST a
  (Right (Just ((a,b), _))) = runParser parseProgram s

makeInstr :: AST -> String
makeInstr a
  = space ++ dataSegment ++ "\n"   ++
    text                 ++ "\n\n" ++
    global               ++ "\n"   ++
    textInstrs           ++ "\n"   ++
    funcInstrs
  where
    ((((textSeg, functions), DataSeg dataSeg _), _), _) = genInstruction (genInstrFromAST a)
    textInstrs = showInstrs textSeg
    dataInstrs = intercalate "\n" (map show dataSeg)
    funcInstrs = space ++ intercalate ("\n" ++ space) (map show functions)
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

  let instrs = makeInstr annotatedAST
  -- writeFile ("./"  ++ (filename ++ ".s")) instrs
  printInstrs annotatedAST

  exitSuccess
