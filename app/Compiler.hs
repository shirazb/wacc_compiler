import System.Environment
import Parser
import Utility.Declarations
main = do
  args           <- getArgs
  let fileName   =  head args  
  program        <- readFile fileName
  let ast        =  fst . head $ parse parseProgram program
  print ast
