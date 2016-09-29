import System.Environment
import Parser
main = do
  args           <- getArgs
  let fileName   =  head args  
  program        <- readFile fileName
  let ast        =  runParser program
  print ast
