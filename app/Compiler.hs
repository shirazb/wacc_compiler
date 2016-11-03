import System.Environment
import Parser.Program
main = do
  args           <- getArgs
  let fileName   =  head args
  program        <- readFile fileName

  let (compiled,annotatedCompiled)   = makeAST program
  print compiled
  putStrLn "THIS IS THE ANNOTATED AST"
  putStrLn "#########################"
  putStrLn "#########################"
  putStrLn "#########################"
  print annotatedCompiled
  return ()
