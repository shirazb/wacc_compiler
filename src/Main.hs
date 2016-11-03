{-
A parser built using parser combinators for the WACC language. Parser
combinators are used because of the flexibility and modularity that they
offer. Building a parser combinator in Haskell also serves as a learning
experience to learn the more advanced features of Haskell. The parser
currently has no error handling.
-}

import Parser.Program
import System.Environment
import Utilities.Declarations (runParser)
import Semantics.Annotators.AST
import Semantics.ScopeChecker

validProgram = "begin int f(int f) is f = 2; return f end int g(int f) is f = 6; return f end int x = call f(2); int y = 3; if x == 3 then println x else println y fi; begin println y end; println x; println y end "
notInScope = "begin x = 4 end "
blockDoesShadow = "begin int x = 4; int y = 3; begin string x = 3; println y end end "
blockRedeclares = "begin int x = 4; begin string x = \"sdfsdf\"; println x end end "
redeclaration = "begin int x = 2; int x = 2 end"
quickSanityCheck = "begin int f() is return 1 end println 2 end"
duplicateFunc = "begin int f() is int f = 2; return f end int f(int f) is f = 6; return f end println 2 end"
recursive = "begin int f() is int x = call f(); return x end skip end "
duplicateParams = "begin int f(int x, int x, char x) is int x = 3; return 2 end skip end "
bareErrors = "begin int f() is f = 2; return f end int g(int f) is g = 6; return f end int z = 2; int x = call z(); begin char x = 4; x = 2 end end"
while = "begin while (x == 2) do char c = 3; c = 2; x = c done end "
nested = "begin while (x == 2) do int[] x = [1, 2, z]; if (cond) then string x = 3 else x = 5 fi done end"
redec = "begin int x = 2; begin string x = x + 3 end end"
array = "begin int[] z = [12]; int[][] x = [1,2]; x[1] = [z] end"
pairTest = "begin int x = 2; pair(int, int) p = newpair(1,1); pair(int, char) z = newpair(1, 'c'); fst p = fst z end"
funcP = "begin int func() is return 1 end  int x = call func(1) end"
pairTest2 = "begin pair(int, int) p = newpair(1,2); pair(int, int) z = newpair(1,3); fst p = fst z end"
scopeError = "begin int x = 3; y = 3 end"

main
  = do
      args        <- getArgs
      let filename = head args
      contents    <- readFile filename
      putStrLn "------------------------------------------------"
      putStrLn "           THE PROGRAM WE HAVE PARSED           "
      putStrLn "------------------------------------------------"
      case runParser parseProgram contents (0,0) of
        Right (Just ((a,b), _)) -> print (annotateAST a)
        Left err                -> print err
        Right Nothing           -> print "Program Failure"
      return ()
