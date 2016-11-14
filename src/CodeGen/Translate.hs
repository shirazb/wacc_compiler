{- This module translates the AST into ARM Assembly code -}

module CodeGen.Translate where

{- LOCAL IMPORTS -}

import CodeGen.Assembly
import CodeGen.Statement
import Utilities.Definitions

translateAST :: AST -> IO ()
translateAST (Program _ s) = putStr (output s)

output :: Stat -> String
output s
  = text ++ "\n\n" ++ global ++ "main" ++ "\n\n" ++
    space ++ "main" ++ ":" ++ "\n" ++
    space ++ space ++ show (Push (OpReg LR)) ++ "\n" ++         
    space ++ space ++ show (head (codegen s)) ++ "\n" ++
    space ++ space ++ show (Pop (OpReg PC)) ++ "\n"

prog :: AST
prog = Program [] (Skip (1,12))

