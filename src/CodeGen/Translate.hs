{- This module translates the AST into ARM Assembly code -}

module CodeGen.Translate where

{- LOCAL IMPORTS -}

import CodeGen.Assembly
import CodeGen.Statement
import Utilities.Definitions

-- POST: Takes an AST and translates it to ARM Assembly code
translateAST :: AST -> IO ()
translateAST (Program _ s) = putStr (output s)

-- POST: Given a statement from a program tree (AST), it converts the statement
--       into ARM Assembly code represented as a string
output :: Stat -> String
output s
  = text ++ "\n\n" ++ global ++ "\n\n" ++
    space ++ "main:" ++ "\n" ++
    spaceX2 ++ show (Push (OpReg LR)) ++ "\n" ++
    spaceX2 ++ concatMap ((flip (++) ("\n" ++ spaceX2)) . show) (codegen s) ++
    show (Pop (OpReg PC)) ++ "\n"

{- TEST PROGRAMS -}

prog1 :: AST
prog1 = Program [] (Skip (1,12))

prog2 :: AST
prog2 = Program [] (Exit (IntLit 256 (1,12)) (1,7))