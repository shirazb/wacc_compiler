{- This module generates ARM11 Assmebly code from an AST -}

module CodeGen.AssemblyGenerator where

import Data.List

{- LOCAL IMPORTS -}
import CodeGen.Assembly
import CodeGen.Statement
import CodeGen.Program
import Utilities.Definitions

-- POST: Generates ARM11 Assembly instructions from the given AST and formats
--       them correctly to be written to a file
makeInstr :: AST -> String
makeInstr a
  = space ++ dataSegment ++ "\n"   ++
    text                 ++ "\n\n" ++
    global               ++ "\n"   ++
    textInstrs           ++ "\n"   ++
    funcInstrs
  where
    ((((((textSeg, functions), DataSeg dataSeg _), _), _), _), _) 
      = genInstruction (genInstrFromAST a)
    textInstrs = showInstrs textSeg
    dataInstrs = intercalate "\n" (map show dataSeg)
    funcInstrs = space ++ intercalate ("\n" ++ space) (map show functions)
    showInstrs = intercalate "\n" . map showInstr
    dataSegment = case dataInstrs of
                   [] -> ""
                   _ -> dataLabel ++ "\n\n" ++ dataInstrs ++ "\n"
