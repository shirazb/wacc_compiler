{- This module provides the ARM Assembly data types, show instances and 
boilerplate code for our code generation output -}

module CodeGen.Assembly where

{- ARM ASSEMBLY BOILERPLATE CODE -}

text, global, semiC :: String

text   = "    .text"
global = "    .global"
semiC  = ":"

{- ARM ASSEMBLY DATA TYPES -}

data Instr
  = Push Op
  | Pop Op
  | Mov Op Op

data Op
  = Reg Int
  | Imm Int


{- SHOW INSTANCES-}

instance Show Instr where
  show (Push r)
    = "PUSH {" ++ show r ++ "}"
  show (Pop r)
    = "POP {" ++ show r ++ "}"
  show (Mov op op')
    = "MOV " ++ show op ++ ", " ++ show op'

instance Show Op where
  show (Reg i)
    = "r" ++ show i
  show (Imm i)
    = "#" ++ show i

