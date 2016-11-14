{- This module provides the ARM Assembly data types, show instances and
boilerplate code for our code generation output -}

module CodeGen.Assembly where

class CodeGen a where
  codegen :: a -> [Instr]

{- ARM ASSEMBLY BOILERPLATE CODE -}

text, global, semiC :: String

space  = "    "
text   = space ++ ".text"
global = space ++ ".global "
semiC  = ":"

{- ARM ASSEMBLY DATA TYPES -}

data Instr
  = Push Op
  | Pop Op
  | Mov Op Op
  | BL String
  | LDR Op Op
  | ADDS Op Op Op
  | SUBS Op Op Op
  | SMULL Op Op Op Op

data Op
  = OpReg Register
  | Imm Int

data Register
  = LR
  | PC
  | Reg Int

{- SHOW INSTANCES -}

instance Show Instr where
  show (Push r)
    = "PUSH {" ++ show r ++ "}"
  show (Pop r)
    = "POP {" ++ show r ++ "}"
  show (Mov op op')
    = "MOV " ++ show op ++ ", " ++ show op'

instance Show Op where
  show (OpReg r)
    = show r
  show (Imm i)
    = "#" ++ show i

instance Show Register where
  show LR
    = "lr"
  show PC
    = "pc"
  show (Reg i)
    = "r" ++ show i
