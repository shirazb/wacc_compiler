{- This module provides the ARM Assembly data types, show instances and
boilerplate code for our code generation output -}

module CodeGen.Assembly where
import Control.Monad.StateStack
import qualified Data.Map as Map
import Control.Monad.State
import Debug.Trace


class CodeGen a where
  codegen :: a -> InstructionMonad [Instr]


type Env = Map.Map String Int
type InstructionMonad a = StateStackT (Env, Int) (State Int) a


genInstruction p = runState (runStateStackT p (Map.empty, 0)) 0



{- ARM ASSEMBLY BOILERPLATE CODE -}

space, spaceX2, text, global, dataLabel, word, ascii :: String

space     = "    "
spaceX2   = space ++ space
text      = space ++ ".text"
global    = space ++ ".global main"
dataLabel = ".data"
word      = ".word"
ascii     = ".ascii"

-- POST:    Prints the the ARM Assembly message label and its number
-- EXAMPLE: "msg_0:" "msg_2:"
msg :: Int -> String
msg n = "msg_" ++ show n ++ ":"

type Label = String

{- ARM ASSEMBLY DATA TYPES -}

data Instr
  = Push Op
  | Pop Op
  | Mov Op Op
  | BL Label
  | LDR Op Op
  | STR Op [Op]
  | SUB Op Op Op
  deriving (Show)

data Op
  = OpReg Register
  | Imm Int
  | ImmLDR Int
  deriving (Show)

data Register
  = LR
  | PC
  | Reg Int
  | SP
  deriving (Show)

{- SHOW INSTANCES -}

-- instance Show Instr where
--   show (Push r)
--     = "PUSH {" ++ show r ++ "}"
--   show (Pop r)
--     = "POP {" ++ show r ++ "}"
--   show (Mov op op')
--     = "MOV " ++ show op ++ ", " ++ show op'
--   show (BL l)
--     = "BL " ++ l
--   show (LDR op op')
--     = "LDR " ++ show op ++ ", " ++ show op'
--
-- instance Show Op where
--   show (OpReg r)
--     = show r
--   show (Imm i)
--     = "#" ++ show i
--   show (ImmLDR i)
--     = "=" ++ show i
--
-- instance Show Register where
--   show LR
--     = "lr"
--   show PC
--     = "pc"
--   show (Reg i)
--     = "r" ++ show i
