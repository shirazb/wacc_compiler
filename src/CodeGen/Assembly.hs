{- This module provides the ARM Assembly data types, show instances and
boilerplate code for our code generation output -}

module CodeGen.Assembly where

import Control.Monad.StateStack
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Identity
import Data.Functor.Identity
import Data.Maybe (fromJust)

{- LOCAL IMPORTS -}
import Utilities.Definitions hiding (Env)


type DataSegment = String
class CodeGen a where
  codegen :: a -> InstructionMonad [Instr]

type Env = Map.Map String Int
type InstructionMonad a = StateStackT (Env, Int) (State Int) a

-- genInstruction :: InstructionMonad a -> ((a, (Map.Map k a1, t)), s)
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
  | LDR Size Indexing Op [Op]
  | STR Size Indexing Op [Op]
  | SUB Flag Op Op Op
  | ADD Flag Op Op Op
  | EOR Op Op Op
  | RSBS Op Op Op

-- include load immediate instructions

data Size = B | W | SB
data Indexing = Pre | Post | NoIdx
data Flag = S | NF

data Op
  = ImmI Int
  | ImmC Char
  | ImmLDRI Int
  | ImmLDRC Char
  | R0
  | R1
  | LR
  | PC
  | SP

typeSizes = [(BaseT BaseInt, W), (BaseT BaseChar, SB), (BaseT BaseBool, SB)]

scopeSize :: Stat -> Int
scopeSize (Declaration t _ _ _)
  = typeSize t
scopeSize (Seq s1 s2 _)
  = scopeSize s1 + scopeSize s2
scopeSize _
  = 0

typeSize :: Type -> Int
typeSize (BaseT BaseInt)
  = 4
typeSize (BaseT BaseBool)
  = 1
typeSize (BaseT BaseChar)
  = 1
typeSize _
  = error "not yet implemented"

sizeFromType :: Type -> Size
sizeFromType
  = fromJust . flip lookup typeSizes

{- SHOW INSTANCES -}
instance Show Size where
  show B = "B"
  show W = ""
  show SB = "SB"


instance Show Instr where
  show (Push r)
    = "PUSH {" ++ show r ++ "}"
  show (Pop r)
    = "POP {" ++ show r ++ "}"
  show (Mov op op')
    = "MOV " ++ show op ++ ", " ++ show op'
  show (BL l)
    = "BL " ++ l
  show (LDR s NoIdx op [op'])
    = "LDR" ++ show s ++ " " ++ show op ++ ", " ++ show op'
  show (LDR s i op ops)
    = "LDR" ++ show s ++ " " ++ show op ++ ", " ++ showIndexing i ops
  show (STR s i op ops)
    = "STR" ++ show s ++ " " ++ show op ++ ", " ++ showIndexing i ops
  show (SUB fl op op' op'')
    = "SUB " ++ show fl ++ " " ++ show op ++ ", " ++ show op' ++ ", " ++ show op''
  show (ADD fl op op' op'')
    = "ADD " ++ show fl ++ " " ++ show op ++ ", " ++ show op' ++ ", " ++ show op''
  show (EOR op op' op'')
    = "EOR " ++ show op ++ ", " ++ show op' ++ ", " ++ show op''
  show (RSBS op op' op'')
    = "RSBS " ++ show op ++ ", " ++ show op' ++ ", " ++ show op''

instance Show Flag where
  show S
    = "S"
  show NF
    = ""
showIndexing :: Indexing -> [Op] -> String
showIndexing index ops
  = case index of
    Pre  -> show ops ++ "!"
    Post -> show (init ops) ++ ", " ++ show (last ops)
    NoIdx -> show ops

instance Show Op where
  show R0
    = "r0"
  show (ImmI i)
    = "#" ++ show i
  show (ImmC c)
    = "#" ++ show c
  show (ImmLDRI i)
    = "=" ++ show i
  show (ImmLDRC c)
    = "=" ++ show c
  show LR
    = "lr"
  show PC
    = "pc"
  show SP
    = "sp"
