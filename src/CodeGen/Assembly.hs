{- This module provides the ARM Assembly data types, show instances and
boilerplate code for our code generation output -}

module CodeGen.Assembly where



import Control.Monad.StateStack
import qualified Data.Map as Map
import Control.Monad.State
import Data.Maybe (fromJust)

{- LOCAL IMPORTS -}
import Utilities.Definitions hiding (Env)

type DataSegment = String

class CodeGen a where
  codegen :: a -> InstructionMonad [Instr]

type Env = Map.Map String Int
type InstructionMonad a = StateStackT (Env, Int) (State Int) a

updateNextLabelNum :: Int -> InstructionMonad ()
updateNextLabelNum
  = lift . put

getNextLabelNum :: InstructionMonad Int
getNextLabelNum
  = lift get

getNextLabel :: InstructionMonad String
getNextLabel = do
  labelNum <- getNextLabelNum
  updateNextLabelNum (labelNum + 1)
  return $ "L" ++ show labelNum

incrementOffset :: Int -> InstructionMonad ()
incrementOffset n = do
  (env, offset) <- get
  put (env, offset + n)

decrementOffset :: Int -> InstructionMonad ()
decrementOffset n = do
  (env, offset) <- get
  put (env, offset - n)

-- Assert Ops are registers
-- Could avoid duplication.
push, pop :: [Op] -> InstructionMonad [Instr]
push []
  = return []
push (x : xs) = do
  let pushX = [Push x]
  incrementOffset 4
  pushRest <- push xs
  return $ pushX ++ pushRest
pop []
  = return []
pop (x : xs) = do
  let popX = [Pop x]
  decrementOffset 4
  popRest <- pop xs
  return $ popX ++ popRest

genInstruction :: InstructionMonad a -> ((a, (Env, Int)), Int)
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
  | BT Label   -- branch true, or "B". B constructor already in use.
  | BL Label
  | BEQ Label
  | LDR Size Indexing Op [Op]
  | STR Size Indexing Op [Op]
  | SUB Flag Op Op Op2
  | ADD Flag Op Op Op2
  | EOR Op Op Op
  | RSBS Op Op Op
  | CMP Op Op2
  | SMULL Op Op Op Op
  | Def Label

-- include load immediate instructions

data Size
  = B
  | W
  | SB

data Indexing
  = Pre
  | Post
  | NoIdx

data Flag
 = S
 | NF

data Shift
  =  LSL
   | LSR
   | ASR
   | ROR
   | NoShift
   deriving (Show)

data Op2
  =  ImmOp2 Int
   | RegOp2 Op           -- Op must be a register
   | RegShift Op Shift   -- Op must be a register
   | Shift Op Shift Int  -- Op must be a register
   | NoOp2

data Op
  = ImmI Int
  | ImmC Char
  | ImmLDRI Int
  | ImmLDRC Char
  | R0
  | R1
  | R4
  | LR
  | PC
  | SP

{- Map from types to sizes -}

typeSizes = [(BaseT BaseInt, W), (BaseT BaseChar, SB), (BaseT BaseBool, SB)]

{- Utility Functions -}

scopeSize :: Stat -> Int
scopeSize (Declaration t _ _ _)
  = typeSize t
scopeSize (Seq s1 s2 _)
  = scopeSize s1 + scopeSize s2
scopeSize _
  = 0

pointerSize :: Int
pointerSize
  = 4

typeSize :: Type -> Int
typeSize (BaseT BaseInt)
  = 4
typeSize (BaseT BaseBool)
  = 1
typeSize (BaseT BaseChar)
  = 1
typeSize (ArrayT _ innerType)
  = pointerSize
typeSize _
  = error "not yet implemented"

sizeFromType :: Type -> Size
sizeFromType
  = fromJust . flip lookup typeSizes

showIndexing :: Indexing -> [Op] -> String
showIndexing index ops
  = case index of
    Pre  -> show ops ++ "!"
    Post -> show (init ops) ++ ", " ++ show (last ops)
    NoIdx -> show ops


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
  show (BT l)
    = "B " ++ l
  show (BL l)
    = "BL " ++ l
  show (BEQ l)
    = "BEQ " ++ l
  show (LDR s NoIdx op [op'])
    = "LDR" ++ show s ++ " " ++ show op ++ ", " ++ show op'
  show (LDR s i op ops)
    = "LDR" ++ show s ++ " " ++ show op ++ ", " ++ showIndexing i ops
  show (STR s i op ops)
    = "STR" ++ show s ++ " " ++ show op ++ ", " ++ showIndexing i ops
  show (SUB fl op op' op2)
    = "SUB " ++ show fl ++ " " ++ show op ++ ", " ++ show op' ++ ", " ++ show op2
  show (ADD fl op op' op2)
    = "ADD " ++ show fl ++ " " ++ show op ++ ", " ++ show op' ++ ", " ++ show op2
  show (EOR op op' op'')
    = "EOR " ++ show op ++ ", " ++ show op' ++ ", " ++ show op''
  show (RSBS op op' op'')
    = "RSBS " ++ show op ++ ", " ++ show op' ++ ", " ++ show op''
  show (Def l)
    = l ++ ":"

instance Show Flag where
  show S
    = "S"
  show NF
    = ""

instance Show Op where
  show R0
    = "r0"
  show R1
    = "r1"
  show R4
    = "r4"
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

instance Show Op2 where
  show (ImmOp2 n)
    = "#" ++ show n
  show (RegOp2 op)
    = show op
  show (RegShift reg shift)
    = show shift ++ " " ++ show reg
  show (Shift reg shift amount)
    = show reg ++ ", " ++ show shift ++ " #" ++ show amount
