{- This module provides the ARM Assembly data types, show instances and
boilerplate code for our code generation output -}

module CodeGen.Assembly where

import Control.Monad.StateStack
import qualified Data.Map as Map
import Control.Monad.State.Strict (get, put, lift, State, runState)
import Data.Maybe (fromJust)

{- LOCAL IMPORTS -}
import Utilities.Definitions hiding (Env)

type DataSegment = String

class CodeGen a where
  codegen :: a -> InstructionMonad [Instr]

type Env = Map.Map String Int
type InstructionMonad a = StateStackT (Env, Int) (State Int) a


getOffset :: Stat -> Int
getOffset s
  = scopeSize s - sizeOfFirstType s

sizeOfFirstType :: Stat -> Int
sizeOfFirstType (Declaration t _ _ _ )
  = typeSize t
sizeOfFirstType (Seq (Declaration t _ _ _) _ _)
  = typeSize t
sizeOfFirstType (Seq s1 s2 _)
  = sizeOfFirstType s2
sizeOfFirstType _
  = 0

-- Updates the next label number with the given number
updateNextLabelNum :: Int -> InstructionMonad ()
updateNextLabelNum
  = lift . put

-- Retrieves the next label number
getNextLabelNum :: InstructionMonad Int
getNextLabelNum
  = lift get

-- Returns the String "L:" appended with the next label num, and updates the label number
getNextLabel :: InstructionMonad String
getNextLabel = do
  labelNum <- getNextLabelNum
  updateNextLabelNum (labelNum + 1)
  return $ "L" ++ show labelNum

-- Increments the offset of the stack pointer (to the start of the scope)
incrementOffset :: Int -> InstructionMonad ()
incrementOffset n = do
  (env, offset) <- get
  put (env, offset + n)

-- Decrements the offset of the stack pointer (to the start of the scope)
decrementOffset :: Int -> InstructionMonad ()
decrementOffset n = do
  (env, offset) <- get
  put (env, offset - n)

-- Assert Ops are registers
-- Could avoid duplication.
push, pop :: [Reg] -> InstructionMonad [Instr]
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
  = Push Reg
  | Pop Reg
  | Mov Reg Op
  | BT Label   -- branch true, or "B". B constructor already in use.
  | BL Label
  | BEQ Label
  | LDR Size Indexing Reg [Op]
  | STR Size Indexing Reg [Op]
  | SUB Flag Reg Reg Op2
  | ADD Flag Reg Reg Op2
  | EOR Reg Reg Op
  | RSBS Reg Reg Op
  | CMP Reg Op2
  | SMULL Reg Reg Reg Reg
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
   | RegOp2 Reg
   | RegShift Reg Shift
   | Shift Reg Shift Int
   | NoOp2

data Reg
  =  R0
   | R1
   | R2
   | R3
   | R4
   | R5
   | R6
   | R7
   | R8
   | R9
   | R10
   | R11
   | SP
   | LR
   | PC

data Op
  = ImmI Int
  | ImmC Char
  | ImmLDRI Int
  | ImmLDRC Char
  | RegOp Reg

{- Map from types to sizes -}

typeSizes = [(BaseT BaseInt, W), (BaseT BaseChar, SB), (BaseT BaseBool, SB)]

{- Utility Functions -}

-- Returns the size of a scope
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
typeSize Pair
  = pointerSize
typeSize t
  = error $ "Cannot call typeSize on type \'" ++ show t ++ "\'"

sizeFromType :: Type -> Size
sizeFromType
  = fromJust . flip lookup typeSizes

showIndexing :: Indexing -> [Op] -> String
showIndexing index ops = case index of
    Pre   -> show ops ++ "!"
    Post  -> show (init ops) ++ ", " ++ show (last ops)
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
  show (ImmI i)
    = "#" ++ show i
  show (ImmC c)
    = "#" ++ show c
  show (ImmLDRI i)
    = "=" ++ show i
  show (ImmLDRC c)
    = "=" ++ show c
  show (RegOp reg)
    = show reg

instance Show Reg where
  show LR
    = "lr"
  show PC
    = "pc"
  show SP
    = "sp"
  show R0
    = "r0"
  show R1
    = "r1"
  show R2
    = "r2"
  show R3
    = "r3"
  show R4
    = "r4"
  show R5
    = "r5"
  show R6
    = "r6"
  show R7
    = "r7"
  show R8
    = "r8"
  show R9
    = "r9"
  show R10
    = "r10"
  show R11
    = "r11"

instance Show Op2 where
  show (ImmOp2 n)
    = "#" ++ show n
  show (RegOp2 op)
    = show op
  show (RegShift reg shift)
    = show shift ++ " " ++ show reg
  show (Shift reg shift amount)
    = show reg ++ ", " ++ show shift ++ " #" ++ show amount
