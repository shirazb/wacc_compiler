{- This module provides the ARM11 Assembly data types, show instances and
boilerplate code for our code generation output -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module CodeGen.Assembly where

import Control.Monad.StateStack
import qualified Data.Map as Map
import Control.Monad.State.Strict (get, put, lift, State, runState, StateT (..))
import Data.Maybe                 (fromJust)
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Applicative
import Data.List
import Data.Tuple

{- LOCAL IMPORTS -}
import Utilities.Definitions hiding (Env)

class CodeGen a where
  codegen :: a -> CodeGenerator [Instr]

genInstruction :: CodeGenerator a -> ((((a, Functions), DataSegment),
                  (Env, StackOffset)), LabelNumber)
genInstruction p
  = runState (runStateStackT (runStateT (runStateT p [])
    (DataSeg mzero startMsgNum)) (Map.empty, startOffset)) startLabelNum
  where
  startMsgNum          = 0
  startOffset          = 0
  startLabelNum        = 0

{- TYPE AND DATA INSTANCES -}

type Label           = String
type LabelNumber     = Int
type StackOffset     = Int
type MsgNumber       = Int
type TextSegment     = [Instr]
type Functions       = [AssemblyFunc]
type Env             = Map.Map String Int
type CodeGenerator a = StateT Functions (StateT DataSegment (StateStackT
                       (Env, Int) (State Int))) a

data DataSegment
  = DataSeg [Data] MsgNumber
  deriving (Show)

data Instr
  = Push [Reg]
  | Pop [Reg]
  | Mov Reg Op
  | BT Label
  | BL Label
  | BEQ Label
  | BLEQ Label
  | BLLT Label
  | BLCS Label
  | BLVS Label
  | BLNE Label
  | LDR Size Indexing Reg [Op]
  | LDREQ Size Indexing Reg [Op]
  | LDRNE Size Indexing Reg [Op]
  | LDRLT Size Indexing Reg [Op]
  | LDRCS Size Indexing Reg [Op]
  | STR Size Indexing Reg [Op]
  | SUB Flag Reg Reg Op2
  | ADD Flag Reg Reg Op2
  | EOR Reg Reg Op
  | RSBS Reg Reg Op
  | CMP Reg Op2
  | SMULL Reg Reg Reg Reg
  | Def Label
  | MOVLT Reg Op2
  | MOVGE Reg Op2
  | MOVLE Reg Op2
  | MOVGT Reg Op2
  | MOVEQ Reg Op2
  | MOVNE Reg Op2
  | LTORG

data Data
  =  MSG Int String Int

data AssemblyFunc
  = FuncA String [Instr]

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
  | MsgName Int

{- MAPPING FROM TYPES TO SIZES DEPENDENT ON THE INSTRUCTION -}

typeSizes, typeSizesLDR, typeSizesSTR :: [(Type, Size)]
typeSizes = [(BaseT BaseInt, W), (BaseT BaseChar, B),
             (BaseT BaseBool, SB), (PolyArray, W), (PolyPair, W),
             (BaseT BaseString, W)]

typeSizesLDR = [(BaseT BaseInt, W), (BaseT BaseChar, SB),
                (BaseT BaseBool, SB), (PolyArray, W), (PolyPair, W),
                (BaseT BaseString, W)]

typeSizesSTR = [(BaseT BaseInt, W), (BaseT BaseChar, B),
                (BaseT BaseBool, B), (PolyArray, W), (PolyPair, W),
                (BaseT BaseString, W)]

{- FUNCTIONS TO CALCULATE SIZE IN BYTES -}

-- POST: Returns size of variables declared at the top level
scopeSize :: Stat -> Int
scopeSize (Declaration t _ _ _)
  = typeSize t
scopeSize (Seq s1 s2 _)
  = scopeSize s1 + scopeSize s2
scopeSize _
  = 0

-- POST: Returns the size of a pointer
pointerSize :: Int
pointerSize
  = 4

-- POST: Returns the size of a pointer to a pair
pairSizeHeap :: Int
pairSizeHeap
  = pointerSize * 2

-- POST: Returns the size of a type
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
typeSize PairT{}
  = pointerSize
typeSize (BaseT BaseString)
  = pointerSize
typeSize t
  = error $ "Cannot call typeSize on type \'" ++ show t ++ "\'"

-- POST: Given a mapping for a kind of instruction, returns the appropriate
--       size of a type
sizeFromType :: [(Type, Size)] -> Type -> Size
sizeFromType ts
  = fromJust . flip lookup ts

{- UTILITY FUNCTIONS FOR INTERACTING WITH THE MONAD STACK -}

-- POST: Returns the list of defined functions
getFunctionInfo :: CodeGenerator Functions
getFunctionInfo
 = get

-- POST: Replaces the list of functions
putFunctionInfo :: Functions -> CodeGenerator ()
putFunctionInfo
  = put

-- POST: Adds a new function definition to the list of functions iff it is not
--       already defined
addFunction :: AssemblyFunc -> CodeGenerator ()
addFunction f@(FuncA s _) = do
  fs <- getFunctionInfo
  when (checkFuncDefined s fs) $
    do putFunctionInfo (fs ++ [f])
       return ()

-- POST: Returns the next available message number and increments the message
--       counter
getNextMsgNum :: CodeGenerator Int
getNextMsgNum = do
  DataSeg ds num <- getData
  putData (DataSeg ds (num + 1))
  return num

-- POST: Returns pairs of instructions to allocate and deallocate stack space
manageStack :: Int -> CodeGenerator ([Instr], [Instr])
manageStack offset =  if offset > maxStackAlloc then
  do
     let  newOffset = offset - maxStackAlloc
     (saveStack, clearStack) <- manageStack newOffset
     return (SUB NF SP SP (ImmOp2 maxStackAlloc) : saveStack,
             ADD NF SP SP (ImmOp2 maxStackAlloc) : clearStack)
  else return ([SUB NF SP SP (ImmOp2 offset)], [ADD NF SP SP (ImmOp2 offset)])
  where
    maxStackAlloc = 1024

-- POST: Adds given string to data segment iff not already
--       defined, returns numer of definition.
addUniqueData :: String -> CodeGenerator Int
addUniqueData s = do
  DataSeg ds num    <- getData
  if checkDataDefined s ds
    then return (getMsgNumData s ds)
    else addData s

-- POST: Adds given data to the data segment unconditionally, returns the
--       message number
addData :: String -> CodeGenerator Int
addData s = do
  -- Generates new data using the supplied string, replacing all escaped
  -- chars to ensure they are actually escaped and not consumed by haskell
  msgNum         <- getNextMsgNum
  let msg         = [MSG msgNum (replaceEscapeChar s) (length s)]
  -- Adds the generated data to the data segment
  DataSeg ds num <- getData
  putData (DataSeg (ds ++ msg) num)
  return msgNum

-- PRE:  String is already defined and has been assigned to a number
-- POST: Returns the definition number of the supplied string
getMsgNumData :: String -> [Data] -> Int
getMsgNumData s ds
  = head [ n | MSG n s' _ <- ds, escapedString == s' ]
  where
    escapedString = replaceEscapeChar s

-- POST: Returns true iff message is not defined
checkDataDefined :: String -> [Data] -> Bool
checkDataDefined s msgs
  = or [escapedString == s' | MSG _ s' _ <- msgs ]
  where
    escapedString = replaceEscapeChar s

-- POST: Properly escapes all escape chars found in string
--       ensuring it is not consumed by haskell.
replaceEscapeChar :: String -> String
replaceEscapeChar []
  = []
replaceEscapeChar (c : cs)
  | c `elem` escapeChars = '\\' : newChar : replaceEscapeChar cs
  | otherwise            = c : replaceEscapeChar cs
  where
    escapeChars = map snd escapeCharList
    newChar     = fromJust $ lookup c (map swap escapeCharList)

-- POST: Returns the data segment
getData :: CodeGenerator DataSegment
getData  = lift get

-- POST: Replaces the data segment with the supplied data segment
putData :: DataSegment -> CodeGenerator ()
putData = lift . put

-- POST: Pushes the current state on the stack
saveStackInfo :: CodeGenerator ()
saveStackInfo
  = lift (lift save)

-- POST: Pops the state of the stack
restoreStackInfo :: CodeGenerator ()
restoreStackInfo
  = lift (lift restore)

-- POST:    Updates the next label number with the given number
-- EXAMPLE: Do you even lift?
updateNextLabelNum :: Int -> CodeGenerator ()
updateNextLabelNum
  = lift . lift . lift . put

-- POST: Retrieves the next label number
getNextLabelNum :: CodeGenerator Int
getNextLabelNum
  = lift . lift . lift $ get

-- POST: Returns the String "L:" appended with the next label num, and updates
--       the label number
getNextLabel :: CodeGenerator String
getNextLabel = do
  labelNum <- getNextLabelNum
  updateNextLabelNum (labelNum + 1)
  return $ "L" ++ show labelNum

-- POST: Replaces the state at the top of the stack.
putStackInfo :: (Env, Int) -> CodeGenerator ()
putStackInfo
  = lift . lift . put

-- POST: Gets the state at the top of the stack
getStackInfo :: CodeGenerator (Env, Int)
getStackInfo
  = lift (lift get)

-- POST: Increments the stack pointer and modifys the variable mappings
incrementOffset :: Int -> CodeGenerator ()
incrementOffset n = do
  (env, offset) <- getStackInfo
  let newEnv = Map.map (+ n) env
  putStackInfo (newEnv, offset + n)

-- POST: Decrements the stack pointer and modifys the variable mappings
decrementOffset :: Int -> CodeGenerator ()
decrementOffset n = do
  (env, offset) <- getStackInfo
  let newEnv = Map.map (\x -> x - n) env
  putStackInfo (newEnv, offset - n)

-- POST: Generates push and pop instructions and modifies the variable mappings.
push, pop :: [Reg] -> CodeGenerator [Instr]
push rs = do
  incrementOffset (4 * length rs)
  return [Push rs]
pop rs = do
  decrementOffset (4 * length rs)
  return [Pop rs]

-- POST: Returns true iff function is not defined
checkFuncDefined :: String -> Functions -> Bool
checkFuncDefined s fs
  = not $ or [ s == s' | FuncA s' _ <- fs ]

{- ARM11 ASSEMBLY BOILERPLATE CODE -}

space, spaceX2, text, global, dataLabel, word, ascii :: String
spaceGen :: Int -> String
spaceGen n
  = concat $ replicate n space
space     = "    "
spaceX2   = space ++ space
text      = space ++ ".text"
global    = space ++ ".global main"
dataLabel = ".data"
word      = ".word"
ascii     = ".ascii"

{- SHOW INSTANCES -}

instance Show AssemblyFunc where
  show (FuncA name body)
    = name ++ ":" ++ "\n"
      ++ intercalate "\n" (map showInstr body)

instance Show Data where
  show (MSG i s l)
    = space ++ "msg_"  ++ show i   ++ ":\n" ++
      spaceGen 3 ++ ".word  " ++ show l ++ "\n" ++
      spaceGen 3 ++ ".ascii  " ++ "\"" ++ s ++ "\""

instance Show Size where
  show B = "B"
  show W = ""
  show SB = "SB"

instance Show Instr where
  show (Push r)
    = "PUSH {" ++ intercalate ", " (map show r) ++ "}"
  show (Pop r)
    = "POP {" ++ intercalate ", " (map show r) ++ "}"
  show (Mov op op')
    = "MOV " ++ show op ++ ", " ++ show op'
  show (BT l)
    = "B " ++ l
  show (BL l)
    = "BL " ++ l
  show (BLEQ l)
    = "BLEQ " ++ l
  show (BEQ l)
    = "BEQ " ++ l
  show (BLLT l)
    = "BLLT " ++ l
  show (BLCS l)
    = "BLCS " ++ l
  show (BLNE l)
    = "BLNE " ++ l
  show (BLVS l)
    = "BLVS " ++ l
  show (LDR s NoIdx reg [op'])
    = "LDR" ++ showSingleInstr s reg op'
  show (LDREQ s NoIdx reg [op'])
    = "LDREQ" ++ showSingleInstr s reg op'
  show (LDRLT s NoIdx reg [op'])
    = "LDRLT" ++ showSingleInstr s reg op'
  show (LDRCS s i reg [op'])
    = "LDRCS" ++ showSingleInstr s reg op'
  show (LDRNE s o reg [op'])
    = "LDRNE" ++ showSingleInstr s reg op'
  show (LDR s i reg ops)
    = "LDR" ++ showSizeIndexingRegOps s i reg ops
  show (LDREQ s i reg ops)
    = "LDREQ" ++ showSizeIndexingRegOps s i reg ops
  show (LDRLT s i reg ops)
    = "LDRLT" ++ showSizeIndexingRegOps s i reg ops
  show (LDRCS s i reg ops)
    = "LDRCS" ++ showSizeIndexingRegOps s i reg ops
  show (LDRNE s i reg ops)
    = "LDRNE" ++ showSizeIndexingRegOps s i reg ops
  show (STR s i reg ops)
    = "STR" ++ showSizeIndexingRegOps s i reg ops
  show (SUB fl reg op' op2)
    = "SUB" ++ show fl ++ " " ++ show reg ++ ", " ++ show op' ++ ", " ++
      show op2
  show (ADD fl reg op' op2)
    = "ADD" ++ show fl ++ " " ++ show reg ++ ", " ++ show op' ++ ", " ++
      show op2
  show (EOR reg op' op'')
    = "EOR " ++ show reg ++ ", " ++ show op' ++ ", " ++ show op''
  show (RSBS reg op' op'')
    = "RSBS " ++ show reg ++ ", " ++ show op' ++ ", " ++ show op''
  show (SMULL r0 r1 r2 r3)
    = "SMULL " ++ show r0 ++ ", " ++ show r1 ++ ", " ++ show r2 ++ ", " ++
      show r3
  show (CMP reg op2)
    = "CMP " ++ show reg ++ ", " ++ show op2
  show (MOVLE reg op2)
    = "MOVLE " ++ show reg ++ ", " ++ show op2
  show (MOVGT reg op2)
    = "MOVGT " ++ show reg ++ ", " ++ show op2
  show (MOVLT reg op2)
    = "MOVLT " ++ show reg ++ ", " ++ show op2
  show (MOVGE reg op2)
    = "MOVGE " ++ show reg ++ ", " ++ show op2
  show (MOVEQ reg op2)
    = "MOVEQ " ++ show reg ++ ", " ++ show op2
  show (MOVNE reg op2)
    = "MOVNE " ++ show reg ++  ", " ++ show op2
  show (Def l)
    = l ++ ":"
  show LTORG
     = ".ltorg"

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
  show (MsgName i)
    = "=msg_" ++ show i

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

{- UTILITY FUNCTIONS FOR SHOW INSTANCES -}

-- POST: Produces a string which shows a size, index, reg and list of operands
showSizeIndexingRegOps :: Size -> Indexing -> Reg -> [Op] -> String
showSizeIndexingRegOps s i reg ops
  = show s ++ " " ++ show reg ++ ", " ++ showIndexing i ops

-- POST: Produces the correct string representation depending on the type of
--       indexing
showIndexing :: Indexing -> [Op] -> String
showIndexing index ops = case index of
  Pre   -> show ops ++ "!"
  Post  -> show (init ops) ++ ", " ++ show (last ops)
  NoIdx -> show ops

-- POST: Produces a string for load and store instructions which have a
--       singleton operand list
showSingleInstr :: Size -> Reg -> Op -> String
showSingleInstr s reg op'
  = show s ++ " " ++ show reg ++ ", " ++ showSingleOp op'

-- POST: Produces a string which shows a single operand
showSingleOp :: Op -> String
showSingleOp op = case op of
  RegOp _ -> "[" ++ show op ++ "]"
  _       -> show op

-- POST: Produces a correctly indented string representation for an instruction
showInstr :: Instr -> String
showInstr label@(Def s)
  = space ++ show label
showInstr instr
  = spaceX2 ++ show instr
