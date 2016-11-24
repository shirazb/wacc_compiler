{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{- This module provides the ARM Assembly data types, show instances and
boilerplate code for our code generation output -}

module CodeGen.Assembly where

import Control.Monad.StateStack
import qualified Data.Map as Map
import Control.Monad.State.Strict (get, put, lift, State, runState, StateT (..))
import Data.Maybe (fromJust)
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Applicative
import Data.List
import Debug.Trace

{- LOCAL IMPORTS -}
import Utilities.Definitions hiding (Env)

type Label = String

type TextSegment = [Instr]
data DataSegment = DataSeg [Data] Int deriving (Show)
type Functions = [AssemblyFunc]
type Env = Map.Map String Int
-- type AssemblyProgram = (DataSegment, TextSegment, Functions)

type CodeGenerator a = StateT Functions (StateT DataSegment (StateStackT (Env, Int) (State Int))) a


-- instance (Monad m, Alternative m, MonadPlus m) => Alternative (StateStackT s m) where
--   empty = lift empty
--   s <|> s' = StateStackT $ unStateStackT s <|> unStateStackT s'
-- --
-- -- instance (Alternative Identity) => MonadPlus Identity where
-- --   mzero = empty
-- --   mplus i i' = i
-- --
-- --
-- instance MonadPlus m => MonadPlus (StateStackT s m) where
--   mzero = lift mzero
--   s `mplus` s' = StateStackT $ unStateStackT s `mplus` unStateStackT s'



class CodeGen a where
  codegen :: a -> CodeGenerator [Instr]

-- genInstruction :: CodeGenerator a -> ((a, (Env, Int)), Int)
genInstruction :: CodeGenerator a -> ((((a, Functions), DataSegment), (Env, Int)), Int)
genInstruction p
  = runState (runStateStackT (runStateT (runStateT p []) (DataSeg mzero 0)) (Map.empty, 0)) 0

{- ARM ASSEMBLY DATA TYPES -}
-- we need to change mov to take Op2
-- instead of op1 but we will change it later
-- because it dosent make much diffrence now

data Instr
  = Push [Reg]
  | Pop [Reg]
  | Mov Reg Op
  | BT Label   -- branch true, or "B". B constructor already in use.
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


data Data
  =  MSG Int String

data AssemblyFunc
  = FuncA String [Instr]
-- POST: Produces a correctly indented string representation for an instruction
showInstr :: Instr -> String
showInstr label@(Def s)
  = space ++ show label
showInstr instr
  = spaceX2 ++ show instr

instance Show AssemblyFunc where
  show (FuncA name body)
    = name ++ ":" ++ "\n"
      ++ intercalate "\n" (map showInstr body)

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
  | MsgName Int

{- Map from types to sizes -}

typeSizes, typeSizesLDR, typeSizesSTR :: [(Type, Size)]
typeSizes = [(BaseT BaseInt, W), (BaseT BaseChar, B),
             (BaseT BaseBool, SB), (PolyArray, W), (PolyPair, W), (BaseT BaseString, W)]

typeSizesLDR = [(BaseT BaseInt, W), (BaseT BaseChar, SB),
                (BaseT BaseBool, SB), (PolyArray, W), (PolyPair, W), (BaseT BaseString, W)]

typeSizesSTR = [(BaseT BaseInt, W), (BaseT BaseChar, B),
                (BaseT BaseBool, B), (PolyArray, W), (PolyPair, W), (BaseT BaseString, W)]

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

pairSize :: Int
pairSize
  = pointerSize * 2

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

sizeFromType :: [(Type, Size)] -> Type -> Size
sizeFromType ts
  = fromJust . flip lookup ts

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

getFunctionInfo :: CodeGenerator Functions
getFunctionInfo
 = get

putFunctionInfo :: Functions -> CodeGenerator ()
putFunctionInfo
  = put

addFunction :: AssemblyFunc -> CodeGenerator ()
addFunction f@(FuncA s _) = do
  fs <- getFunctionInfo
  when (checkFuncDefined s fs) $
    do putFunctionInfo (fs ++ [f]) -- f:fs
       return ()

getNextMsgNum :: CodeGenerator Int
getNextMsgNum = do
  DataSeg ds num <- getData
  putData (DataSeg ds (num + 1))
  return num

manageStack :: Int -> CodeGenerator ([Instr], [Instr])
manageStack offset =  if offset > 1024 then
  do
     let  newOffset = offset - 1024
     (saveStack, clearStack) <- manageStack newOffset
     return (SUB NF SP SP (ImmOp2 1024) : saveStack, ADD NF SP SP (ImmOp2 1024) : clearStack)
  else return ([SUB NF SP SP (ImmOp2 offset)], [ADD NF SP SP (ImmOp2 offset)])

addData' :: String -> CodeGenerator Int
addData' s = do
  DataSeg ds num <- getData
  if checkDataDefined s ds
    then return (getMsgNumData s ds)
    else do {msgNum <- getNextMsgNum; addData (MSG msgNum s); return msgNum }

getMsgNumData :: String -> [Data] -> Int
getMsgNumData s ds
  = head [ n | MSG n s' <- ds, s == s' ]

-- POST: Returns true iff message is not defined
checkDataDefined :: String -> [Data] -> Bool
checkDataDefined s msgs
  = or [ s == s' | MSG _ s' <- msgs ]

addData :: Data -> CodeGenerator ()
addData d = do
  DataSeg ds num <- getData
  putData (DataSeg (ds ++ [d]) num)

getData :: CodeGenerator DataSegment
getData  = lift get

putData :: DataSegment -> CodeGenerator ()
putData = lift . put

saveStackInfo :: CodeGenerator ()
saveStackInfo
  = lift (lift save)

restoreStackInfo :: CodeGenerator ()
restoreStackInfo
  = lift (lift restore)

-- Updates the next label number with the given number
updateNextLabelNum :: Int -> CodeGenerator ()
updateNextLabelNum
  = lift . lift . lift . put

-- Retrieves the next label number
getNextLabelNum :: CodeGenerator Int
getNextLabelNum
  = lift . lift . lift $ get

-- Returns the String "L:" appended with the next label num, and updates the label number
getNextLabel :: CodeGenerator String
getNextLabel = do
  labelNum <- getNextLabelNum
  updateNextLabelNum (labelNum + 1)
  return $ "L" ++ show labelNum

putStackInfo :: (Env, Int) -> CodeGenerator ()
putStackInfo
  = lift . lift . put

getStackInfo :: CodeGenerator (Env, Int)
getStackInfo
  = lift (lift get)

-- Increments the offset of the stack pointer (to the start of the scope)
incrementOffset :: Int -> CodeGenerator ()
incrementOffset n = do
  (env, offset) <- getStackInfo
  let newEnv = Map.map (+ n) env
  putStackInfo (newEnv, offset + n)

-- Decrements the offset of the stack pointer (to the start of the scope)
decrementOffset :: Int -> CodeGenerator ()
decrementOffset n = do
  (env, offset) <- getStackInfo
  let newEnv = Map.map (\x -> x - n) env
  putStackInfo (newEnv, offset - n)

-- Assert Ops are registers
-- Could avoid duplication.
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

{- ARM ASSEMBLY BOILERPLATE CODE -}

space, spaceX2, text, global, dataLabel, word, ascii :: String
spaceGen :: Int -> String
spaceGen n = concat $ replicate n space
space     = "    "
spaceX2   = space ++ space
text      = space ++ ".text"
global    = space ++ ".global main"
dataLabel = ".data"
word      = ".word"
ascii     = ".ascii"

{- SHOW INSTANCES -}

-- our lengths are going to be slightly off?
instance Show Data where
  show (MSG i s)
    = space ++ "msg_"  ++ show i   ++ ":\n" ++
      spaceGen 3 ++ ".word  " ++ show (length s) ++ "\n" ++
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
    = "SUB" ++ show fl ++ " " ++ show reg ++ ", " ++ show op' ++ ", " ++ show op2
  show (ADD fl reg op' op2)
    = "ADD" ++ show fl ++ " " ++ show reg ++ ", " ++ show op' ++ ", " ++ show op2
  show (EOR reg op' op'')
    = "EOR " ++ show reg ++ ", " ++ show op' ++ ", " ++ show op''
  show (RSBS reg op' op'')
    = "RSBS " ++ show reg ++ ", " ++ show op' ++ ", " ++ show op''
  show (SMULL r0 r1 r2 r3)
    = "SMULL " ++ show r0 ++ ", " ++ show r1 ++ ", " ++ show r2 ++ ", " ++ show r3
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

showSizeIndexingRegOps :: Size -> Indexing -> Reg -> [Op] -> String
showSizeIndexingRegOps s i reg ops
  = show s ++ " " ++ show reg ++ ", " ++ showIndexing i ops

showIndexing :: Indexing -> [Op] -> String
showIndexing index ops = case index of
    Pre   -> show ops ++ "!"
    Post  -> show (init ops) ++ ", " ++ show (last ops)
    NoIdx -> show ops

showSingleInstr :: Size -> Reg -> Op -> String
showSingleInstr s reg op'
  = show s ++ " " ++ show reg ++ ", " ++ showSingleOp op'

showSingleOp :: Op -> String
showSingleOp op = case op of
  RegOp _ -> "[" ++ show op ++ "]"
  _       -> show op

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
