{- This module provides the ARM Assembly data types, show instances and
boilerplate code for our code generation output -}

module CodeGen.Assembly where


import Control.Monad.StateStack
import qualified Data.Map as Map
import Control.Monad.Identity
import Control.Monad.State
import Debug.Trace
type VarMap = Map.Map String Int

-- what do we want as the type??
type Test a = StateStack Int a
type InstructionMonad a = StateStackT Int (StateT Int ((StateT VarMap Identity))) a

-- runStack :: (Num s, Num s1) => StateStackT s1 (StateT s (StateStackT (Map.Map k a1) Identity)) a -> (((a, s1), s), Map.Map k a1)
runStack p = runIdentity $ runStateT (runStateT (runStateStackT p 1) 2) Map.empty

getMap :: InstructionMonad String
getMap = do
  st <- get
  traceM "                              -------------------------                      "
  traceM $ show st
  st1 <- lift get
  traceM "                              -------------------------                      "
  traceM $ show st1
  traceM "                              -------------------------                      "
  st' <- lift (lift get)
  traceM $ show st'
  return "sd"

class CodeGen a where
  codegen :: a -> [Instr]

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
  | BL String
  | LDR Op Op
  | ADDS Op Op Op
  | SUBS Op Op Op
  | SMULL Op Op Op Op

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
