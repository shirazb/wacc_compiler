{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

import Utility.Definitions

import Control.Monad.State
import Control.Monad.Except
import Control.Monad
import qualified Data.Map as Map 

-- Store the environment (variables)
type CheckerState = Map.Map String Expr

data Error
  = TypeError String
  | UndefinedVar String
  deriving Show

-- This is our monad that will execute the type checking
newtype Checker a 
  = Checker { runChecker :: ExceptT Error (State CheckerState) a } 
  deriving (Functor, 
            Applicative,
            Monad,
            MonadState CheckerState,
            MonadError Error)

runCheck :: Checker a -> Either Error a
runCheck e = (flip evalState initState) . runExceptT $ runChecker e
           where initState = Map.empty

-- #TODO
-- checkAT :: Expr -> Checker ArrayType
-- checkAT  = null

-- #TODO
-- checkPT :: Expr -> Checker PairType
-- checkPT  = null

-- PRE:  None
-- POST: Checks BaseType
checkBT :: Expr -> Checker BaseType
checkBT (IntLit _)           = return BaseInt
checkBT (BoolLit _)          = return BaseBool
checkBT (CharLit _)          = return BaseChar
checkBT (StringLit _)        = return BaseString
checkBT (UnaryApp _ e)       = checkBT e
checkBT (BinaryApp _ e1 e2)  = do 
  t1 <- checkBT e1
  t2 <- checkBT e2
  if t1 == t2
    then return t1
    else throwError
           $ TypeError
           $ "BinaryApp expects arguments to have the same type, got: "
             ++ show t1 ++ ", " ++ show t2

-- These are just shortcut names for the testExprs below
int  = IntLit
bool = BoolLit
char = CharLit
str  = StringLit

testExpr1 = int 5     -- Passes
testExpr2 = bool True -- Passes

main = do
  case runCheck (checkBT testExpr1) of
    Right _  -> putStrLn "Type check success"
    Left err -> putStrLn $ "Error: " ++ show err
