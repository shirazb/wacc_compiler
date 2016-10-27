{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- Exploratory work in building a typechecker, this is a prototype -}

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
-- import General U
import qualified Data.Map as Map

import Utilities.Definitions

-- Store the environment (variables)
type CheckerState
  = Map.Map String Expr

data Error
  = TypeError String
  | UndefinedVar String
  deriving Show

-- This is the monad that will execute the type checking
newtype Checker a
  = Checker { runChecker :: ExceptT Error (State CheckerState) a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadState CheckerState,
            MonadError Error)

runCheck :: Checker a -> Either Error a
runCheck e
  = (flip evalState initState) . runExceptT $ runChecker e
  where
    initState = Map.empty

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

testExpr1 = int 5
testExpr2 = bool True
testExpr3 = BinaryApp Add (testExpr1) (testExpr2)

main = do
  case runCheck (checkBT testExpr3) of
    Right _  -> putStrLn "Type check success"
    Left err -> putStrLn $ "Error: " ++ show err
