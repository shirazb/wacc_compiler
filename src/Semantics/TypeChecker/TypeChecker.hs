module Semantics.TypeChecker.TypeChecker where

import Control.Monad.Writer.Strict
import Utilities.Definitions

type TypeErrMsg = String

data ErrorClass = ErrorClass Position TypeError

data TypeError
  = Mismatch Type Type
  | InvalidArgs Type Type

-- data Mismatch
--   = DeclarationMismatch
--   |

-------------------------------------------------------------------------------
{- STATEMENTS -}

typeCheckStat :: Stat -> Writer [TypeErrMsg] ()
typeCheckStat (Declaration t ident rhs) = do
  typeRHS <- typeCheckRHS rhs
  when (typeRHS /= t) (tell ["mismatch"])

typeCheckStat (Assignment lhs rhs) = do
  typeLHS <- typeCheckLHS lhs

  -- typeCheckRHS will type check params in functions
  -- it will return the return type
  typeRHS <- typeCheckRHS rhs
  when (typeLHS /= typeRHS) (tell ["Type mismatch in declaration"])

typeCheckStat (Read lhs)
  = void $ typeCheckLHS lhs

typeCheckStat (Free expr@(IdentE _))
  = return ()

typeCheckStat (Free _)
  = tell ["Free called with invalid args"]

typeCheckStat (Exit (IntLit _))
  = return ()

typeCheckStat (Exit _)
  = tell ["Exit passed non integer arg"]

typeCheckStat (If cond s1 s2) = do
  expr <- typeCheckExpr cond
  when (expr /= BaseT BaseBool) (tell ["If condition not valid"])
  typeCheckStat s1
  typeCheckStat s2

typeCheckStat (While cond stat) = do
  expr <- typeCheckExpr cond
  when (expr /= BaseT BaseBool) (tell ["While condition not valid"])
  typeCheckStat stat

typeCheckStat (Return expr)
  = void $ typeCheckExpr expr

typeCheckStat (Print expr)
  = void $ typeCheckExpr expr

typeCheckStat (Println expr)
  = void $ typeCheckExpr expr

typeCheckStat (Block s)
  = typeCheckStat s

typeCheckStat (Seq s s') = do
  typeCheckStat s
  typeCheckStat s'

-------------------------------------------------------------------------------
{- ARRAYS -}

constructArray :: Int -> DataType -> Type
constructArray dim baseType
  = DataT (ArrayT (Array dim baseType))

getDimension :: Type -> Int
getDimension (DataT (ArrayT (Array n _)))
  = n
getDimension _
  = 0

-------------------------------------------------------------------------------
{- HELPER FUNCTIONS -}

checkSameType :: [Type] -> Bool
checkSameType es
  = and $ zipWith (==) es (tail es)

checkForTypeErrInList :: [Type] -> Bool
checkForTypeErrInList
  = any (== TypeErr)

typeCheckExpr :: Expr -> Writer [TypeErrMsg] Type
typeCheckExpr (IntLit _)
  = return $ DataT (BaseT BaseInt)
typeCheckExpr (ExprArray (ArrayElem ident es))
  = typeCheckArrayDeref ident es

-- Returns the type of an identifier
typeCheckIdent :: Ident -> Writer [TypeErrMsg] Type
typeCheckIdent (Ident _ (Info t _))
  = return t

-- checks type of each expression is an int, by delegating to helper that writes error msgs
-- check type of ident derefence
typeCheckArrayDeref :: Ident -> [Expr] -> Writer [TypeErrMsg] Type
typeCheckArrayDeref (Ident _ (Info (DataT (ArrayT a@(Array n t))) Variable)) es = do
  mapM_ (checkExprHasType (DataT (BaseT BaseInt))) es
  if n < length es
    then tell ["Cannot derefence; not an array."] >> return TypeErr
    else return $ DataT $ ArrayT a
typeCheckArrayDeref _ _
  = tell ["Cannot derefence; not an array."] >> return TypeErr

-- Gets the type of the innermost elements of an array. Returns TypeErr if ident

-- Checks an expression has Type Int
checkExprHasType :: Type -> Expr -> Writer [TypeErrMsg] ()
checkExprHasType t e = do
  eType <- typeCheckExpr e
  when (eType /= t)
      (tell ["type of array index must be int, actually: " ++ show t])
