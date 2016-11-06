{-# LANGUAGE MultiWayIf #-}

module Semantics.TypeChecker.AssignLHS (
  typeCheckLHS
) where

import Debug.Trace

import Control.Monad.Writer.Strict
import Semantics.TypeChecker.TypeCheckerExpr
import Utilities.Definitions

typeCheckLHS :: AssignLHS -> TypeChecker Type
typeCheckLHS (Var ident) = do
  identT <- typeCheckIdent ident
  if identT == PolyFunc
    then tell ["Error (DataTypeOnly identT)"] >> return NoType
    else return identT
typeCheckLHS (ArrayDeref arrayElem)
  = typeCheckArrayElem arrayElem
typeCheckLHS (PairDeref pairElem)
  = typeCheckPairElem pairElem

typeCheckArrayElem :: ArrayElem -> TypeChecker Type
typeCheckArrayElem (ArrayElem ident indexes) = do
  -- get ident's type
  identT <- typeCheckIdent ident

  -- check indexes are ints
  -- NB: Reports type errors, but does not propagate up NoType
  mapM_ typeCheckIsInt indexes

  -- minimum array dimension required
  let numDerefs = length indexes
  let expectedType = ArrayT numDerefs PolyArray

  -- if ident is an array, check it was not derefenced too many times
  if isValidArrayType identT
    then do
      -- Check dim accomodates the number of derefences
      let ArrayT dim innerT = identT
      if numDerefs > dim
        -- too many derefences; tell error
        then do
          tell ["Error (Mismatch expectedType identT)"]
          return NoType

        -- return type of correct dimension
        else
          return $ constructType (dim - numDerefs) innerT

    -- ident was not an array
    else do
      tell ["Error (Mismatch PolyArray identT)"]
      return NoType


-- Given a dimension and a Type, constructs an array of that dimension and type.
-- 0-dimensional arrays are returned just as a type, not wrapped in an ArrayT
constructType :: Int -> Type -> Type
constructType dim innerT
  | dim < 0    = error "Assertion failed in Semantics.TypeChecker.AssignLHS. constructType: negative dimension given."
  | dim == 0   = innerT
  | otherwise  = ArrayT dim innerT

-- Checks is an array type, and if so, that its inner type is a PairT or BaseT
isValidArrayType :: Type -> Bool
isValidArrayType (ArrayT _ (PairT _ _))
  = True
isValidArrayType (ArrayT _ (BaseT _))
  = True
isValidArrayType _
  = False

isValidPairType :: Type -> Bool
isValidPairType t@FuncT{}
  = False
isValidPairType _
  = True

-- Returns the type of an ident
typeCheckIdent :: Ident -> TypeChecker Type
typeCheckIdent (Ident _ info)
  = return (typeInfo info)

-- checkIsInt e <-> e is an int
typeCheckIsInt :: Expr -> TypeChecker ()
typeCheckIsInt e = do
  eType <- typeCheckExpr e
  unless (eType == BaseT BaseInt)
    (tell ["Error (Mismatch (BaseT BaseInt) eType)"])

typeCheckPairElem :: PairElem -> TypeChecker Type
typeCheckPairElem (PairElem _ PairLiteral)
  = tell ["Error NullPointerDeref"] >> return NoType
typeCheckPairElem (PairElem selector expr) = do
  exprT <- typeCheckExpr expr
  if  | NoType       <- exprT  -> return NoType
      | PairT pt pt' <- exprT  -> case selector of
        Fst  -> return pt
        Snd  -> return pt'
      | otherwise -> do
        tell ["Error (Mismatch PolyPair exprT)"]
        return NoType

-- Common test resources
x = Ident "x" (Info (PairT (BaseT BaseInt) Pair) Variable)
f = Ident "f" (Info (FuncT (BaseT BaseChar) [ArrayT 5 (BaseT BaseString)]) Function)
a = Ident "a" (Info (ArrayT 3 (PairT Pair (ArrayT 2 (BaseT BaseInt)))) Variable)
p = Ident "p" (Info (PairT (ArrayT 2 (BaseT BaseInt)) (BaseT BaseString)) Variable)

-- Var tests
varX = Var x
funcVarFails = Var f

-- ArrayDeref tests
validArrayDeref = ArrayDeref (ArrayElem a [BinaryApp (Arith Mul) (IntLit 1) (IntLit 2), UnaryApp Neg (IntLit (-3))])
derefFuncFails = ArrayDeref (ArrayElem f [IntLit 3])
tooManyDerefsFails = ArrayDeref (ArrayElem a [IntLit 2, IntLit 5, IntLit 2, IntLit 4])
derefToInnerTypeReturnsInnerType = ArrayDeref (ArrayElem a [IntLit 10, IntLit 2, IntLit 11])
propagatesTypeErrInIndex = ArrayDeref (ArrayElem a [UnaryApp Neg (BoolLit True)])

-- PairDeref tests
validPairDeref = PairDeref (PairElem Fst (IdentE p))
nullPairDeref  = PairDeref (PairElem Snd PairLiteral)
malformedExprInPairDerefPropagates = PairDeref (PairElem Snd (UnaryApp Not (IntLit 3)))
nonPairExprFails = PairDeref (PairElem Snd (IdentE a))

runTest
  = runWriter . typeCheckLHS
-- 
--
-- typeCheckConcat :: [Type] -> TypeChecker Type
-- typeCheckConcat (NoType : ts)
--   = return NoType
-- typeCheckConcat (t : ts) = do
--   result <- typeCheckConcat ts
