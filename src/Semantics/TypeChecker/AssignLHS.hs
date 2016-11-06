module Semantics.TypeChecker.AssignLHS (
  typeCheckLHS
) where

import Control.Monad.Writer.Strict
import Utilities.Definitions

data Error
  = TypeError Type Type
  | DataTypeOnly Type
  | NullPointerDeref
  deriving (Show, Eq)

type TypeChecker a = Writer [Error] a

typeCheckLHS :: AssignLHS -> TypeChecker Type
typeCheckLHS (Var ident) = do
  identT <- typeCheckIdent ident
  if identT == PolyFunc
    then tell [DataTypeOnly identT] >> return NoType
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
  mapM_ typeCheckIsInt indexes

  -- minimum array dimension required
  let numDerefs = length indexes
  let expectedType = ArrayT numDerefs PolyArray

  isValidArrayType <- checkValidArrayType identT

  -- if ident is an array, check it was not derefenced too many times
  if isValidArrayType
    then do
      -- Check dim accomodates the number of derefences
      let ArrayT dim innerT = identT
      if numDerefs > dim
        -- too many derefences; tell error
        then do
          tell [TypeError expectedType identT]
          return NoType

        -- return type of correct dimension
        else
          return $ constructType (numDerefs - dim) innerT

    -- ident was not an array
    else do
      tell [TypeError expectedType identT]
      return NoType


-- Given a dimension and a Type, constructs an array of that dimension and type.
-- 0-dimensional arrays are returned just as a type, not wrapped in an ArrayT
constructType :: Int -> Type -> Type
constructType dim innerT
  | dim < 0    = error "Assertion failed in Semantics.TypeChecker.AssignLHS. constructType: negative dimension given."
  | dim == 0   = innerT
  | otherwise  = ArrayT dim innerT

-- Checks is an array type, and if so, that its inner type is a PairT or BaseT
checkValidArrayType :: Type -> TypeChecker Bool
checkValidArrayType (ArrayT _ (PairT _ _))
  = return True
checkValidArrayType (ArrayT _ (BaseT _))
  = return True
checkValidArrayType t = do
  tell [TypeError PolyArray t]
  return False

checkValidPairType :: Type -> TypeChecker Bool
checkValidPairType t@FuncT{} = do
  tell [DataTypeOnly t]
  return False
checkValidPairType _
  = return True

-- Returns the type of an ident
typeCheckIdent :: Ident -> TypeChecker Type
typeCheckIdent (Ident _ info)
  = return (typeInfo info)

-- type checks an expr
typeCheckExpr  = undefined

-- checkIsInt e <-> e is an int
typeCheckIsInt :: Expr -> TypeChecker ()
typeCheckIsInt e = do
  eType <- typeCheckExpr e
  unless (eType == BaseT BaseInt)
    (tell [TypeError (BaseT BaseInt) eType])

typeCheckPairElem :: PairElem -> TypeChecker Type
typeCheckPairElem (PairElem _ PairLiteral)
  = tell [NullPointerDeref] >> return NoType
typeCheckPairElem (PairElem selector expr) = do
  exprT            <- typeCheckExpr expr
  isValidPairType  <- checkValidPairType exprT
  if isValidPairType
    then
      let PairT pt pt' = exprT in
      case selector of
        Fst  -> return pt
        Snd  -> return pt'
    else
      return NoType

-- Common test resources
x = Ident "x" (Info (PairT (BaseT BaseInt) Pair) Variable)
f = Ident "f" (Info (FuncT (BaseT BaseChar) [ArrayT 5 (BaseT BaseString)]) Function)
a = Ident "a" (Info (ArrayT 1 (PairT Pair (ArrayT 2 (BaseT BaseInt)))) Variable)
p = Ident "p" (Info (PairT (ArrayT 2 (BaseT BaseInt)) (BaseT BaseString)) Variable)
illegalP
  = Ident "illegalPair" (Info (PairT Pair (PairT (FuncT (BaseT BaseInt) [BaseT BaseChar]) Pair)) Variable)

-- Var tests
varX = Var x
funcVarFails = Var f

-- ArrayDeref tests
validArrayDeref = ArrayDeref (ArrayElem (Ident "a" (Info (ArrayT 3 (BaseT BaseInt)) Variable)) [BinaryApp (Arith Mul) (IntLit 1) (IntLit 2), UnaryApp Neg (IntLit (-3))])
derefFuncFails = ArrayDeref (ArrayElem f [IntLit 3])
tooManyDerefsFails = ArrayDeref (ArrayElem a [IntLit 2, IntLit 5])
derefToInnerTypeReturnsInnerType = ArrayDeref (ArrayElem a [IntLit 10])

-- PairDeref tests
validPairDeref = PairDeref (PairElem Fst (IdentE p))
nullPairDeref  = PairDeref (PairElem Snd PairLiteral)
malformedExprInPairDerefPropagates = PairDeref (PairElem Snd (UnaryApp Not (IntLit 3)))
nonPairExprFails = PairDeref (PairElem Snd (IdentE a))
illegalPairTypeFails = PairDeref (PairElem Snd (IdentE illegalP))

runTest lhs
  = runWriter $ typeCheckLHS lhs
