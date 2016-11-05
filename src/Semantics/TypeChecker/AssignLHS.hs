module Semantics.TypeChecker.AssignLHS (
  typeCheckLHS
) where

import Control.Monad.Writer.Strict
import Utilities.Definitions

data TypeError
  = Mismatch Type Type

data Error
  = TypeError Type Type
  | NullPointerDeref

type TypeChecker a = Writer [Error] a

-- typeCheckLHS :: AssignLHS -> Writer [TypeErrMsg] Type
-- typeCheckLHS (Var ident)
--   = typeCheckIdent ident
-- typeCheckLHS (ArrayDeref (ArrayElem ident es))
--   = typeCheckArrayDeref ident es
-- -- COMMENT
-- -- MAKE SURE YOU READ THIS
-- -- I COULD BE DOING SOMETHING PRETTY WRONG
-- -- BUT FROM MY UNDERSTANDING
-- -- pair dereferences can only have identifiers
-- -- so any other thing should throw and Error
-- -- should we explicitly checkk for null
-- -- benefit is that we will have better messages
-- typeCheckLHS (PairDeref (PairElem pairElemSelector ident@IdentE{})) = do
--   -- this pattern match will fail if you have a type Error
--   DataT (PairT pt pt') <- typeCheckExpr ident
--   case pairElemSelector of
--     Fst -> return (DataT pt)
--     Snd -> return (DataT pt')
-- typeCheckLHS (PairDeref (PairElem _ PairLiteral))
--   = tell ["Trying to dereference a null"] >> return TypeErr
-- typeCheckLHS (PairDeref _)
--   = tell ["Trying to derefernce not a pair"] >> return TypeErr

typeCheckLHS :: AssignLHS -> TypeChecker Type
typeCheckLHS (Var ident)
  = typeCheckIdent ident
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
  let expectedType = ArrayT numDerefs ArrayPoly

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
checkValidArrayType = undefined

checkValidPairType = undefined

-- Returns the type of an ident
typeCheckIdent = undefined

-- type checks an expr
typeCheckExpr  = undefined

-- checkIsInt e <-> e is an int
typeCheckIsInt = undefined

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
