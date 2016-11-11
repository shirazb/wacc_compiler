{- This module type checks expressions -}

{-# LANGUAGE MultiWayIf #-}

module Semantics.TypeChecker.Expression where

import qualified Prelude
import Prelude hiding (LT, EQ)
import Control.Monad.Writer.Strict (tell, unless)

{- LOCAL IMPORTS -}
import Semantics.ErrorMessages
import Utilities.Definitions

-- POST: Type checks expressions, returns either a type which is the type of
--       the expression or returns NoType denoting that the expression failed
--       to type check
typeCheckExpr :: Expr -> TypeChecker Type

typeCheckExpr (IntLit _ _)
  = return (BaseT BaseInt)

typeCheckExpr (BoolLit _ _)
  = return (BaseT BaseBool)

typeCheckExpr (CharLit _ _)
  = return (BaseT BaseChar)

typeCheckExpr (StringLit _ _)
  = return (BaseT BaseString)

typeCheckExpr (PairLiteral _)
  = return PolyPair

typeCheckExpr (IdentE (Ident _ (Info t _)) _)
  = return t

typeCheckExpr (ExprArray ae@(ArrayElem i indexes _) _)
  = typeCheckArrayElem ae

typeCheckExpr expr@(UnaryApp Not expr' pos) = do
  t <- typeCheckExpr expr'
  checkUnAppType (BaseT BaseBool) t (BaseT BaseBool) pos expr

typeCheckExpr expr@(UnaryApp Neg expr' pos) = do
  t <- typeCheckExpr expr'
  checkUnAppType (BaseT BaseInt) t (BaseT BaseInt) pos expr

typeCheckExpr expr@(UnaryApp Len expr' pos) = do
  t <- typeCheckExpr expr'
  checkUnAppType PolyArray t (BaseT BaseInt) pos expr

typeCheckExpr expr@(UnaryApp Ord expr' pos) = do
  t <- typeCheckExpr expr'
  checkUnAppType (BaseT BaseChar) t (BaseT BaseInt) pos expr

typeCheckExpr expr@(UnaryApp Chr expr' pos) = do
  t <- typeCheckExpr expr'
  checkUnAppType (BaseT BaseInt) t (BaseT BaseChar) pos expr

typeCheckExpr binExpr@(BinaryApp op@(Arith _) expr expr' _) = do
  t  <- typeCheckExpr expr
  t' <- typeCheckExpr expr'
  checkBinaryApp op (BaseT BaseInt) t t' binExpr

typeCheckExpr binExpr@(BinaryApp op@(Logic _) expr expr' _) = do
  t  <- typeCheckExpr expr
  t' <- typeCheckExpr expr'
  checkBinaryApp op (BaseT BaseBool) t t' binExpr

typeCheckExpr binExpr@(BinaryApp op@(RelOp _) expr expr' pos) = do
  t  <- typeCheckExpr expr
  t' <- typeCheckExpr expr'
  case (t, t') of
    (NoType, _ ) -> return NoType
    (_,  NoType) -> return NoType
    _             | t /= t' -> tell [typeMismatch t t' pos binExpr]
                                 >> return NoType
                  | checkCharOrInt t -> return (BaseT BaseBool)
                  | otherwise -> tell [typeMismatch RelationalT t pos binExpr]
                                   >> return NoType

typeCheckExpr binExpr@(BinaryApp op@(EquOp _) expr expr' pos) = do
  t <- typeCheckExpr expr
  t' <- typeCheckExpr expr'
  case (t, t') of
    (NoType, _) -> return NoType
    (_, NoType) -> return NoType
    _             | t /= t' -> tell [typeMismatch t t' pos binExpr]
                                 >> return NoType
                  | otherwise -> return (BaseT BaseBool)

{- HELPER FUNCTIONS -}

-- POST: Checks if the argument provided to the Unary operator is of the
--       correct type. Logs an error message if they are not and returns
--       NoType, otherwise it returns the return type of the unary operator
checkUnAppType :: Show a => Type -> Type -> Type -> Position -> a
                              -> TypeChecker Type
checkUnAppType expectedT actualT opReturnT pos a
  = if actualT /= expectedT
      then do {
        tell [typeMismatch expectedT actualT pos a];
        return NoType
      } else return opReturnT

-- POST: Checks if the two arguments provided to the binary operator are of the
--       same and correct type. If they are then it returns the return type of
--       the binary opeartor, otherwise it returns NoType and it logs and error
--       message
checkBinaryApp :: BinOp -> Type -> Type -> Type -> Expr -> TypeChecker Type
checkBinaryApp op opT arg1T arg2T expr
 = if arg1T /= arg2T
     then tell [typeMismatch arg1T arg2T (getPosExpr expr) expr] >>
            return NoType;
     else evalArg
 where
   evalArg = case (arg1T, arg2T) of
                 (NoType, _) -> return NoType
                 (_, NoType) -> return NoType
                 _            -> if | opT /= arg1T -> tell [typeMismatch opT
                                        arg1T (getPosExpr expr) expr]
                                        >> return NoType
                                    | opT /= arg2T -> tell [typeMismatch opT
                                        arg2T (getPosExpr expr) expr]
                                        >> return NoType
                                    | otherwise -> return arg1T

-- POST: Returns true if the given type is of either type char or int
checkCharOrInt :: Type -> Bool
checkCharOrInt t
  = t == BaseT BaseChar || t == BaseT BaseInt

-- POST: Type checks the elemnets of an array. It returns the type of the array
--       element with the correct dimension if it  succeeds, otherwise it logs
--       the error message and returns NoType
-- NOTE: This function reports type errors but does not propagate NoType up
typeCheckArrayElem :: ArrayElem -> TypeChecker Type
typeCheckArrayElem arrElem@(ArrayElem ident indexes pos) = do
  identT <- typeCheckIdent ident
  mapM_ typeCheckIsInt indexes
-- Minimum array dimension required
  let numDerefs = length indexes
-- If ident is an array, check that it was not derefenced too many times
  if isValidArrayType identT
    then do
      let ArrayT dim innerT = getArrayType identT
--    Check dim accomodates the number of derefences
      if numDerefs > dim
--      If there are too many derefences, report an error
        then do
          tell [dimensionMismatch numDerefs dim pos arrElem]
          return NoType
--      Else return type of correct dimension
        else
          return $ constructType (dim - numDerefs) innerT
--  Else ident was not an array
    else do
      tell [typeMismatch PolyArray identT pos arrElem]
      return NoType


-- POST: Given a dimension and a type, constructs an array of that dimension
--       and type. 0-dimensional arrays are returned just as a type, not
--       wrapped in an ArrayT
constructType :: Int -> Type -> Type
constructType dim innerT
  | dim < 0    = error ("Assertion failed in Semantics.TypeChecker.AssignLHS."
                        ++ " ConstructType: negative dimension given.")
  | dim == 0   = innerT
  | otherwise  = ArrayT dim innerT

-- POST: Checks whether the input is a valid array type. If it is, it also
--       checks that its inner type is a PairT or BaseT
isValidArrayType :: Type -> Bool
isValidArrayType (ArrayT _ (PairT _ _))
  = True
isValidArrayType (BaseT BaseString)
  = True
isValidArrayType (ArrayT _ (BaseT _))
  = True
isValidArrayType _
  = False

-- POST: Returns the type of an identifier
typeCheckIdent :: Ident -> TypeChecker Type
typeCheckIdent (Ident _ info)
  = return (typeInfo info)

-- POST: Checks if the type of an expression is an int
typeCheckIsInt :: Expr -> TypeChecker ()
typeCheckIsInt e = do
  eType <- typeCheckExpr e
  unless (eType == BaseT BaseInt)
    (tell [typeMismatch (BaseT BaseInt) eType (getPosExpr e) e])

-- POST: Returns the positon of the given expression in the source file
getPosExpr :: Expr -> Position
getPosExpr e
  = case e of
      StringLit _     p -> p
      CharLit _       p -> p
      IntLit _        p -> p
      BoolLit _       p -> p
      PairLiteral     p -> p
      IdentE _        p -> p
      ExprArray _     p -> p
      UnaryApp _ _    p -> p
      BinaryApp _ _ _ p -> p

-- POST: Type checks the elements in a pair
typeCheckPairElem :: PairElem -> TypeChecker Type
typeCheckPairElem pair@(PairElem _ (PairLiteral _) pos)
  = tell [nullPtrDeref pos pair] >> return NoType
typeCheckPairElem pair@(PairElem selector expr pos) = do
  exprT <- typeCheckExpr expr
  if  | NoType       <- exprT  -> return NoType
      | PairT pt pt' <- exprT  -> case selector of
          Fst  -> return pt
          Snd  -> return pt'
      | otherwise -> do
          tell [typeMismatch PolyPair exprT pos pair]
          return NoType

-- POST: Returns the type of an array
-- NOTE: If the input is a StringLit, the function returns a 1 dimensional
--       array of chars
getArrayType :: Type -> Type
getArrayType a@ArrayT{}
  = a
getArrayType s@(BaseT BaseString)
  = ArrayT 1 (BaseT BaseChar)
getArrayType _
  = error "TypeChecker.Expression.getArrayType: calling on non-array type."
