{-# LANGUAGE MultiWayIf #-}

module Semantics.TypeChecker.Expression where

import qualified Prelude
import Prelude hiding (LT, EQ)
import Control.Monad.Writer.Strict

import Utilities.Definitions
import Semantics.ErrorMessages

{- Utility -}
-- POST: Checks if the argument provided to the Unary operator
-- is of the correct type. Logs an error message if they are not, otherwise
-- it returns the return type of the unary operator.
checkUnAppType :: Show a => Type -> Type -> Type -> Position -> a
                              -> TypeChecker Type
checkUnAppType expectedT actualT opReturnT pos a
  = if actualT /= expectedT
      then do {
        tell [typeMismatch expectedT actualT pos a];
        return NoType
      } else return opReturnT

checkBinaryApp :: BinOp -> Type -> Type -> Type -> Expr -> TypeChecker Type
checkBinaryApp op opT arg1T arg2T expr
  = if arg1T /= arg2T
      then tell [typeMismatch arg1T arg2T (getPos expr) expr] >>
             return NoType;
      else evalArg
  where
    evalArg = case arg1T of
                 NoType -> return NoType
                 _      -> evalArg2
    evalArg2 = case arg2T of
                 NoType -> return NoType
                 _      -> eval
    eval     = if opT /= arg1T || opT /= arg2T
                 then tell [typeMismatch opT arg1T (getPos expr) expr] >>
                        return NoType
                 else return arg1T

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
    (NoType, _ ) -> return NoType --
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

checkCharOrInt :: Type -> Bool
checkCharOrInt t
  = t == BaseT BaseChar || t == BaseT BaseInt

typeCheckArrayElem :: ArrayElem -> TypeChecker Type
typeCheckArrayElem arrElem@(ArrayElem ident indexes pos) = do
  -- get ident's type
  identT <- typeCheckIdent ident

  -- check indexes are ints
  -- NB: Reports type errors, but does not propagate up NoType
  mapM_ typeCheckIsInt indexes

  -- minimum array dimension required
  let numDerefs = length indexes

  -- if ident is an array, check it was not derefenced too many times
  if isValidArrayType identT
    then do
      -- Pattern match on the array type
      let ArrayT dim innerT = getArrayType identT

        -- Check dim accomodates the number of derefences
      if numDerefs > dim
        -- too many derefences; tell error
        then do
          tell [dimensionMismatch numDerefs dim pos arrElem]
          return NoType

        -- return type of correct dimension
        else
          return $ constructType (dim - numDerefs) innerT

    -- ident was not an array
    else do
      tell [typeMismatch PolyArray identT pos arrElem]
      return NoType


-- Given a dimension and a Type, constructs an array of that dimension and type.
-- 0-dimensional arrays are returned just as a type, not wrapped in an ArrayT
constructType :: Int -> Type -> Type
constructType dim innerT
  | dim < 0    = error ("Assertion failed in Semantics.TypeChecker.AssignLHS." ++
                        " ConstructType: negative dimension given.")
  | dim == 0   = innerT
  | otherwise  = ArrayT dim innerT

-- Checks is an array type, and if so, that its inner type is a PairT or BaseT
isValidArrayType :: Type -> Bool
isValidArrayType (ArrayT _ (PairT _ _))
  = True
isValidArrayType (BaseT BaseString)
  = True
isValidArrayType (ArrayT _ (BaseT _))
  = True
isValidArrayType _
  = False

-- Returns the type of an ident
typeCheckIdent :: Ident -> TypeChecker Type
typeCheckIdent (Ident _ info)
  = return (typeInfo info)

-- checkIsInt e <-> e is an int
typeCheckIsInt :: Expr -> TypeChecker ()
typeCheckIsInt e = do
  eType <- typeCheckExpr e
  unless (eType == BaseT BaseInt)
    (tell [typeMismatch (BaseT BaseInt) eType (getPos e) e])

getPos :: Expr -> Position
getPos e
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

getArrayType :: Type -> Type
getArrayType a@ArrayT{}
  = a
getArrayType s@(BaseT BaseString)
  = ArrayT 1 (BaseT BaseChar)
getArrayType _
  = error "TypeChecker.Expression.getArrayType: calling on non-array type."
