{-# LANGUAGE MultiWayIf #-}

module Semantics.TypeChecker.Expression where

import qualified Prelude
import Prelude hiding (LT, EQ)
import Control.Monad.Writer.Strict

import Utilities.Definitions
import Semantics.ErrorMessages
import Debug.Trace

{- TEST CASES -}

-- testUnaryApp1  = UnaryApp Neg (BoolLit True)
-- testUnaryApp2  = UnaryApp Neg (IntLit 1)
-- testUnaryApp3  = UnaryApp Len identTest
-- testUnaryApp4  = UnaryApp Len (IntLit 1)
-- -- Test len with array deref expr
-- testUnaryApp5  = UnaryApp Len identTest1
-- testUnaryApp6  = UnaryApp Chr (CharLit 'c')
-- testUnaryApp7  = UnaryApp Chr (IntLit 1)
-- testUnaryApp8  = UnaryApp Not (BoolLit True)
-- testUnaryApp9  = UnaryApp Not (IntLit (-1))
-- testBinaryMul1 = BinaryApp (Arith Mul) (IntLit 1) (IntLit 1)
-- testBinaryMul2 = BinaryApp (Arith Mul) (CharLit 'c') (CharLit 'd')
-- testBinaryMul3 = BinaryApp (Arith Mul) (IntLit 1) PairLiteral
-- testBinaryMul4 = BinaryApp (Arith Mul) testUnaryApp1 (IntLit 1)
-- testBinaryMul5 = BinaryApp (Arith Mul) (IntLit 1) testUnaryApp1
-- testBinaryMul6 = BinaryApp (Arith Mul) (CharLit 'e') (IntLit 1)
-- testBinaryMul7 = BinaryApp (Arith Mul) testUnaryApp2 testBinaryMul1
-- testBinaryAnd1 = BinaryApp (Logic AND) (BoolLit True) (BoolLit True)
-- testBinaryAnd2 = BinaryApp (Logic AND) (CharLit 'e') (CharLit 'd')
-- testBinaryOr3  = BinaryApp (Logic OR)  (BoolLit True) (CharLit 'e')
-- testBinaryOr4  = BinaryApp (Logic OR)  (CharLit 'd') (BoolLit False)
-- testBinaryOr5  = BinaryApp (Logic OR)  testBinaryAnd1 (BoolLit True)
-- testBinaryLT1  = BinaryApp (RelOp LT) (CharLit 'c') (CharLit 'd')
-- testBinaryLT2  = BinaryApp (RelOp LT) (IntLit 1)  (IntLit 1)
-- testBinaryLT3  = BinaryApp (RelOp LT) (CharLit 'c') (IntLit 1)
-- testBinaryLT4  = BinaryApp (RelOp LT) testUnaryApp1 (CharLit 'c')
-- testBinaryLT5  = BinaryApp (RelOp LT) (IntLit 1) testUnaryApp1
-- testBinaryEQ1  = BinaryApp (EquOp EQ) (IntLit 1) (IntLit 2)
-- testBinaryEQ2  = BinaryApp (EquOp EQ) (StringLit "dzs") (StringLit "das")
-- testBinaryEQ3  = BinaryApp (EquOp EQ) (CharLit 'c') (CharLit 'd')
-- testBinaryEQ4  = BinaryApp (EquOp EQ) (BoolLit True) (BoolLit False)
-- testBinaryEQ5  = BinaryApp (EquOp EQ) identTest identTest
-- testBinaryEQ6  = BinaryApp (EquOp EQ) identTest2 identTest
-- -- what does it mean to type-check arrays and pairs
-- -- do you check the inner types
-- -- or do you defer that to the run time?
--
-- identTest      = IdentE (Ident "x" (Info (ArrayT 1 (BaseT BaseInt)) Variable))
-- identTest1     = IdentE (Ident "x" (Info (BaseT BaseChar) Variable))
-- identTEST1'    = Ident "x" (Info (BaseT BaseChar) Variable)
-- identTest2     = IdentE (Ident "x" (Info (ArrayT 1 (BaseT BaseChar)) Variable))
-- identTEST2'    = Ident "x" (Info (ArrayT 1 (BaseT BaseChar)) Variable)
-- identTEST3'    = Ident "x" (Info (ArrayT 1 (BaseT BaseInt)) Variable)
-- identTEST4'    = Ident "z" (Info (ArrayT 2 (BaseT BaseInt)) Variable)
-- identTest3     = IdentE (Ident "x" (Info (PairT (BaseT BaseInt) (BaseT BaseInt)) Variable))
-- identTest4     = IdentE (Ident "y" (Info (ArrayT 2 (BaseT BaseInt)) Variable))
-- identTest5     = IdentE (Ident "x" (Info (ArrayT 1 (BaseT BaseInt)) Variable))
-- testBinaryEQ7  = BinaryApp (EquOp EQ) identTest3 identTest4
-- testBinaryEQ8  = BinaryApp (EquOp EQ) identTest identTest2
-- -- testing equality with diff dimension arrays
-- -- testing when deref is to much
-- -- testing when the types of the array differ
-- -- testing when its actually okay
-- -- testing when derefering equals a literal equality with another literal
-- testBinaryEQ9  = BinaryApp (EquOp EQ) (ExprArray (ArrayElem identTEST1' [IntLit 1])) (ExprArray (ArrayElem identTEST2' [IntLit 2]))
-- testBinaryEQ10 = BinaryApp (EquOp EQ) (ExprArray (ArrayElem identTEST3'[IntLit 1, IntLit 1])) (ExprArray (ArrayElem identTEST2'[IntLit 1]))
-- testBinaryEQ11 = BinaryApp (EquOp EQ) (ExprArray (ArrayElem identTEST4' [IntLit 1, IntLit 1])) (ExprArray (ArrayElem identTEST1' [IntLit 1]))
-- testBinaryEQ12 = BinaryApp (EquOp EQ) (ExprArray (ArrayElem identTEST2' [IntLit 1])) (ExprArray (ArrayElem identTEST3' [IntLit 1]))
-- testBinaryEq13 = BinaryApp (EquOp EQ) (ExprArray (ArrayElem identTEST3' [IntLit 1])) (ExprArray (ArrayElem identTEST3' [IntLit 1]))

{- Utility -}

checkUnAppType :: Show a => Type -> Type -> Type -> Position -> a -> TypeChecker Type
checkUnAppType expectedT actualT opReturnT pos a
  = if actualT /= expectedT
      then do {
        tell [typeMismatch expectedT actualT pos a];
        return NoType
      } else return opReturnT

checkBinaryApp :: BinOp -> Type -> Type -> Type -> Expr -> TypeChecker Type
checkBinaryApp op opT arg1T arg2T expr
  = if arg1T /= arg2T
      then tell [""] >>
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
    _             | t /= t' -> tell [typeMismatch t t' pos binExpr] >> return NoType
                  | checkCharOrInt t -> return (BaseT BaseBool)
                  | otherwise -> tell [typeMismatch RelationalT t pos binExpr] >> return NoType
typeCheckExpr binExpr@(BinaryApp op@(EquOp _) expr expr' pos) = do
  t <- typeCheckExpr expr
  t' <- typeCheckExpr expr'
  case (t, t') of
    (NoType, _) -> return NoType
    (_, NoType) -> return NoType
    _             | t /= t' -> tell [typeMismatch t t' pos binExpr] >> return NoType
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
  | dim < 0    = error "Assertion failed in Semantics.TypeChecker.AssignLHS. constructType: negative dimension given."
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
