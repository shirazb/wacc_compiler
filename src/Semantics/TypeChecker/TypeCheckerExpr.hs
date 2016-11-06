module Semantics.TypeChecker.TypeCheckerExpr where

import qualified Prelude
import Prelude hiding (LT, EQ)
import Control.Monad.Writer.Strict

import Utilities.Definitions
import ErrorMessages.Semantic
import Debug.Trace


-- data ArrayElem = ArrayElem Ident [Expr]             deriving (Eq, Show)

-- tests
testUnaryApp1  = UnaryApp Neg (BoolLit True)
testUnaryApp2  = UnaryApp Neg (IntLit 1)
testUnaryApp3  = UnaryApp Len identTest
testUnaryApp4  = UnaryApp Len (IntLit 1)
-- test len with array deref expr
testUnaryApp5  = UnaryApp Len identTest1
testUnaryApp6  = UnaryApp Chr (CharLit 'c')
testUnaryApp7  = UnaryApp Chr (IntLit 1)
testUnaryApp8  = UnaryApp Not (BoolLit True)
testUnaryApp9  = UnaryApp Not (IntLit (-1))
testBinaryMul1 = BinaryApp (Arith Mul) (IntLit 1) (IntLit 1)
testBinaryMul2 = BinaryApp (Arith Mul) (CharLit 'c') (CharLit 'd')
testBinaryMul3 = BinaryApp (Arith Mul) (IntLit 1) PairLiteral
testBinaryMul4 = BinaryApp (Arith Mul) testUnaryApp1 (IntLit 1)
testBinaryMul5 = BinaryApp (Arith Mul) (IntLit 1) testUnaryApp1
testBinaryMul6 = BinaryApp (Arith Mul) (CharLit 'e') (IntLit 1)
testBinaryMul7 = BinaryApp (Arith Mul) testUnaryApp2 testBinaryMul1
testBinaryAnd1 = BinaryApp (Logic AND) (BoolLit True) (BoolLit True)
testBinaryAnd2 = BinaryApp (Logic AND) (CharLit 'e') (CharLit 'd')
testBinaryOr3  = BinaryApp (Logic OR)  (BoolLit True) (CharLit 'e')
testBinaryOr4  = BinaryApp (Logic OR)  (CharLit 'd') (BoolLit False)
testBinaryOr5  = BinaryApp (Logic OR)  testBinaryAnd1 (BoolLit True)
testBinaryLT1  = BinaryApp (RelOp LT) (CharLit 'c') (CharLit 'd')
testBinaryLT2  = BinaryApp (RelOp LT) (IntLit 1)  (IntLit 1)
testBinaryLT3  = BinaryApp (RelOp LT) (CharLit 'c') (IntLit 1)
testBinaryLT4  = BinaryApp (RelOp LT) testUnaryApp1 (CharLit 'c')
testBinaryLT5  = BinaryApp (RelOp LT) (IntLit 1) testUnaryApp1
testBinaryEQ1  = BinaryApp (EquOp EQ) (IntLit 1) (IntLit 2)
testBinaryEQ2  = BinaryApp (EquOp EQ) (StringLit "dzs") (StringLit "das")
testBinaryEQ3  = BinaryApp (EquOp EQ) (CharLit 'c') (CharLit 'd')
testBinaryEQ4  = BinaryApp (EquOp EQ) (BoolLit True) (BoolLit False)
testBinaryEQ5  = BinaryApp (EquOp EQ) identTest identTest
testBinaryEQ6  = BinaryApp (EquOp EQ) identTest2 identTest
-- what does it mean to type-check arrays and pairs
-- do you check the inner types
-- or do you defer that to the run time???
identTest3     = IdentE (Ident "x" (Info (PairT (BaseT BaseInt) (BaseT BaseInt)) Variable))
identTest4     = IdentE (Ident "y" (Info (PairT (BaseT BaseInt) (BaseT BaseChar)) Variable))
identTest      = IdentE (Ident "x" (Info (ArrayT 1 (BaseT BaseInt)) Variable))
identTest1     = IdentE (Ident "x" (Info (BaseT BaseChar) Variable))
identTest2     = IdentE (Ident "x" (Info (ArrayT 1 (BaseT BaseChar)) Variable))
testBinaryEQ7  = BinaryApp (EquOp EQ) identTest3 identTest4
testBinaryEQ8  = BinaryApp (EquOp EQ) identTest identTest2

-- check derefernce on all expressions

type ErrorMsg      = String
type TypeChecker a = Writer [ErrorMsg] a

{- Utility -}

checkUnAppType :: Type -> Type -> Type -> TypeChecker Type
checkUnAppType expectedT actualT opReturnT
  = if actualT /= expectedT
      then do {
        tell [generateErrMsg expectedT actualT];
        return NoType
      } else return opReturnT

checkBinaryApp :: BinOp -> Type -> Type -> Type -> TypeChecker Type
checkBinaryApp op opT arg1T arg2T
  = if arg1T /= arg2T
      then tell [Error (BinaryOPErr op (MismatchArgs arg1T arg2T))] >>
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
                 then tell [Error (BinaryOpInvalidArgs op opT arg1T)] >>
                        return NoType
                 else return arg1T

typeCheckExpr :: Expr -> TypeChecker Type
typeCheckExpr (IntLit _)
  = return (BaseT BaseInt)
typeCheckExpr (BoolLit _)
  = return (BaseT BaseBool)
typeCheckExpr (CharLit _)
  = return (BaseT BaseChar)
typeCheckExpr (StringLit _)
  = return (BaseT BaseString)
typeCheckExpr PairLiteral
  = return Pair
typeCheckExpr (IdentE (Ident _ (Info t _)))
  = return t
typeCheckExpr (ExprArray (ArrayElem i indexes))
  = undefined
typeCheckExpr (UnaryApp Not expr) = do
  t <- typeCheckExpr expr
  checkUnAppType (BaseT BaseBool) t (BaseT BaseBool)
typeCheckExpr (UnaryApp Neg expr) = do
  t <- typeCheckExpr expr
  checkUnAppType (BaseT BaseInt) t (BaseT BaseInt)
typeCheckExpr (UnaryApp Len expr) = do
  t <- typeCheckExpr expr
  checkUnAppType PolyArray t (BaseT BaseInt)
typeCheckExpr (UnaryApp Ord expr) = do
  t <- typeCheckExpr expr
  checkUnAppType (BaseT BaseChar) t (BaseT BaseInt)
typeCheckExpr (UnaryApp Chr expr) = do
  t <- typeCheckExpr expr
  checkUnAppType (BaseT BaseInt) t (BaseT BaseChar)

typeCheckExpr (BinaryApp op@(Arith _) expr expr') = do
  t  <- typeCheckExpr expr
  t' <- typeCheckExpr expr'
  checkBinaryApp op (BaseT BaseInt) t t'
typeCheckExpr (BinaryApp op@(Logic _) expr expr') = do
  t  <- typeCheckExpr expr
  t' <- typeCheckExpr expr'
  checkBinaryApp op (BaseT BaseBool) t t'
typeCheckExpr (BinaryApp op@(RelOp _) expr expr') = do
  t  <- typeCheckExpr expr
  t' <- typeCheckExpr expr'
  case (t, t') of
    (NoType, _ ) -> return NoType --
    (_,  NoType) -> return NoType
    _             | t /= t' -> tell [Error (BinaryOPErr op (MismatchArgs t t'))] >> return NoType
                  | checkCharOrInt t || checkCharOrInt t' -> return (BaseT BaseBool)
                  | otherwise -> tell [Error (BinaryOpInvalidArgsRel t)] >> return NoType
typeCheckExpr (BinaryApp op@(EquOp _) expr expr') = do
  t <- typeCheckExpr expr
  t' <- typeCheckExpr expr'
  case (t, t') of
    (NoType, _) -> return NoType
    (_, NoType) -> return NoType
    _             | t /= t' -> tell [Error (BinaryOPErr op (MismatchArgs t t'))] >> return NoType
                  | otherwise -> return (BaseT BaseBool)



checkCharOrInt :: Type -> Bool
checkCharOrInt t = t == BaseT BaseChar || t == BaseT BaseInt
