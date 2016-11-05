module Semantics.TypeChecker.TypeCheckerExpr where
import Control.Monad.Writer.Strict
import Utilities.Definitions
import Debug.Trace
-- data ArrayElem = ArrayElem Ident [Expr]             deriving (Eq, Show)

-- tests
testUnaryApp1  = UnaryApp Neg (BoolLit True)
testBinaryAdd1 = BinaryApp Mul (IntLit 1) (IntLit 1)
testBinaryAdd2 = BinaryApp Mul (CharLit 'c') (CharLit 'd')
testBinaryAdd3 = BinaryApp Mul (IntLit 1) PairLiteral
testBinaryAdd4 = BinaryApp Mul testUnaryApp1 (IntLit 1)
testBinaryAdd5 = BinaryApp Mul (IntLit 1) testUnaryApp1
testBinaryAdd6 = BinaryApp Mul (CharLit 'e') (IntLit 1)



data Error = Error TypeError deriving (Show)

type TypeChecker a = Writer [Error] a

data TypeError
  = Mismatch Type Type
  | InvalidArgs [Type] [Type]
  | OnlyIdentifier
  | BinaryOPErr BinOp TypeError
  | BinaryOpInvalidArgs Type Type
  | MismatchArgs Type Type
  deriving (Show)

{- Utility-}

checkType ::Type -> Type -> TypeChecker Type
checkType expectedT actualT
  = if actualT /= expectedT
      then do {
        traceM "We are in checkType";
        tell [Error (Mismatch expectedT actualT)];
        return NoType;
      } else return actualT

-- write error msgs
checkBinaryApp :: BinOp -> Type -> Type -> Type -> TypeChecker Type
checkBinaryApp op opT arg1T arg2T
  = if arg1T /= arg2T
      then tell [Error (BinaryOPErr op (InvalidArgs [arg1T] [arg2T]))] >> return NoType;
      else
        case arg1T of
          NoType -> return NoType
          _      -> case arg2T of
                       NoType -> return NoType
                       _       -> if opT /= arg1T || opT /= arg2T
                                    then return NoType
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
  checkType (BaseT BaseBool) t
typeCheckExpr (UnaryApp Neg expr) = do
  t <- typeCheckExpr expr
  checkType (BaseT BaseInt) t
typeCheckExpr (UnaryApp Len expr) = do
  t <- typeCheckExpr expr
  checkType PolyArray t
typeCheckExpr (UnaryApp Ord expr) = do
  t <- typeCheckExpr expr
  checkType (BaseT BaseChar) t
typeCheckExpr (UnaryApp Chr expr) = do
  t <- typeCheckExpr expr
  checkType (BaseT BaseInt) t
typeCheckExpr (BinaryApp Mul expr expr') = do
  t  <- typeCheckExpr expr
  t' <- typeCheckExpr expr'
  checkBinaryApp Mul (BaseT BaseInt) t t'
