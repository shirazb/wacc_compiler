module Definitions where
import Control.Monad
import Control.Applicative

--- DATA DEFINITION FOR WACC HASKELL COMPILER PROJECT --
-- DEFINITIONS FOR PARSER
newtype Parser a = Parser {parse :: String -> [(a,String)]}

instance Functor Parser where
  fmap f p = p >>= \x -> return (f x)

instance Applicative Parser where
  pure v = Parser $ \inp -> [(v, inp)]
  Parser p1 <*> Parser p2 = Parser $ \inp -> [(f a, s'')| (f, s') <- p1 inp, (a, s'') <- p2 s']

instance Alternative Parser where
  empty = mzero
  p <|> q = Parser $ \s ->
     case parse p s of
       [] -> parse q s
       res -> res

instance Monad Parser where
  p >>= f  = Parser $ \s -> concatMap (\(a,s') -> parse (f a) s') $ parse p s
  return v = Parser $ \inp -> [(v, inp)]

instance MonadPlus Parser where
  mzero = Parser $ const []
  mplus p q = Parser $ \inp -> parse p inp ++ parse q inp

-- WACC SYNTAX -- ABSTRACT SYNTAX TREE
data Program = Program [Func] Stat deriving (Show, Eq)
data Ident   = Ident String deriving (Show, Eq)
data Func    = Func Type Ident ParamList Stat deriving (Show, Eq)
data ParamList = ParamList [Param] deriving (Show, Eq)
data Param   = Param Type Ident deriving (Show, Eq)
data Stat
  = Skip
  | Declaration Type Ident AssignRHS
  | Assignment AssignLHS AssignRHS
  | Read AssignLHS
  | Free Expr
  | Return Expr
  | Exit Expr
  | Print Expr
  | Println Expr
  | If Expr Stat Stat
  | While Expr Stat
  | Begin Stat
  | Seq Stat Stat
  deriving (Show, Eq)
data AssignLHS = Var Ident | ArrayDeref ArrayElem | PairDeref PairElem deriving (Show, Eq)
data AssignRHS
  = Expression Expr
  | NewArray ArrayLit
  | NewPair Expr Expr
  | PairElement PairElem
  | FuncCall Ident ArgList
  deriving (Show, Eq)
data ArgList = Args [Expr] deriving (Show, Eq)
data PairElem = First Expr | Second Expr deriving (Show, Eq)
data Type = BaseT BaseType | ArrayT ArrayType | PairT PairType deriving (Show, Eq)
data BaseType = BaseInt | BaseBool | BaseChar | BaseString deriving (Show, Eq)
data ArrayType = ArrayType Type deriving (Show, Eq)
data PairType = PairType PairElemType PairElemType deriving (Show, Eq)
data PairElemType = BaseP BaseType | BaseA ArrayType | Pair deriving (Show, Eq)
-- this is subject to change --
data Expr = StringLit String
            | CharLit Char
            | IntLit Int
            | BoolLit Bool
            | PairLiteral
            | ExprI Ident
            | ExprArray ArrayElem
            | UnaryOp UnOp Expr
            | BinaryOp BinOp Expr Expr
            | Expr Expr
            deriving (Show, Eq)

data ArrayLit = ArrayLit [Expr] deriving (Show, Eq)
data ArrayElem = ArrayElem Ident [Expr] deriving (Show, Eq)
-- data Expr    = LiteralExpr Literal | UnExpr UnOp Expr | BinExpr BinOp Expr Expr  deriving (Show, Eq)
-- data Literal = StringLit String | CharLit Char | IntLit Int | BoolLit Bool | ArrayLit [Expr]  deriving (Show, Eq)
data UnOp    = Exclatation | Neg | Len | Ord | Chr  deriving (Show, Eq)
data BinOp   = Mul | Div | Mod | Add | Sub | AND | OR | LT | LTE | EQ | GTE | GT  deriving (Show, Eq)
-- data ArOp    = Mul | Div | Mod | Add | Sub          deriving (Show, Eq) -- too much factoring
-- data BoolOp  = AND | OR | LT | LTE | EQ | GTE | GT  deriving (Show, Eq) -- too much factoring
