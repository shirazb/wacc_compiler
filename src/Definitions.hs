module Definitions where

--- DATA DEFINITION FOR WACC HASKELL COMPILER PROJECT --

-- WACC SYNTAX -- ABSTRACT SYNTAX TREE
data Program = Program [Func] Stat deriving (Show, Eq)
type Ident = String
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
data Expr = StringLit String
            | CharLit Char
            | IntLit Int
            | BoolLit Bool
            | PairLiteral
            | ExprI Ident
            | ExprArray ArrayElem
            | UnaryApp UnOp Expr
            | BinaryApp BinOp Expr Expr
            deriving (Show, Eq)

data ArrayLit = ArrayLit [Expr] deriving (Show, Eq)
data ArrayElem = ArrayElem Ident [Expr] deriving (Show, Eq)
data UnOp    = Not | Neg | Len | Ord | Chr  deriving (Show, Eq)
data BinOp   = Mul | Div | Mod | Add | Sub | AND | OR | LT | LTE | EQ | GTE | GT | NEQ  deriving (Show, Eq)

