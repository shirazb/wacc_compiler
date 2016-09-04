module Utility.Definitions where

{- WACC SYNTAX/DATA DEFINITIONS -}

type Ident     = String

data Program   = Program [Func] Stat                deriving (Show, Eq)
data Func      = Func Type Ident ParamList Stat     deriving (Show, Eq)
data ParamList = ParamList [Param]                  deriving (Show, Eq)
data Param     = Param Type Ident                   deriving (Show, Eq)
type ArrayType = Type
data ArrayElem = ArrayElem Ident [Expr]             deriving (Show, Eq)
data PairType  = PairType PairElemType PairElemType deriving (Show, Eq)

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

data AssignLHS
  = Var Ident
  | ArrayDeref ArrayElem
  | PairDeref PairElem
  deriving (Show, Eq)

data AssignRHS
  = ExprAssign      Expr
  | ArrayLitAssign  [Expr]
  | NewPairAssign   Expr Expr
  | PairElemAssign  PairElem
  | FuncCallAssign  Ident [Expr]
  deriving (Show, Eq)

data PairElem
  = First Expr
  | Second Expr
  deriving (Show, Eq)

data Type
  = BaseT BaseType
  | ArrayT ArrayType
  | PairT PairType
  deriving (Show, Eq)

data BaseType
  = BaseInt
  | BaseBool
  | BaseChar
  | BaseString
  deriving (Show, Eq)

data PairElemType
  = BaseP  BaseType
  | ArrayP ArrayType
  | Pair
  deriving (Show, Eq)

data Expr
  = StringLit String
  | CharLit Char
  | IntLit Int
  | BoolLit Bool
  | PairLiteral
  | IdentE Ident
  | ExprArray ArrayElem
  | UnaryApp UnOp Expr
  | BinaryApp BinOp Expr Expr
  deriving (Show, Eq)

data UnOp
  = Not
  | Neg
  | Len
  | Ord
  | Chr
  deriving (Show, Eq)

data BinOp
  = Mul
  | Div
  | Mod
  | Add
  | Sub
  | AND
  | OR
  | LT
  | LTE
  | EQ
  | GTE
  | GT
  | NEQ
  deriving (Show, Eq)

keywords = ["while", "if", "else"]

lowBinOps   =  [("+", Add), ("-", Sub) , (">=", GTE), (">", Utility.Definitions.GT),
               ("<=", LTE), ("<", Utility.Definitions.LT), ("==", Utility.Definitions.EQ),
               ("!=", NEQ), ("&&", AND), ("||", OR)]
highBinOps   = [("*", Mul)]
higherBinOps = [("/", Div), ("%", Div)]

unOpAssoc = [("!", Not), ("-", Neg), ("len", Len), ("ord", Ord), ("chr", Chr)]

baseTypes = [("int", BaseInt),("bool", BaseBool),
            ("char", BaseChar),("string", BaseString)]
