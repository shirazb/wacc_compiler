module Utility.Definitions where

{- WACC SYNTAX/DATA DEFINITIONS -}

import Data.List

indent :: String -> String
indent string
  = unlines indentedLines
  where
    indentedLines = map (++ "    ") (lines string)

type Ident     = String
instance Show Program where
  show (Program funcs body)
    = show funcs ++ indent (show body)

instance Show Func where
  show (Func t name params body)
    = show t ++ "  " ++ show name ++ "(" ++ show params ++ ")" ++ "is\n" ++ indent (show body) ++ "end\n"

instance Show ParamList where
  show (ParamList list)
    = intercalate "," (map show list)

instance Show Param where
  show (Param t name)
    = show t ++ " " ++ show name

instance Show ArrayElem where
  show (ArrayElem name elems)
    = show name ++ show (concatMap (\x ->  "[" ++ show x ++ "]") elems)

instance Show PairType where
  show (PairType pt1 pt2)
    = "pair(" ++ show pt1 ++ ", " ++ show pt2 ++ ")"

instance Show Stat where
  show Skip
    = "skip"

  show (Declaration typ ident rhs)
   = show typ ++ " " ++ show ident ++ " = " ++ show rhs

  show (Assignment lhs rhs)
    = show lhs ++ " = " ++ show rhs

  show (Read lhs)
    = "read " ++ show lhs

  show (If cond stat1 stat2)
    = "if" ++ "(" ++ show cond ++ ")" ++ " then " ++ show stat1 ++ " else "
       ++ show stat2 ++ " fi "
  show (While cond body)
    = "while (" ++ show cond ++ ")" ++ "do\n" ++ indent (show body) ++ "end"

  



assoclist = [(Return, "return"), (Exit, "exit"), (Print, "print"), (Println, "println")]

data Program   = Program [Func] Stat                deriving (Eq)
data Func      = Func Type Ident ParamList Stat     deriving (Eq)
data ParamList = ParamList [Param]                  deriving (Eq)
data Param     = Param Type Ident                   deriving (Eq)
type ArrayType = Type
data ArrayElem = ArrayElem Ident [Expr]             deriving (Eq)
data PairType  = PairType PairElemType PairElemType deriving (Eq)

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
  deriving (Eq)

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

lowBinOps   =  [("+", Add), ("-", Sub) , (">=", GTE), (">", Utility.Definitions.GT),
               ("<=", LTE), ("<", Utility.Definitions.LT), ("==", Utility.Definitions.EQ),
               ("!=", NEQ), ("&&", AND), ("||", OR)]
highBinOps   = [("*", Mul)]
higherBinOps = [("/", Div), ("%", Div)]

unOpAssoc = [("!", Not), ("-", Neg), ("len", Len), ("ord", Ord), ("chr", Chr)]

baseTypes = [("int", BaseInt),("bool", BaseBool),
            ("char", BaseChar),("string", BaseString)]
