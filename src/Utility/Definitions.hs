module Utility.Definitions where

{- WACC SYNTAX/DATA DEFINITIONS -}


import Data.Char
import Data.List

showAndIndent :: Show a => a -> String
showAndIndent
  = indent . show

indent :: String -> String
indent s
  = unlines indentedLines
  where
    indentedLines = map ("    " ++) (lines s)

listToString :: Show a => String -> [a] -> String -> String
listToString open xs close
  = open ++ intercalate ", " (map show xs) ++ close

flippedLookup :: Eq b => b -> [(a, b)] -> a
flippedLookup y xs
  = head [ x | (x, y') <- xs, y == y' ]

showFuncs :: [Func] -> String
showFuncs = concatMap (flip (++) "\n" . show)

instance Show Program where
  show (Program funcs body)
    =  "begin\n" ++ indent(showFuncs funcs ++ showAndIndent body) ++ "end"

instance Show Func where
  show (Func t name params body)
    = show t ++ "  " ++ name ++ show params ++ " is\n" ++ showAndIndent body ++ "end\n"

instance Show ParamList where
  show (ParamList list)
    = listToString "(" list ")"

instance Show Param where
  show (Param t name)
    = show t ++ " " ++ name


instance Show ArrayElem where
  show (ArrayElem name elems)
    = show name ++ show (concatMap (\x ->  "[" ++ show x ++ "]") elems)

instance Show PairType where
  show (PairType pt1 pt2)
    = "pair" ++ listToString "(" [pt1, pt2] ")"

instance Show Stat where
  show Skip
    = "skip"
  show (Declaration typ ident rhs)
   = show typ ++ " " ++ ident ++ " = " ++ show rhs
  show (Assignment lhs rhs)
    = show lhs ++ " = " ++ show rhs
  show (Read lhs)
    = "read " ++ show lhs
  show (Free expr)
    = "free " ++ show expr
  show (Return expr)
    = "return " ++ show expr
  show (Exit expr)
    = "exit " ++ show expr
  show (Print expr)
    = "print " ++ show expr
  show (Println expr)
    = "println " ++ show expr
  show (If cond stat stat')
    = "if" ++ " (" ++ show cond ++ ") " ++ "then\n" ++ showAndIndent stat ++ "else\n"
       ++ showAndIndent stat' ++ "fi"
  show (While cond body)
    = "while (" ++ show cond ++ ") " ++ "do\n" ++ showAndIndent body ++ "done"
  show (Block stat)
    = "begin\n" ++ showAndIndent stat ++ "end"
  show (Seq stat stat')
    = show stat ++ ";\n" ++ show stat'

functionStrings
  = [(Return, "return"), (Exit, "exit"), (Print, "print"), (Println, "println")]

instance Show AssignLHS where
  show (Var ident)
    = ident
  show (ArrayDeref arrayElem)
    = show arrayElem
  show (PairDeref pairElem)
    = show pairElem

instance Show AssignRHS where
  show (ExprAssign e)
    = show e
  show (ArrayLitAssign elems)
    = listToString "[" elems "]"
  show (NewPairAssign e e')
    = "newpair" ++ listToString "(" [e, e'] ")"
  show (FuncCallAssign funcName params)
    = "call " ++ funcName ++ listToString "(" params ")"
  show (PairElemAssign pair)
    = show pair

instance Show PairElem where
  show (Fst e)
    = "fst " ++ show e
  show (Snd e)
    = "snd " ++ show e

instance Show Type where
  show (BaseT baseType)
    = show baseType
  show (ArrayT arrayType)
    = show arrayType
  show (PairT pairType)
    = show pairType

instance Show BaseType where
  show BaseInt
    = "int"
  show BaseBool
    = "bool"
  show BaseChar
    = "char"
  show BaseString
    = "string"

instance Show PairElemType where
  show (BaseP baseType)
    = show baseType
  show (ArrayP arrayType)
    = show arrayType
  show Pair
    = "pair"

instance Show Expr where
  show (StringLit s)
    = show s
  show (CharLit c)
    = "\'" ++ [c] ++ "\'"
  show (IntLit x)
    = show x
  show (BoolLit b)
    = map toLower (show b)
  show PairLiteral
    = "null"
  show (IdentE ident)
    = ident
  show (ExprArray arrayElem)
    = show arrayElem
  show (UnaryApp unOp expr)
    = "(" ++ show unOp ++ " " ++ show expr ++ ")"
  show (BinaryApp binOp expr expr')
    = "(" ++ show expr ++ " " ++ show binOp ++ " " ++ show expr' ++ ")"

instance Show UnOp where
  show unOp
    = flippedLookup unOp unOpAssoc

instance Show BinOp where
  show binOp
    = flippedLookup binOp (lowBinOps ++ highBinOps ++ higherBinOps)

type Ident     = String
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
  | Block Stat
  | Seq Stat Stat
  deriving (Eq)

data AssignLHS
  = Var Ident
  | ArrayDeref ArrayElem
  | PairDeref PairElem
  deriving (Eq)

data AssignRHS
  = ExprAssign      Expr
  | ArrayLitAssign  [Expr]
  | NewPairAssign   Expr Expr
  | PairElemAssign  PairElem
  | FuncCallAssign  Ident [Expr]
  deriving (Eq)

data PairElem
  = Fst Expr
  | Snd Expr
  deriving (Eq)

data Type
  = BaseT BaseType
  | ArrayT ArrayType
  | PairT PairType
  deriving (Eq)

data BaseType
  = BaseInt
  | BaseBool
  | BaseChar
  | BaseString
  deriving (Eq)

data PairElemType
  = BaseP  BaseType
  | ArrayP ArrayType
  | Pair
  deriving (Eq)

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
  deriving (Eq)

data UnOp
  = Not
  | Neg
  | Len
  | Ord
  | Chr
  deriving (Eq)

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
  deriving (Eq)

lowBinOps   =  [("+", Add), ("-", Sub) , (">=", GTE), (">", Utility.Definitions.GT),
               ("<=", LTE), ("<", Utility.Definitions.LT), ("==", Utility.Definitions.EQ),
               ("!=", NEQ), ("&&", AND), ("||", OR)]
highBinOps   = [("*", Mul)]
higherBinOps = [("/", Div), ("%", Mod)]

unOpAssoc = [("!", Not), ("-", Neg), ("len", Len), ("ord", Ord), ("chr", Chr)]

baseTypes = [("int", BaseInt),("bool", BaseBool),
            ("char", BaseChar),("string", BaseString)]
