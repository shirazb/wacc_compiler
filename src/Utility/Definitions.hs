module Utility.Definitions where

{- WACC SYNTAX/DATA DEFINITIONS. Definitions used to build Abstract Syntax Tree (AST).
   AST is an internal represenation of the parsed contents of a given input file.
   Module also defines show instances, which are primarily used to print the internal
   represenation in a more human readable format to aid debugging.  -}

import           Data.Char
import           Data.List
import           Debug.Trace

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

unOpAssoc = [("len", Len), ("ord", Ord), ("chr", Chr)]
unOpAssocHigher = [("!", Not), ("-", Neg)]


baseTypes = [("int", BaseInt),("bool", BaseBool),
            ("char", BaseChar),("string", BaseString)]


{-
Show instances and utility functions which are used to print the AST built by the parser in a more human readable format.
-}


{- Utility Functions -}
showAndIndent :: Show a => a -> String
showAndIndent
  = indent . show

-- PRE: None
-- POST: Indents the given string by 4 spaces and returns the result as a string.
indent :: String -> String
indent s
  = unlines indentedLines
  where
    indentedLines = map ("    " ++) (lines s)

-- PRE: None
-- POST: Converts a list in to a string, and seperates the elements in the list with a comma.
-- Example usage: listToString "[" [1,2,3] "]" will return "[1,2,3]"
listToString :: Show a => String -> [a] -> String -> String
listToString open xs close
  = open ++ intercalate ", " (map show xs) ++ close

-- PRE: (key, value) list is not empty and contains the association you are looking for.
-- POST: performs a reverse lookup of a (key,value) list returning the key as the result and taking the value as the input.
flippedLookup :: Eq b => b -> [(a, b)] -> a
flippedLookup y xs
  = head [ x | (x, y') <- xs, y == y' ]

-- PRE: None
-- POST: Generates a string represenation of a list of functions.
showFuncs :: [Func] -> String
showFuncs = concatMap (flip (++) "\n" . show)

{-
Show instances for the data definitions used to represent the AST.
-}

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

instance Show UnOp where
  show unOp
    = flippedLookup unOp unOpAssoc

instance Show BinOp where
  show binOp
    = flippedLookup binOp (lowBinOps ++ highBinOps ++ higherBinOps)

instance Show ArrayElem where
  show (ArrayElem name elems)
    = name ++ concatMap (\x ->  "[" ++ show x ++ "]") elems

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

minPrecedence :: Int
minPrecedence
  = 0

-- Currently using same relative precedences as Haskell
class Show a => ExpressionTerm a where
  precedence :: a -> Int

instance ExpressionTerm Expr where
  precedence (UnaryApp unOp _)
    = precedence unOp
  precedence (BinaryApp binOp _ _)
    = precedence binOp
  precedence _
    = minPrecedence - 1

instance ExpressionTerm UnOp where
  precedence Not
    = 2
  precedence Neg
    = 2
  precedence _
    = 12

instance ExpressionTerm BinOp where
  precedence Mul
    = 4
  precedence Div
    = 4
  precedence Mod
    = 4
  precedence Add
    = 6
  precedence Sub
    = 6
  precedence Utility.Definitions.LT
    = 8
  precedence LTE
    = 8
  precedence GTE
    = 8
  precedence Utility.Definitions.GT
    = 8
  precedence Utility.Definitions.EQ
    = 10
  precedence NEQ
    = 10
  precedence AND
    = 12
  precedence OR
    = 12

inBrackets :: String -> String
inBrackets s
  = "(" ++ s ++ ")"

-- In our current use case, this takes an operator and an expression, then
-- adds brackets around the expression if its precedence is weaker than the
-- operator's.
-- I have used the ExpressionTerm class and kept the type more general than Operator -> Expr -> String because
-- we may encounter more complex cases later on.
-- This is as opposed to having precedenceExpr, precedenceUnOp, precedenceBinOp
-- functions, and making the type of this funciton more specific as discussed
-- above.
showSecondWithoutRedundantBrackets :: (ExpressionTerm a, ExpressionTerm b) => a -> b -> String
showSecondWithoutRedundantBrackets t t'
  = let  showT' = show t' in
    if   precedence t < precedence t'
    then inBrackets showT'
    else showT'

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
    = unOpString ++ showSecondWithoutRedundantBrackets unOp expr
    where
      unOpString = show unOp ++ " "

  show (BinaryApp binOp expr expr')
    = showExpr ++ " " ++ show binOp ++ " " ++ showExpr'
    where
      showExpr         = showSecondWithoutRedundantBrackets binOp expr
      showExpr'        = showSecondWithoutRedundantBrackets binOp expr'
