{-
WACC SYNTAX/DATA DEFINITIONS. Definitions used to build Abstract Syntax Tree
(AST). AST is an internal represenation of the parsed contents of a given
input file. Module also defines show instances, which are primarily used to
print the internal represenation in a more human readable format to aid
debugging.
-}

-- TODO: WE SHOULD HAVE DIFFERENT DEFINITIONS FILES FOR DIFFERENT
--       COMPILATION STAGES

module Utilities.Def2 where

import Data.Char
import Data.List
import qualified Data.Map as Map
import Control.Monad.State.Strict

type Env       = Map.Map (String, Context) Info
type ArrayType = Type
type AST       = Program
type Position  = (Int, Int)
type Err       = (String, Position)

-- errrr, someone come up with a better name pls...
type LexicalScoper a = State SymbolTable a

data Program   = Program [Func] Stat                         deriving (Eq, Show)
data Func      = Func Type Ident ParamList Stat Position     deriving (Eq, Show)
data ParamList = ParamList [Param] Position                  deriving (Eq, Show)
data Param     = Param Type Ident Position                   deriving (Eq, Show)
data ArrayElem = ArrayElem Ident [Expr] Position             deriving (Eq, Show)
data PairType  = PairType PairElemType PairElemType Position deriving (Eq, Show)
data Ident     = Ident String Info                           deriving (Eq, Show)

data ErrorType
  = NoError
  | Duplicate
  | NotInScope
  deriving (Eq, Show)

data Info
  = Info {
    typeInfo  :: Type,
    context   :: Context
  }
  | ScopeError ErrorType
  | NoInfo
  deriving (Eq, Show)

data Context
  = Function
  | Variable
  deriving (Eq, Ord, Show)

data SymbolTable
  = ST SymbolTable Env
  | None
  deriving (Eq, Show)

data Stat
  = Skip Position
  | Declaration Type Ident AssignRHS Position
  | Assignment AssignLHS AssignRHS Position
  | Read AssignLHS Position
  | Free Expr Position
  | Return Expr Position
  | Exit Expr Position
  | Print Expr Position
  | Println Expr Position
  | If Expr Stat Stat Position
  | While Expr Stat Position
  | Block Stat Position
  | Seq Stat Stat Position
  deriving (Eq, Show)

data AssignLHS
  = Var Ident Position
  | ArrayDeref ArrayElem Position
  | PairDeref PairElem Position
  deriving (Eq, Show)

-- do we need to carry around types of assignRHS
data AssignRHS
  = ExprAssign Expr Position
  | ArrayLitAssign [Expr] Position
  | NewPairAssign  Expr Expr Position
  | PairElemAssign PairElem Position
  | FuncCallAssign Ident [Expr] Position
  deriving (Eq, Show)

data PairElem
  = PairElem PairElemSelector Expr Position
  deriving (Eq, Show)

data PairElemSelector
  = Fst
  | Snd
  deriving (Eq, Show)

data Type
  = BaseT BaseType Position
  | ArrayT ArrayType Position
  | PairT PairType Position
  deriving (Eq, Show)

data BaseType
  = BaseInt
  | BaseBool
  | BaseChar
  | BaseString
  deriving (Eq, Show)

data PairElemType
  = BaseP BaseType
  | ArrayP ArrayType
  | Pair
  deriving (Eq, Show)

data Expr
  = StringLit String Position
  | CharLit Char Position
  | IntLit Int Position
  | BoolLit Bool Position
  | PairLiteral Position
  | IdentE Ident Position
  | ExprArray ArrayElem Position
  | UnaryApp UnOp Expr Position
  | BinaryApp BinOp Expr Expr Position
  deriving (Eq, Show)

data UnOp
  = Not
  | Neg
  | Len
  | Ord
  | Chr
  deriving (Eq, Show)

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
  deriving (Eq, Show)

baseTypes       = [("int", BaseInt), ("bool", BaseBool),
                  ("char", BaseChar), ("string", BaseString)]

lowBinOps       = [("+", Add), ("-", Sub) , (">=", GTE),
                  (">", Utilities.Def2.GT), ("<=", LTE),
                  ("<", Utilities.Def2.LT), (
                  "==", Utilities.Def2.EQ), ("!=", NEQ),
                  ("&&", AND), ("||", OR)]
highBinOps      = [("*", Mul)]
higherBinOps    = [("/", Div), ("%", Mod)]

unOpAssoc       = [("len", Len), ("ord", Ord), ("chr", Chr)]
unOpAssocHigher = [("!", Not), ("-", Neg)]

{-
The following utility functions and show instances are used to print the AST
in a clear and readable format.
-}

-- {- UTILITY FUNCTIONS: -}
--
-- showAndIndent :: Show a => a -> String
-- showAndIndent
--   = indent . show
--
-- -- POST: Indents the given string by 4 spaces and returns the result as a
-- --       string
-- indent :: String -> String
-- indent s
--   = unlines indentedLines
--   where
--     indentedLines = map ("    " ++) (lines s)
--
-- -- POST:    Converts a list in to a string, and seperates the elements in the
-- --          list with a comma
-- -- EXAMPLE: listToString "[" [1,2,3] "]" will return "[1,2,3]"
-- listToString :: Show a => String -> [a] -> String -> String
-- listToString open xs close
--   = open ++ intercalate ", " (map show xs) ++ close
--
-- -- PRE:  (key, value) list is not empty and contains the association you are
-- --       looking for
-- -- POST: Performs a reverse lookup of a (key,value) list returning the key as
-- --       the result and taking the value as the input
-- flippedLookup :: Eq b => b -> [(a, b)] -> a
-- flippedLookup y xs
--   = head [ x | (x, y') <- xs, y == y' ]
--
-- -- POST: Generates a string represenation of a list of functions
-- showFuncs :: [Func] -> String
-- showFuncs = concatMap (flip (++) "\n" . show)
--
-- {- SHOW INSTANCES: -}
-- -- These are for the data definitions used to represent the AST
--
-- instance Show Program where
--   show (Program funcs body)
--     =  "begin\n" ++ indent(showFuncs funcs ++ showAndIndent body) ++ "end"
--
-- instance Show Func where
--   show (Func t ident params body)
--     = show t ++ "  " ++ show ident ++ show params ++ " is\n" ++
--       showAndIndent body ++  "end\n"
--
-- instance Show ParamList where
--   show (ParamList list)
--     = listToString "(" list ")"
--
-- instance Show Param where
--   show (Param t ident)
--     = show t ++ " " ++ show ident
--
-- instance Show UnOp where
--   show unOp
--     = flippedLookup unOp (unOpAssoc ++ unOpAssocHigher)
--
-- instance Show BinOp where
--   show binOp
--     = flippedLookup binOp (lowBinOps ++ highBinOps ++ higherBinOps)
--
-- instance Show ArrayElem where
--   show (ArrayElem ident elems)
--     = show ident ++ concatMap (\x ->  "[" ++ show x ++ "]") elems
--
-- instance Show PairType where
--   show (PairType pt1 pt2)
--     = "pair" ++ listToString "(" [pt1, pt2] ")"
--
-- instance Show Stat where
--   show Skip
--     = "skip"
--   show (Declaration typ ident rhs)
--    = show typ ++ " " ++ show ident ++ " = " ++ show rhs
--   show (Assignment lhs rhs)
--     = show lhs ++ " = " ++ show rhs
--   show (Read lhs)
--     = "read " ++ show lhs
--   show (Free expr)
--     = "free " ++ show expr
--   show (Return expr)
--     = "return " ++ show expr
--   show (Exit expr)
--     = "exit " ++ show expr
--   show (Print expr)
--     = "print " ++ show expr
--   show (Println expr)
--     = "println " ++ show expr
--   show (If cond stat stat')
--     = "if" ++ " (" ++ show cond ++ ") " ++ "then\n" ++ showAndIndent stat ++
--         "else\n" ++ showAndIndent stat' ++ "fi"
--   show (While cond body)
--     = "while (" ++ show cond ++ ") " ++ "do\n" ++ showAndIndent body ++ "done"
--   show (Block stat)
--     = "begin\n" ++ showAndIndent stat ++ "end"
--   show (Seq stat stat')
--     = show stat ++ ";\n" ++ show stat'
--
-- instance Show AssignLHS where
--   show (Var ident)
--     = show ident
--   show (ArrayDeref arrayElem)
--     = show arrayElem
--   show (PairDeref pairElem)
--     = show pairElem
--
-- instance Show AssignRHS where
--   show (ExprAssign e)
--     = show e
--   show (ArrayLitAssign elems)
--     = listToString "[" elems "]"
--   show (NewPairAssign e e')
--     = "newpair" ++ listToString "(" [e, e'] ")"
--   show (FuncCallAssign funcName params)
--     = "call " ++ show funcName ++ listToString "(" params ")"
--   show (PairElemAssign pair)
--     = show pair
--
-- instance Show PairElem where
--   show (PairElem selector expr)
--     = show selector ++ " " ++ show expr
--
-- instance Show PairElemSelector where
--   show Fst
--     = "fst"
--   show Snd
--     = "snd"
--
-- instance Show Type where
--   show (BaseT baseType)
--     = show baseType
--   show (ArrayT arrayType)
--     = show arrayType ++ "[]"
--   show (PairT pairType)
--     = show pairType
--
-- instance Show BaseType where
--   show BaseInt
--     = "int"
--   show BaseBool
--     = "bool"
--   show BaseChar
--     = "char"
--   show BaseString
--     = "string"
--
-- instance Show PairElemType where
--   show (BaseP baseType)
--     = show baseType
--   show (ArrayP arrayType)
--     = show arrayType ++ "[]"
--   show Pair
--     = "pair"
--
-- minPrecedence :: Int
-- minPrecedence
--   = 0
--
-- -- Currently using same relative precedences as Haskell
--
-- class Show a => ExpressionTerm a where
--   precedence :: a -> Int
--
-- instance ExpressionTerm Expr where
--   precedence (UnaryApp unOp _)
--     = precedence unOp
--   precedence (BinaryApp binOp _ _)
--     = precedence binOp
--   precedence _
--     = minPrecedence - 1
--
-- instance ExpressionTerm UnOp where
--   precedence Not
--     = 2
--   precedence Neg
--     = 2
--   precedence _
--     = 12
--
-- instance ExpressionTerm BinOp where
--   precedence Mul
--     = 4
--   precedence Div
--     = 4
--   precedence Mod
--     = 4
--   precedence Add
--     = 6
--   precedence Sub
--     = 6
--   precedence Utilities.Def2.LT
--     = 8
--   precedence LTE
--     = 8
--   precedence GTE
--     = 8
--   precedence Utilities.Def2.GT
--     = 8
--   precedence Utilities.Def2.EQ
--     = 10
--   precedence NEQ
--     = 10
--   precedence AND
--     = 12
--   precedence OR
--     = 12
--
-- inBrackets :: String -> String
-- inBrackets s
--   = "(" ++ s ++ ")"
--
-- {-
-- In our current use case, this takes an operator and an expression, then
-- adds brackets around the expression if its precedence is weaker than the
-- operator's. I have used the ExpressionTerm class and kept the type more
-- general than Operator -> Expr -> String because we may encounter more
-- complex cases later on. This is as opposed to having precedenceExpr,
-- precedenceUnOp, precedenceBinOp functions, and making the type of this
-- funciton more specific as discussed above.
-- -}
--
-- showWithoutBrackets :: (ExpressionTerm a, ExpressionTerm b) => a -> b -> String
-- showWithoutBrackets t t'
--   = let showT' = show t' in
--     if precedence t < precedence t'
--       then inBrackets showT'
--       else showT'
--
-- -- instance Show Ident where
-- --   show (Ident name _)
-- --     = name
--
-- instance Show Expr where
--   show (StringLit s)
--     = show s
--   show (CharLit c)
--     = "\'" ++ [c] ++ "\'"
--   show (IntLit x)
--     = show x
--   show (BoolLit b)
--     = map toLower (show b)
--   show PairLiteral
--     = "null"
--   show (IdentE ident)
--     = show ident
--   show (ExprArray arrayElem)
--     = show arrayElem
--
--   show (UnaryApp unOp expr)
--     = unOpString ++ showWithoutBrackets unOp expr
--     where
--       unOpString = show unOp ++ " "
--
--   show (BinaryApp binOp expr expr')
--     = showExpr ++ " " ++ show binOp ++ " " ++ showExpr'
--     where
--       showExpr  = showWithoutBrackets binOp expr
--       showExpr' = showWithoutBrackets binOp expr'
