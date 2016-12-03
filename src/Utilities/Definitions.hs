{- This module contains the data definitions for the WACC syntac. Definitions
used to build an Abstract Syntax Tree (AST). The AST is an internal
represenation of the parsed contents of a given input file. This module also
defines show instances, which are used to pretty-print the internal
represenation of the program -}

module Utilities.Definitions where

import qualified Prelude
import Prelude hiding (GT, LT, EQ)
import Data.Char
import Data.List
import qualified Data.Map as Map
import Control.Monad.State.Strict (State)
import Control.Monad.Writer.Strict (Writer)

type Env             = Map.Map (String, Context) Info
type AST             = Program
type Position        = (Int, Int)
type Err             = (String, Position)
type ErrorMsg        = String
type ArrayIndexes    = [Expr]
type TypeChecker a   = Writer [ErrorMsg] a
type ScopeAnalysis a = State SymbolTable a
type ScopeError      = (String, ScopeErrorType, Position)

data Program   = Program [Func] Stat                     deriving (Eq, Show)
data Func      = Func Type Ident ParamList Stat Position deriving (Eq, Show)
data ParamList = ParamList [Param] Position              deriving (Eq, Show)
data Param     = Param Type Ident Position               deriving (Eq, Show)
data ArrayElem = ArrayElem Ident ArrayIndexes Position   deriving (Eq, Show)
data Ident     = Ident String Info                       deriving (Eq, Show)

data ScopeErrorType
  = NoError
  | Duplicate
  | NotInScope
  deriving (Eq, Show)

data Info
  = Info {
    typeInfo :: Type,
    context  :: Context
  }
  | ScopeError ScopeErrorType
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
  | For Stat Expr Stat Stat Position -- ***
  | Block Stat Position
  | Seq Stat Stat Position
  deriving (Eq, Show)

data AssignLHS
  = Var Ident Position
  | ArrayDeref ArrayElem Position
  | PairDeref PairElem Position
  deriving (Eq, Show)

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

data BaseType
  = BaseInt
  | BaseBool
  | BaseChar
  | BaseString
  deriving (Eq)

instance Show BaseType where
  show BaseInt
    = "int"
  show BaseBool
    = "bool"
  show BaseChar
    = "char"
  show BaseString
    = "string"

data Type
  = BaseT BaseType
  | PairT Type Type
  | ArrayT Int Type
  | Pair
  | FuncT Type [Type]
  | NoType
  | PolyArray
  | PolyFunc
  | PolyPair
  | RelationalT
  | DataType
  deriving (Show)


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

data LogicalOp
  = AND
  | OR
  deriving (Eq, Show)

data RelationalOp
  = LT
  | LTE
  | GTE
  | GT
  deriving (Eq, Show)

data EqOps
 = EQ
 | NEQ
 deriving (Eq, Show)

data ArithOp
  = Mul
  | Div
  | Mod
  | Add
  | Sub
  deriving (Eq, Show)

data BinOp
  = Logic LogicalOp
  | Arith ArithOp
  | RelOp RelationalOp
  | EquOp EqOps
  deriving (Eq, Show)

{- FUNDAMENTAL TYPES -}

baseTypes = [("int", BaseInt), ("bool", BaseBool),
             ("char", BaseChar), ("string", BaseString)]

{- BINARY OPERATOR PRECEDENCES -}

binOpPrec1, binOpPrec2, binOpPrec3 :: [(String, BinOp)]
binOpPrec4, binOpPrec5, binOpPrec6 :: [(String, BinOp)]

binOpPrec1 = [("/", Arith Div), ("%", Arith Mod), ("*", Arith Mul)]
binOpPrec2 = [("+", Arith Add), ("-", Arith Sub)]
binOpPrec3 = [(">=", RelOp GTE), (">",  RelOp GT),
              ("<=", RelOp LTE), ("<",  RelOp LT)]
binOpPrec4 = [("==", EquOp EQ), ("!=", EquOp NEQ)]
binOpPrec5 = [("&&", Logic AND)]
binOpPrec6 = [("||", Logic OR)]

{- UNARY OPERATOR PRECEDENCES -}

unOpPrec1, unOpPrec2 :: [(String, UnOp)]

unOpPrec1 = [("!", Not), ("-", Neg)]
unOpPrec2 = [("len", Len), ("ord", Ord), ("chr", Chr)]

{- ESCAPE CHARS -}

escapeCharList = [('b','\b'), ('n','\n'), ('f','\f'),
                  ('r','\r'), ('t','\t'), ('\\','\\'),
                  ('\"','\"'), ('\'','\''), ('0', '\0')]

{- INT BOUNDS -}

minInt, maxInt :: Int
minInt = -2147483648
maxInt = 2147483647

{- EQUALITY INSTANCES -}

instance Eq Type where
  NoType == _             = True
  _      == NoType        = True
  PolyArray  == ArrayT _ _ = True
  ArrayT _ _ == PolyArray  = True
  PolyArray == BaseT BaseString   = True
  BaseT BaseString == PolyArray   = True
  BaseT  BaseString == ArrayT 1 (BaseT BaseChar)  = True
  ArrayT 1 (BaseT BaseChar) == (BaseT BaseString) = True
  _ == PolyArray          = False
  PolyArray == _          = False
  FuncT t ts == FuncT t' ts' = t == t' && ts == ts'
  ArrayT dim t == ArrayT dim' t' = t == t' && dim == dim'
  PairT t1 t2 == PairT t1' t2' = t1 == t1' && t2 == t2'
  BaseT t == BaseT t' = t == t'
  PolyFunc  == PolyFunc  = True
  PolyFunc  == FuncT _ _ = True
  FuncT _ _ == PolyFunc  = True
  PolyPair  == PairT _ _ = True
  PairT _ _ == PolyPair  = True
  PolyPair  == PolyPair  = True
  PolyPair  == Pair      = True
  Pair      == PolyPair  = True
  PairT _ _ == Pair      = True
  Pair      == PairT _ _ = True
  Pair      == Pair      = True
  _ == _ = False

-- {- HELPER FUNCTIONS -}
--
-- -- POST: Shows and indents a string
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
-- -- POST: Constructs a string representation of a dimension of an array
-- constructArray :: Int -> String
-- constructArray 0
--   = ""
-- constructArray 1
--   = "[]"
-- constructArray n
--   = constructArray (n - 1) ++ "[]"
--
-- -- POST: Constant which represents the minimum precedence level
-- minPrecedence :: Int
-- minPrecedence
--   = 0
--
-- -- POST: Returns a given string in brackets
-- inBrackets :: String -> String
-- inBrackets s
--   = "(" ++ s ++ ")"
--
-- -- POST: Removes superfluous brackets
-- showWithoutBrackets :: (ExpressionTerm a, ExpressionTerm b) => a -> b -> String
-- showWithoutBrackets t t'
--   = let showT' = show t' in
--       if precedence t < precedence t'
--         then inBrackets showT'
--         else showT'
--
-- {- SHOW INSTANCES -}
--
-- instance Show Program where
--   show (Program funcs body)
--     =  "begin\n" ++ indent(showFuncs funcs ++ showAndIndent body) ++ "end"
-- --
-- instance Show Func where
--   show (Func t ident params body _)
--     = show t ++ "  " ++ show ident ++ show params ++ " is\n" ++
--       showAndIndent body ++  "end\n"
--
-- instance Show ParamList where
--   show (ParamList list _)
--     = listToString "(" list ")"
--
-- instance Show Param where
--   show (Param t ident _)
--     = show t ++ " " ++ show ident
--
-- instance Show UnOp where
--   show unOp
--     = flippedLookup unOp (unOpPrec2 ++ unOpPrec1)
--
-- instance Show BinOp where
--   show binOp
--     = flippedLookup binOp (binOpPrec1 ++ binOpPrec2 ++ binOpPrec3
--                            ++ binOpPrec4 ++ binOpPrec5 ++ binOpPrec6)
--
-- instance Show ArrayElem where
--   show (ArrayElem ident elems _)
--     = show ident ++ concatMap (\x ->  "[" ++ show x ++ "]") elems
--
-- instance Show Ident where
--   show (Ident name _)
--     = name
--
-- instance Show Stat where
--   show (Skip _)
--     = "skip"
--   show (Declaration typ ident rhs _)
--    = show typ ++ " " ++ show ident ++ " = " ++ show rhs
--   show (Assignment lhs rhs _)
--     = show lhs ++ " = " ++ show rhs
--   show (Read lhs _)
--     = "read " ++ show lhs
--   show (Free expr _)
--     = "free " ++ show expr
--   show (Return expr _)
--     = "return " ++ show expr
--   show (Exit expr _)
--     = "exit " ++ show expr
--   show (Print expr _)
--     = "print " ++ show expr
--   show (Println expr _)
--     = "println " ++ show expr
--   show (If cond stat stat' _)
--     = "if" ++ " (" ++ show cond ++ ") " ++ "then\n" ++ showAndIndent stat ++
--         "else\n" ++ showAndIndent stat' ++ "fi"
--   show (While cond body _)
--     = "while (" ++ show cond ++ ") " ++ "do\n" ++ showAndIndent body ++ "done"
--   show (Block stat _)
--     = "begin\n" ++ showAndIndent stat ++ "end"
--   show (Seq stat stat' _)
--     = show stat ++ ";\n" ++ show stat'
-- --
-- instance Show AssignLHS where
--   show (Var ident _)
--     = show ident
--   show (ArrayDeref arrayElem _)
--     = show arrayElem
--   show (PairDeref pairElem _)
--     = show pairElem
--
-- instance Show AssignRHS where
--   show (ExprAssign e _)
--     = show e
--   show (ArrayLitAssign elems _)
--     = listToString "[" elems "]"
--   show (NewPairAssign e e' _)
--     = "newpair" ++ listToString "(" [e, e'] ")"
--   show (FuncCallAssign funcName params _)
--     = "call " ++ show funcName ++ listToString "(" params ")"
--   show (PairElemAssign pair _)
--     = show pair
--
-- instance Show PairElem where
--   show (PairElem selector expr _)
--     = show selector ++ " " ++ show expr
--
-- instance Show PairElemSelector where
--   show Fst
--     = "fst"
--   show Snd
--     = "snd"
--
-- instance Show ScopeErrorType where
--   show NoError
--     = "No Error with thsi variable"
--   show Duplicate
--     = "Duplicate"
--   show NotInScope
--     = "Not in scope"
--
-- instance Show Type where
--   show (BaseT baseType)
--      = show baseType
--   show (ArrayT dim arrayType)
--      = show arrayType ++ constructArray dim
--   show (PairT t t')
--      = "pair(" ++ show t ++ ", " ++ show t' ++ ")"
--   show Pair
--      = "null"
--   show (FuncT t paramTypes)
--     = "Return Type: " ++ show t ++ "\n"
--        ++ "Parameter Types: " ++ listToString "(" paramTypes ")"
--   show NoType
--     = "No Type"
--   show PolyArray
--    = "Array of any type"
--   show PolyFunc
--    = "Function of any type"
--   show PolyPair
--    = "Pair of any type"
--   show RelationalT
--    = "char or int"
--   show DataType
--    = "int or string or char or array or pair"
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
-- class Show a => ExpressionTerm a where
--   precedence :: a -> Int
--
-- instance ExpressionTerm Expr where
--   precedence (UnaryApp unOp _ _)
--     = precedence unOp
--   precedence (BinaryApp binOp _ _ _)
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
--   precedence (Arith Mul)
--     = 4
--   precedence (Arith Div)
--     = 4
--   precedence (Arith Mod)
--     = 4
--   precedence (Arith Add)
--     = 6
--   precedence (Arith Sub)
--     = 6
--   precedence (RelOp LT)
--     = 8
--   precedence (RelOp LTE)
--     = 8
--   precedence (RelOp GTE)
--     = 8
--   precedence (RelOp GT)
--     = 8
--   precedence (EquOp EQ)
--     = 10
--   precedence (EquOp NEQ)
--     = 10
--   precedence (Logic AND)
--     = 12
--   precedence (Logic OR)
--     = 12
--
-- instance Show Expr where
--   show (StringLit s _)
--     = show s
--   show (CharLit c _)
--     = "\'" ++ [c] ++ "\'"
--   show (IntLit x _)
--     = show x
--   show (BoolLit b _)
--     = map toLower (show b)
--   show (PairLiteral _)
--     = "null"
--   show (IdentE ident _)
--     = show ident
--   show (ExprArray arrayElem _)
--     = show arrayElem
--
--   show (UnaryApp unOp expr _)
--     = unOpString ++ showWithoutBrackets unOp expr
--     where
--       unOpString = show unOp ++ " "
--
--   show (BinaryApp binOp expr expr' _)
--     = showExpr ++ " " ++ show binOp ++ " " ++ showExpr'
--     where
--       showExpr  = showWithoutBrackets binOp expr
--       showExpr' = showWithoutBrackets binOp expr'
