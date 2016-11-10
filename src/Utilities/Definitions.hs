{-
WACC SYNTAX/DATA DEFINITIONS. Definitions used to build Abstract Syntax Tree
(AST). AST is an internal represenation of the parsed contents of a given
input file. Module also defines show instances, which are primarily used to
print the internal represenation in a more human readable format to aid
debugging.
-}

module Utilities.Definitions where

import qualified Prelude
import Prelude hiding (GT, LT, EQ)
import Data.Char
import Data.List
import qualified Data.Map as Map
import Control.Monad.State.Strict (State)
import Control.Monad.Writer.Strict (Writer)

type Env       = Map.Map (String, Context) Info
type AST       = Program
type Position  = (Int, Int)
type Err       = (String, Position)

-- Type Checking
type ErrorMsg      = String
type TypeChecker a = Writer [ErrorMsg] a

-- errrr, someone come up with a better name pls...
type LexicalScoper a = State SymbolTable a

data Program   = Program [Func] Stat                     deriving (Eq, Show)
data Func      = Func Type Ident ParamList Stat Position deriving (Eq, Show)
data ParamList = ParamList [Param] Position              deriving (Eq, Show)
data Param     = Param Type Ident Position               deriving (Eq, Show)
data ArrayElem = ArrayElem Ident [Expr] Position         deriving (Eq, Show)
data Ident     = Ident String Info                       deriving (Eq, Show)

type ScopeError = (String, ScopeErrorType, Position)

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
  deriving (Eq, Show)

data Type
  = BaseT BaseType
  | PairT Type Type   -- cannot be FuncT
  | ArrayT Int Type   -- can only be PairT or BaseT --should we enforce this
  | Pair
  | FuncT Type [Type] -- cannot be FuncT or Pair
  | NoType
  | PolyArray
  | PolyFunc
  | PolyPair
  | RelationalT
  | DataType
  deriving (Show)

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

{- Fundamental Types -}

baseTypes       = [("int", BaseInt), ("bool", BaseBool),
                   ("char", BaseChar), ("string", BaseString)]

{- Binary Operator Precedences -}

binOpPrec1, binOpPrec2, binOpPrec3 :: [(String, BinOp)]
binOpPrec4, binOpPrec5, binOpPrec6 :: [(String, BinOp)]

binOpPrec1      = [("/", Arith Div), ("%", Arith Mod), ("*", Arith Mul)]
binOpPrec2      = [("+", Arith Add), ("-", Arith Sub)]
binOpPrec3      = [(">=", RelOp GTE), (">",  RelOp GT),
                   ("<=", RelOp LTE), ("<",  RelOp LT)]
binOpPrec4      = [("==", EquOp EQ), ("!=", EquOp NEQ)]
binOpPrec5      = [("&&", Logic AND)]
binOpPrec6      = [("||", Logic OR)]

{- Unary Operator Precedences -}

unOpPrec1, unOpPrec2 :: [(String, UnOp)]

unOpPrec1 = [("!", Not), ("-", Neg)]
unOpPrec2 = [("len", Len), ("ord", Ord), ("chr", Chr)]

{- Escape Chars -}
escapeCharList = [('b','\b'), ('n','\n'), ('f','\f'),
                  ('r','\r'), ('t','\t'), ('\\','\\'),
                  ('\"','\"'), ('\'','\''), ('0', '\0')]

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
--     = flippedLookup unOp (unOpAssoc ++ unOpPrec1)
--
-- instance Show BinOp where
--   show binOp
--     = flippedLookup binOp (lowBinOps ++ highBinOps ++ higherBinOps)
--
-- instance Show ArrayElem where
--   show (ArrayElem ident elems)
--     = show ident ++ concatMap (\x ->  "[" ++ show x ++ "]") elems
--
-- -- instance Show PairType where
-- --   show (PairType pt1 pt2)
-- --     = "pair" ++ listToString "(" [pt1, pt2] ")"
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
-- -- instance Show PairElemSelector where
-- --   show Fst
-- --     = "fst"
-- --   show Snd
-- --     = "snd"
-- --
-- -- instance Show Type where
-- --   show (BaseT baseType)
-- --     = show baseType
-- --   show (ArrayT arrayType)
-- --     = show arrayType ++ "[]"
-- --   show (PairT pairType)
-- --     = show pairType
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
-- -- instance Show PairElemType where
-- --   show (BaseP baseType)
-- --     = show baseType
-- --   show (ArrayP arrayType)
-- --     = show arrayType ++ "[]"
-- --   show Pair
-- --     = "pair"
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
--   precedence Utilities.Definitions.LT
--     = 8
--   precedence LTE
--     = 8
--   precedence GTE
--     = 8
--   precedence Utilities.Definitions.GT
--     = 8
--   precedence Utilities.Definitions.EQ
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
