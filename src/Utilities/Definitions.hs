{- This module contains the data definitions for the WACC syntac. Definitions
used to build an Abstract Syntax Tree (AST). The AST is an internal
represenation of the parsed contents of a given input file. This module also
defines pretty instances, which are used to pretty-print the internal
represenation of the program -}

module Utilities.Definitions where

import qualified Prelude
import Prelude hiding (GT, LT, EQ)
import Data.Char
import Data.List
import qualified Data.Map as Map
import Control.Monad.State.Strict (State (..), StateT (..))
import Control.Monad.Writer.Strict (Writer)

type AST                 = Program
type Err                 = (String, Position)
type ErrorMsg            = String
type Position            = (Int, Int)
type ScopeError          = (String, ScopeErrorType, Position)
type ArrayIndexes        = [Expr]
type TypeChecker a       = Writer [ErrorMsg] a
type ArithmeticErrors    = [String]
type ConstantEvaluator a = Writer ArithmeticErrors a

data Program   = Program [Class] [Func] Stat             deriving (Eq, Show)
data Func      = Func Type Ident ParamList Stat Position deriving (Eq, Show)
data ParamList = ParamList [Param] Position              deriving (Eq, Show)
data Param     = Param Type Ident Position               deriving (Eq, Show)
data ArrayElem = ArrayElem Ident ArrayIndexes Position   deriving (Eq, Show)

data FuncCall
  = FuncCall Ident [Expr]
  deriving (Eq, Show)

data Ident
  = Ident String Info
  | Self Info
  deriving (Show, Eq)

data MemberAccess
  = MemList Instance [Member] Position
  deriving (Eq, Show)

data Instance
  = VarObj Ident Position
  | FuncReturnsObj FuncCall Position
  deriving (Eq, Show)

data Member
  = FieldAccess Ident Position
  | MethodCall FuncCall Position
  deriving (Eq, Show)

data Field
  = Field {
    fieldType  :: Type,
    fieldIdent :: Ident,
    fieldPos   :: Position
  }
  deriving (Eq, Show)

data Constructor
  = Constructor {
      constructorParams :: ParamList,
      constructorBody   :: Stat,
      constructorPos    :: Position
  }
  deriving (Eq, Show)

data Class
  = Class Ident [Field] Constructor [Func] Position
  deriving (Eq, Show)

data ScopeErrorType
  = NoError
  | Duplicate
  | NotInScope
  | SelfNotInClass
  | ClassNotInScope String
  | CyclicConstr String
  deriving (Eq, Show)

data Info
  = Info InfoType Type Context
  | ClassInfo String [Ident] [Type]
  | ScopeError ScopeErrorType
  | NoInfo
  deriving (Eq, Show)

data InfoType
  = Instance
  | Static
  deriving (Eq, Show)

data Context
  = Function
  | Variable
  | ClassName
  deriving (Eq, Ord, Show)

data Stat
  = Skip Position
  | Declaration Type Ident AssignRHS Position
  | Assignment AssignLHS AssignRHS Position
  | Read AssignLHS Position
  | Free Expr Position
  | Return Expr Position
  | ReturnVoid Position
  | Exit Expr Position
  | Print Expr Position
  | Println Expr Position
  | If Expr Stat Stat Position
  | While Expr Stat Position
  | For Stat Expr Stat Stat Position
  | Block Stat Position
  | Break Position
  | Continue Position
  | CallFunc FuncCall Position
  | CallMethod MemberAccess Position
  | Seq Stat Stat Position
  deriving (Eq, Show)

data AssignLHS
  = Var Ident Position
  | ArrayDeref ArrayElem Position
  | PairDeref PairElem Position
  | MemberDeref MemberAccess Position
  deriving (Eq, Show)

data AssignRHS
  = ExprAssign Expr Position
  | ArrayLitAssign [Expr] Position
  | NewPairAssign  Expr Expr Position
  | PairElemAssign PairElem Position
  | FuncCallAssign FuncCall Position
  | ConstructAssign FuncCall Position
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
  = Void
  | BaseT BaseType
  | PairT Type Type
  | ArrayT Int Type
  | ClassT String
  | FuncT Type [Type]
  | Pair
  | NoType
  | PolyArray
  | PolyFunc
  | PolyPair
  | RelationalT
  | DataType
  | NotInScopeT String
  deriving (Show)

data Expr
  = StringLit String Position
  | CharLit Char Position
  | IntLit Int Position
  | BoolLit Bool Position
  | PairLiteral Position
  | ExprMemberAccess MemberAccess Position
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
  Void      == Void      = True
  ClassT i  == ClassT i' = i == i'
  _ == _ = False

{- HELPER FUNCTIONS -}

class Pretty a where
  pretty :: a -> String

-- POST: Shows and indents a string
showAndIndent :: Pretty a => a -> String
showAndIndent
  = indent . pretty

-- POST: Indents the given string by 4 spaces and returns the result as a
--       string
indent :: String -> String
indent s
  = unlines indentedLines
  where
    indentedLines = map ("  " ++) (lines s)

-- POST:    Converts a list in to a string, and seperates the elements in the
--          list with a comma
-- EXAMPLE: listToString "[" [1,2,3] "]" will return "[1,2,3]"
listToString :: Pretty a => String -> [a] -> String -> String
listToString open xs close
  = open ++ intercalate ", " (map pretty xs) ++ close

-- PRE:  (key, value) list is not empty and contains the association you are
--       looking for
-- POST: Performs a reverse lookup of a (key,value) list returning the key as
--       the result and taking the value as the input
flippedLookup :: Eq b => b -> [(a, b)] -> a
flippedLookup y xs
  = head [ x | (x, y') <- xs, y == y' ]

-- POST: Generates a string represenation of a list of functions
manyPretty :: Pretty a => [a] -> String
manyPretty
  = concatMap (flip (++) "\n" . pretty)

-- POST: Constructs a string representation of a dimension of an array
constructArray :: Int -> String
constructArray 0
  = ""
constructArray 1
  = "[]"
constructArray n
  = constructArray (n - 1) ++ "[]"

-- POST: Constant which represents the minimum precedence level
minPrecedence :: Int
minPrecedence
  = 0

-- POST: Returns a given string in brackets
inBrackets :: String -> String
inBrackets s
  = "(" ++ s ++ ")"

-- POST: Removes superfluous brackets
showWithoutBrackets :: (ExpressionTerm a, ExpressionTerm b) => a -> b -> String
showWithoutBrackets t t'
  = let showT' = pretty t' in
      if precedence t < precedence t'
        then inBrackets showT'
        else showT'

{- SHOW INSTANCES -}

instance Pretty Program where
  pretty (Program classes funcs body)
    = "begin\n" ++ indent (
          manyPretty classes  ++ "\n" ++
          manyPretty funcs    ++
          pretty body) ++
      "end"

instance Pretty Class where
  pretty (Class ident fields constructor methods pos)
    = "class " ++ pretty ident ++ " is\n\n" ++ indent (
        manyPretty fields  ++ "\n" ++
        pretty constructor ++ "\n" ++
        manyPretty methods) ++
      "end"


instance Pretty Field where
  pretty (Field t ident _)
    = pretty t ++ " " ++ pretty ident ++ ";"

instance Pretty Constructor where
  pretty (Constructor params body _)
    = "init" ++ pretty params ++ " is\n" ++ showAndIndent body ++ "end\n"

instance Pretty Func where
  pretty (Func t ident params body _)
    = pretty t ++ " " ++ pretty ident ++ pretty params ++ " is\n" ++
      showAndIndent body ++  "end\n"

instance Pretty ParamList where
  pretty (ParamList list _)
    = listToString "(" list ")"

instance Pretty Param where
  pretty (Param t ident _)
    = pretty t ++ " " ++ pretty ident

instance Pretty UnOp where
  pretty unOp
    = flippedLookup unOp (unOpPrec2 ++ unOpPrec1)

instance Pretty BinOp where
  pretty binOp
    = flippedLookup binOp (binOpPrec1 ++ binOpPrec2 ++ binOpPrec3
                           ++ binOpPrec4 ++ binOpPrec5 ++ binOpPrec6)

instance Pretty ArrayElem where
  pretty (ArrayElem ident elems _)
    = pretty ident ++ concatMap (\x ->  "[" ++ pretty x ++ "]") elems

instance Pretty Ident where
  pretty (Ident name info)
    = name ++ "\x1b[32m" ++ " ( " ++ show info ++ ")" ++ "\x1b[0m"
  pretty (Self info)
    = "self" ++ "\x1b[32m" ++ " ( " ++ show info ++ ")" ++ "\x1b[0m"

instance Pretty MemberAccess where
  pretty (MemList inst ms _)
    = pretty inst ++ "." ++ intercalate "." (map pretty ms)

instance Pretty Instance where
  pretty (VarObj ident _)
    = pretty ident
  pretty (FuncReturnsObj fc _)
    = pretty fc

instance Pretty FuncCall where
  pretty (FuncCall ident args)
    = pretty ident ++ listToString "(" args ")"

instance Pretty Member where
  pretty (FieldAccess ident _)
    = pretty ident
  pretty (MethodCall fc _)
    = pretty fc

instance Pretty Stat where
  pretty (Skip _)
    = "skip"
  pretty (Declaration typ ident rhs _)
   = pretty typ ++ " " ++ pretty ident ++ " = " ++ pretty rhs
  pretty (Assignment lhs rhs _)
    = pretty lhs ++ " = " ++ pretty rhs
  pretty (Read lhs _)
    = "read " ++ pretty lhs
  pretty (Free expr _)
    = "free " ++ pretty expr
  pretty (Return expr _)
    = "return " ++ pretty expr
  pretty (Exit expr _)
    = "exit " ++ pretty expr
  pretty (Print expr _)
    = "print " ++ pretty expr
  pretty (Println expr _)
    = "println " ++ pretty expr
  pretty (If cond stat stat' _)
    = "if" ++ " (" ++ pretty cond ++ ") " ++ "then\n" ++ showAndIndent stat ++
        "else\n" ++ showAndIndent stat' ++ "fi"
  pretty (While cond body _)
    = "while (" ++ pretty cond ++ ") " ++ "do\n" ++ showAndIndent body ++ "done"
  pretty (Block stat _)
    = "begin\n" ++ showAndIndent stat ++ "end"
  pretty (Seq stat stat' _)
    = pretty stat ++ ";\n" ++ pretty stat'
  pretty (Break _)
    = "break"
  pretty (Continue _)
    = "continue"
  pretty (CallFunc fc pos)
    = "call " ++ pretty fc
  pretty (CallMethod member pos)
    = pretty member

instance Pretty AssignLHS where
  pretty (Var ident _)
    = pretty ident
  pretty (ArrayDeref arrayElem _)
    = pretty arrayElem
  pretty (PairDeref pairElem _)
    = pretty pairElem
  pretty (MemberDeref member _)
    = pretty member

instance Pretty AssignRHS where
  pretty (ExprAssign e _)
    = pretty e
  pretty (ArrayLitAssign elems _)
    = listToString "[" elems "]"
  pretty (NewPairAssign e e' _)
    = "newpair" ++ listToString "(" [e, e'] ")"
  pretty (FuncCallAssign fc _)
    = "call " ++ pretty fc
  pretty (PairElemAssign pair _)
    = pretty pair
  pretty (ConstructAssign fc _)
    = "new " ++ pretty fc

instance Pretty PairElem where
  pretty (PairElem selector expr _)
    = pretty selector ++ " " ++ pretty expr

instance Pretty PairElemSelector where
  pretty Fst
    = "fst"
  pretty Snd
    = "snd"

instance Pretty ScopeErrorType where
  pretty NoError
    = "No Error with thsi variable"
  pretty Duplicate
    = "Duplicate"
  pretty NotInScope
    = "Not in scope"

instance Pretty Type where
  pretty (BaseT baseType)
    = pretty baseType
  pretty (ArrayT dim arrayType)
    = pretty arrayType ++ constructArray dim
  pretty (PairT t t')
    = "pair(" ++ pretty t ++ ", " ++ pretty t' ++ ")"
  pretty Pair
    = "null"
  pretty Void
    = "void"
  pretty (ClassT name)
    = name
  pretty (FuncT t paramTypes)
    =  pretty t
  pretty NoType
    = "No Type"
  pretty PolyArray
    = "Array of any type"
  pretty PolyFunc
    = "Function of any type"
  pretty PolyPair
    = "Pair of any type"
  pretty RelationalT
    = "char or int"
  pretty DataType
    = "int or string or char or array or pair"

instance Pretty BaseType where
  pretty BaseInt
    = "int"
  pretty BaseBool
    = "bool"
  pretty BaseChar
    = "char"
  pretty BaseString
    = "string"

class Pretty a => ExpressionTerm a where
  precedence :: a -> Int

instance ExpressionTerm Expr where
  precedence (UnaryApp unOp _ _)
    = precedence unOp
  precedence (BinaryApp binOp _ _ _)
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
  precedence (Arith Mul)
    = 2
  precedence (Arith Div)
    = 2
  precedence (Arith Mod)
    = 2
  precedence (Arith Add)
    = 4
  precedence (Arith Sub)
    = 4
  precedence (RelOp LT)
    = 6
  precedence (RelOp LTE)
    = 6
  precedence (RelOp GTE)
    = 6
  precedence (RelOp GT)
    = 6
  precedence (EquOp EQ)
    = 8
  precedence (EquOp NEQ)
    = 8
  precedence (Logic AND)
    = 10
  precedence (Logic OR)
    = 10

instance Pretty Expr where
  pretty (StringLit s _)
    = show s
  pretty (CharLit c _)
    = "\'" ++ [c] ++ "\'"
  pretty (IntLit x _)
    = show x
  pretty (BoolLit b _)
    = map toLower (show b)
  pretty (PairLiteral _)
    = "null"
  pretty (ExprMemberAccess mem _)
    = pretty mem
  pretty (IdentE ident _)
    = pretty ident
  pretty (ExprArray arrayElem _)
    = pretty arrayElem

  pretty (UnaryApp unOp expr _)
    = unOpString ++ showWithoutBrackets unOp expr
    where
      unOpString = pretty unOp ++ " "

  pretty (BinaryApp binOp expr expr' _)
    = showExpr ++ " " ++ pretty binOp ++ " " ++ showExpr'
    where
      showExpr  = showWithoutBrackets binOp expr
      showExpr' = showWithoutBrackets binOp expr'
