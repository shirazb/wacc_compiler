--- DATA DEFINITION FOR WACC HASKELL COMPILER PROJECT --

-- DEFINITIONS FOR PARSER
data ParseState = P Pos String
type Pos = (Row,Column)
type Row = Int
type Column = Int
newtype Parser a = Parser {parse :: String -> [(a,String)]}

-- WACC SYNTAX -- ABSTRACT SYNTAX TREE
data Program = Program [Func] Stat
data Ident   = Ident String
data Func    = Func Type Ident ParamList Stat
data ParamList = ParamList [Param]
data Param   = Param Type Ident
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
data AssignLHS = Var Ident | ArrayDeref ArrayElem | PairDeref PairElem
data AssignRHS
  = Expression Expr
  | NewArray ArrayLit
  | NewPair PairLit Expr Expr
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
            | ExprArray Ident [Expr]
            | UnaryOp UnOp Expr
            | BinaryOp BinOp Expr Expr
            | Expr Expr

-- data Expr    = LiteralExpr Literal | UnExpr UnOp Expr | BinExpr BinOp Expr Expr  deriving (Show, Eq)
-- data Literal = StringLit String | CharLit Char | IntLit Int | BoolLit Bool | ArrayLit [Expr]  deriving (Show, Eq)
data UnOp    = Exclatation | Neg | Len | Ord | Chr  deriving (Show, Eq)
data BinOp   = ArBinOp ArOp | BoolBinOp BoolOp      deriving (Show, Eq)
data ArOp    = Mul | Div | Mod | Add | Sub          deriving (Show, Eq)
data BoolOp  = AND | OR | LT | LTE | EQ | GTE | GT  deriving (Show, Eq)
